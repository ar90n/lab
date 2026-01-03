#include <Arduino.h>
#include <WiFi.h>
#include <SPI.h>
#include <SD.h>
#include <functional>

#include "secrets.h"

#ifndef ENABLE_CS_SENSE
#define ENABLE_CS_SENSE 0
#endif

// ===== Pin assign =====
static constexpr int PIN_SCK = D8;
static constexpr int PIN_MISO = D9;
static constexpr int PIN_MOSI = D10;
static constexpr int PIN_SD_CS = D2;
static constexpr int PIN_CS_SENSE = D3;
static constexpr int PIN_REQUEST = D4;
static constexpr int PIN_HEARTBEAT = D5;
static constexpr int PIN_DEBUG = D6;
static constexpr int PIN_SD_VDD = D7;

// ===== Timing constants =====
static constexpr uint32_t SPI_BLOCKOUT_PERIOD_MS = 20000;
static constexpr uint32_t HEARTBEAT_TICK_MS = 500;

// ===== HTTP =====
static constexpr uint16_t HTTP_PORT = 80;
static WiFiServer g_server(HTTP_PORT);

static const char *kDstPath = "/model.gcode";

static volatile TickType_t g_blockout_until_tick = 0;
static volatile TickType_t g_heartbeat_until_tick = 0;
static volatile bool g_we_have_bus = false;
static bool g_sd_ready = false;
static bool g_heartbeat_state = false;

class Defer
{
public:
  explicit Defer(std::function<void()> f) : m_f(std::move(f)) {}
  ~Defer()
  {
    if (m_f)
      m_f();
  }

  Defer(const Defer &) = delete;
  Defer &operator=(const Defer &) = delete;

  Defer(Defer &&other) noexcept : m_f(std::move(other.m_f)) {}
  Defer &operator=(Defer &&other) noexcept
  {
    if (this != &other)
      m_f = std::move(other.m_f);
    return *this;
  }

private:
  std::function<void()> m_f;
};

static inline TickType_t nowTick()
{
  return xTaskGetTickCount();
}

static inline bool printerCsLowNow()
{
#if ENABLE_CS_SENSE
  return digitalRead(PIN_CS_SENSE) == LOW;
#else
  return false;
#endif
}

static inline bool printerRecentlyUsedBus()
{
#if ENABLE_CS_SENSE
  return nowTick() < g_blockout_until_tick;
#else
  return false;
#endif
}

static void IRAM_ATTR onPrinterCsFalling()
{
  if (!g_we_have_bus)
  {
    g_blockout_until_tick = xTaskGetTickCountFromISR() + pdMS_TO_TICKS(SPI_BLOCKOUT_PERIOD_MS);
  }
}

static inline void sdVddOn()
{
  digitalWrite(PIN_SD_VDD, LOW);
}

static inline void sdVddOff()
{
  digitalWrite(PIN_SD_VDD, HIGH);
}

static inline void handleHeartbeat()
{
  TickType_t now = nowTick();
  if (now < g_heartbeat_until_tick)
  {
    return;
  }

  g_heartbeat_until_tick = now + pdMS_TO_TICKS(HEARTBEAT_TICK_MS);
  g_heartbeat_state = !g_heartbeat_state;
  digitalWrite(PIN_HEARTBEAT, g_heartbeat_state ? HIGH : LOW);
}

static void spiPinsHiZ()
{
  pinMode(PIN_SCK, INPUT);
  pinMode(PIN_MOSI, INPUT);
  pinMode(PIN_MISO, INPUT);
  pinMode(PIN_SD_CS, INPUT);
}

static void sdVddPowerCycle(uint32_t off_ms = 1000, uint32_t on_ms = 500)
{
  spiPinsHiZ();
  sdVddOff();
  delay(off_ms);
  sdVddOn();
  delay(on_ms);
  g_sd_ready = false;
}

static void takeBusControl()
{
  g_we_have_bus = true;

  // Enable SD VDD
#ifdef ENABLE_SD_VDD
  sdVddOn();
  delay(2);
#endif

  // CS high
  pinMode(PIN_SD_CS, OUTPUT);
  digitalWrite(PIN_SD_CS, HIGH);

  // Enable  SPI pins
  pinMode(PIN_SCK, OUTPUT);
  pinMode(PIN_MOSI, OUTPUT);
  pinMode(PIN_MISO, INPUT);

  SPI.begin(PIN_SCK, PIN_MISO, PIN_MOSI, PIN_SD_CS);
}

static void relinquishBusControl()
{
  SPI.end();
  spiPinsHiZ();
  g_we_have_bus = false;
}

static void restoreForPrinter()
{
  relinquishBusControl();
#ifdef ENABLE_SD_VDD
  sdVddPowerCycle();
#endif
}

static bool ensureSdReady()
{
  if (g_sd_ready)
  {
    return true;
  }

  if (!SD.begin(PIN_SD_CS, SPI, 20000000))
  {
    return false;
  }

  g_sd_ready = true;
  return true;
}

static String readLine(WiFiClient &c)
{
  String s = c.readStringUntil('\n');
  if (s.endsWith("\r"))
    s.remove(s.length() - 1);
  return s;
}

static void sendRespAndClose(WiFiClient &c, int code, const char *reason, const String &body)
{
  c.printf("HTTP/1.1 %d %s\r\n", code, reason);
  c.print("Connection: close\r\n");
  c.print("Content-Type: text/plain\r\n");
  c.printf("Content-Length: %u\r\n", (unsigned)body.length());
  c.print("\r\n");
  c.print(body);
  c.stop();
}

struct HttpReq
{
  String method;
  String path;
  long contentLength = -1;
  bool chunked = false;
};

static bool parseRequest(WiFiClient &c, HttpReq &out)
{
  String req = readLine(c);
  if (req.length() == 0)
  {
    return false;
  }

  int sp1 = req.indexOf(' ');
  int sp2 = (sp1 >= 0) ? req.indexOf(' ', sp1 + 1) : -1;
  if (sp1 < 0 || sp2 < 0)
  {
    return false;
  }

  out.method = req.substring(0, sp1);
  out.path = req.substring(sp1 + 1, sp2);

  while (true)
  {
    String h = readLine(c);
    if (h.length() == 0)
    {
      break;
    }

    int colon = h.indexOf(':');
    if (colon < 0)
    {
      continue;
    }

    String key = h.substring(0, colon);
    String val = h.substring(colon + 1);
    val.trim();

    if (key.equalsIgnoreCase("Content-Length"))
    {
      out.contentLength = val.toInt();
    }
    if (key.equalsIgnoreCase("Transfer-Encoding") &&
        (val.indexOf("chunked") >= 0 || val.indexOf("Chunked") >= 0))
    {
      out.chunked = true;
    }
  }
  return true;
}

static bool isPutOrPost(const String &m)
{
  return m.equalsIgnoreCase("PUT") || m.equalsIgnoreCase("POST");
}

static bool writeBodyToSdOverwrite(WiFiClient &c, long contentLength)
{
  SD.remove(kDstPath);
  File f = SD.open(kDstPath, FILE_WRITE);
  if (!f)
  {
    return false;
  }
  Defer defer_close([&]
                    { f.close(); });

  static uint8_t buf[1024];
  long remaining = contentLength;

  while (remaining > 0)
  {
    if (printerCsLowNow())
    {
      SD.remove(kDstPath);
      c.stop();
      return false;
    }

    size_t want = (remaining > (long)sizeof(buf)) ? sizeof(buf) : (size_t)remaining;
    size_t n = c.readBytes(buf, want);
    if (n == 0)
    {
      SD.remove(kDstPath);
      return false;
    }
    if (f.write(buf, n) != n)
    {
      SD.remove(kDstPath);
      return false;
    }
    remaining -= (long)n;
  }

  f.flush();
  return true;
}

static void handleHttpClient(WiFiClient &c)
{
  c.setTimeout(30);

  HttpReq r;
  if (!parseRequest(c, r))
  {
    sendRespAndClose(c, 400, "Bad Request", "bad request\n");
    return;
  }

  if (!isPutOrPost(r.method))
  {
    sendRespAndClose(c, 405, "Method Not Allowed", "use PUT/POST\n");
    return;
  }
  if (r.path != "/upload")
  {
    sendRespAndClose(c, 404, "Not Found", "use /upload\n");
    return;
  }
  if (r.chunked)
  {
    sendRespAndClose(c, 501, "Not Implemented", "chunked not supported\n");
    return;
  }
  if (r.contentLength <= 0)
  {
    sendRespAndClose(c, 411, "Length Required", "Content-Length required\n");
    return;
  }

  if (printerRecentlyUsedBus() || printerCsLowNow())
  {
    sendRespAndClose(c, 423, "Locked", "printer is using SD\n");
    return;
  }

  bool ok = false;
  {
    takeBusControl();
    Defer defer_restore([&]
                        { restoreForPrinter(); });

    if (!ensureSdReady())
    {
      sendRespAndClose(c, 500, "Internal Server Error", "SD.begin failed\n");
      return;
    }

    ok = writeBodyToSdOverwrite(c, r.contentLength);
  }

  if (!c.connected())
  {
    return;
  }

  if (!ok)
  {
    sendRespAndClose(c, 500, "Internal Server Error", "write failed\n");
    return;
  }

  sendRespAndClose(c, 200, "OK", String("OK saved ") + kDstPath + "\n");
}

static void setupSdVdd()
{
  pinMode(PIN_SD_VDD, OUTPUT);
#ifdef ENABLE_SD_VDD
  sdVddOn();
#else
  sdVddOff();
#endif
}

static void setupCsSense()
{
  pinMode(PIN_CS_SENSE, INPUT_PULLUP);
#if ENABLE_CS_SENSE
  attachInterrupt(digitalPinToInterrupt(PIN_CS_SENSE), onPrinterCsFalling, FALLING);
#endif
}

static void setupLEDs()
{
  pinMode(PIN_REQUEST, OUTPUT);
  pinMode(PIN_HEARTBEAT, OUTPUT);
  pinMode(PIN_DEBUG, OUTPUT);

  digitalWrite(PIN_REQUEST, LOW);
  digitalWrite(PIN_HEARTBEAT, LOW);
  digitalWrite(PIN_DEBUG, LOW);
}

static void connectWifi()
{
  WiFi.mode(WIFI_STA);
  WiFi.begin(WIFI_SSID, WIFI_PASS);
  Serial.print("WiFi connecting");
  while (WiFi.status() != WL_CONNECTED)
  {
    delay(200);
    Serial.print(".");
  }
  Serial.println();
  Serial.print("IP: ");
  Serial.println(WiFi.localIP());
}

void setup()
{
  Serial.begin(115200);
  delay(1500);

  setupSdVdd();
  setupCsSense();
  setupLEDs();
  connectWifi();

#ifdef ENABLE_SD_VDD
  sdVddPowerCycle();
#endif

  g_server.begin();
  Serial.println("HTTP upload server ready: PUT/POST /upload");
}

void loop()
{
  handleHeartbeat();

  WiFiClient c = g_server.available();
  if (!c)
  {
    delay(1);
    return;
  }
  handleHttpClient(c);
}

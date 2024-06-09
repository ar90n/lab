// derived from https://tamanegi.digick.jp/computer-embedded/mcuboa/zerotiny/

#include <Adafruit_NeoPixel.h>
#include <Arduino.h>
#include <cstdint>

#define LED_PIN 29

#define COLOR_REPEAT 2
#define N 57

// create a pixel strand with 1 pixel on PIN_NEOPIXEL
Adafruit_NeoPixel pixels(N, LED_PIN);

uint8_t color = 0, count = 0;
uint32_t colors[] = {pixels.Color(125, 0, 0), pixels.Color(0, 125, 0), pixels.Color(0, 0, 125), pixels.Color(125, 125, 125)};
const uint8_t COLORS_LEN = (uint8_t)(sizeof(colors) / sizeof(colors[0]));

void setup()
{
    pixels.begin(); // initialize the pixel
}

void loop()
{
    for(int i = 0; i < N; i++)
    {
        pixels.setPixelColor(i, colors[color]);
    }
    pixels.show();

    delay(1000);

    //pixels.clear();
    //pixels.show();

    //delay(1000);

    count++;

    if (count >= COLOR_REPEAT)
    {
        count = 0;
        color++;
        if (color >= COLORS_LEN)
        {
            color = 0;
        }
    }
}
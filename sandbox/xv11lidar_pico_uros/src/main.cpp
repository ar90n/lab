#include <optional>

#include <Arduino.h>
#include <xv11lidar.h>
#include <micro_ros_platformio.h>

#include <rcl/rcl.h>
#include <rclc/rclc.h>
#include <rclc/executor.h>

#include <std_msgs/msg/string.h>
#include <rosidl_runtime_c/string.h>
#include <rosidl_runtime_c/string_functions.h>

#include <std_msgs/msg/int32.h>
#include <sensor_msgs/msg/laser_scan.h>

#if !defined(MICRO_ROS_TRANSPORT_ARDUINO_SERIAL)
#error This example is only avaliable for Arduino framework with serial transport.
#endif

#define RCCHECK(fn)              \
  do                             \
  {                              \
    rcl_ret_t temp_rc = fn;      \
    if ((temp_rc != RCL_RET_OK)) \
    {                            \
      error_loop();              \
    }                            \
  } while (0)
#define RCSOFTCHECK(fn)          \
  do                             \
  {                              \
    rcl_ret_t temp_rc = fn;      \
    if ((temp_rc != RCL_RET_OK)) \
    {                            \
    }                            \
  } while (0)

namespace
{
  constexpr int PWM_PIN = 7;
  constexpr int RPM = 300;

  float distances[2][360];
  static uint32_t distances_gen[360];
  uint32_t distances_index = 0;
  uint32_t last_timestamp_us = 0;
  static int sec = 0;
  float duration = 0;

  xv11::ReturnType read_byte_from_serial()
  {
    if (Serial1.available() > 0)
    {
      return std::make_pair(true, static_cast<uint8_t>(Serial1.read()));
    }
    else
    {
      return std::make_pair(false, static_cast<uint8_t>(0));
    }
  }

  void write_pwm_value(float pwm)
  {
    analogWrite(PWM_PIN, pwm);
  }

  xv11::Lidar lidar(
      read_byte_from_serial,
      write_pwm_value,
      micros,
      RPM);

  rcl_publisher_t publisher;
  rclc_executor_t executor;
  rclc_support_t support;
  rcl_allocator_t allocator;
  rcl_node_t node;
  rcl_timer_t publish_timer;
  rcl_timer_t fetch_timer;

  void fetch_timer_callback(rcl_timer_t *timer, int64_t last_call_time)
  {
    RCLC_UNUSED(last_call_time);

    if (timer == NULL)
    {
      return;
    }

    xv11::DataPacket packet;
    while (lidar.process(&packet))
    {
      int const lidarAngle = packet.angle_quad;
      for (int i = 0; i < 4; i++)
      {
        auto const gen = distances_gen[4 * lidarAngle + i]++;
        distances[gen % 2][4 * lidarAngle + i] = (packet.distances[i] / 1000.0); // Distance to object in meters
      }
      duration = (packet.timestamp_us - last_timestamp_us) / 1000000.0;
      last_timestamp_us = packet.timestamp_us;
    }
  }

  sensor_msgs__msg__LaserScan msg;
  void publish_timer_callback(rcl_timer_t *timer, int64_t last_call_time)
  {
    RCLC_UNUSED(last_call_time);

    if (timer == NULL)
    {
      return;
    }

    static float data[360];
    rosidl_runtime_c__String__assign(&msg.header.frame_id, "lidar");
    msg.ranges.data = data;
    msg.ranges.capacity = 360;
    msg.ranges.size = 360;
    msg.header.stamp.sec = sec++;
    msg.angle_min = 3.14 / 1;
    msg.angle_max = -3.14 / 1;
    msg.angle_increment = -2 * 3.14 / 360;
    msg.time_increment = duration;
    msg.scan_time = msg.time_increment * 360;
    msg.range_min = 0.15;
    msg.range_max = 6.0;

    for (int i = 0; i < 360; i++)
    {
      msg.ranges.data[i] = distances[distances_index][i];
      distances[distances_index][i] = 0.0;
    }
    distances_index = (distances_index + 1) % 2;
    RCSOFTCHECK(rcl_publish(&publisher, &msg, NULL));
  }
}

// Error handle loop
void error_loop()
{
  while (1)
  {
    delay(100);
  }
}

void setup()
{
  // Configure lidar
  pinMode(PWM_PIN, OUTPUT);
  analogWrite(PWM_PIN, 100);

  // Configure serial transport
  Serial.begin(115200);
  Serial1.begin(115200, SERIAL_8N1);
  set_microros_serial_transports(Serial);
  delay(2000);

  allocator = rcl_get_default_allocator();

  // create init_options
  RCCHECK(rclc_support_init(&support, 0, NULL, &allocator));

  // create node
  RCCHECK(rclc_node_init_default(&node, "pico_micro_ros_practice_node", "", &support));

  // create publisher
  RCCHECK(rclc_publisher_init_default(
      &publisher,
      &node,
      ROSIDL_GET_MSG_TYPE_SUPPORT(sensor_msgs, msg, LaserScan),
      "pico_micro_ros_practice_node_publisher"));

  // create publish_timer,
  const unsigned int publish_timer_timeout = 60.0 / RPM * 1000 + 10;
  RCCHECK(rclc_timer_init_default(
      &publish_timer,
      &support,
      RCL_MS_TO_NS(publish_timer_timeout),
      publish_timer_callback));

  // create fetch_timer,
  const unsigned int fetch_timer_timeout = (60.0 / RPM * 1000) / 90 * 5;
  RCCHECK(rclc_timer_init_default(
      &fetch_timer,
      &support,
      RCL_MS_TO_NS(fetch_timer_timeout),
      fetch_timer_callback));

  // create executor
  RCCHECK(rclc_executor_init(&executor, &support.context, 2, &allocator));
  RCCHECK(rclc_executor_add_timer(&executor, &publish_timer));
  RCCHECK(rclc_executor_add_timer(&executor, &fetch_timer));
}

void loop()
{
  RCSOFTCHECK(rclc_executor_spin_some(&executor, RCL_MS_TO_NS(10)));
}
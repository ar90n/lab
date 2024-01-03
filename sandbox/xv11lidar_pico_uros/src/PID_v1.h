/**********************************************************************************************
 * PID Library forked from Arduino PID Library - Version 1.2.1
 * Original edition written by Brett Beauregard <br3ttb@gmail.com> brettbeauregard.com
 *
 * This Library is licensed under the MIT License
 **********************************************************************************************/

#ifndef PID_v1_h
#define PID_v1_h

#include <tuple>

using TimeStamp = unsigned long;
template <typename T>
T to_seconds(TimeStamp time)
{
  return static_cast<T>(time) / static_cast<T>(1000);
}

template <typename T>
class OutputRange
{
public:
  OutputRange(T min, T max) : min(min), max(max) {}
  ~OutputRange() = default;

  T clamp(T value) const
  {
    return std::max(min, std::min(value, max));
  }

  T min;
  T max;
};

template <typename T, TimeStamp SampleTimeMs>
class PID
{
public:
  using ReturnType = std::tuple<bool, T>;

  PID(T setpoint, T kp, T ki, T kd, T output_min, T output_max);
  PID(PID const &) = delete;
  ~PID() = default;

  PID operator=(const PID &) = delete;

  // performs the PID calculation.  it should be called every time loop() cycles.
  // the first value of the returned tuple is true when the output is computed, false otherwise
  ReturnType compute(T input, TimeStamp now);

  T const setpoint;
  T const kp; // * (P)roportional Tuning Parameter
  T const ki; // * (I)ntegral Tuning Parameter
  T const kd; // * (D)erivative Tuning Parameter
  OutputRange<T> const output_range;

private:
  TimeStamp lastTime;
  T output_sum;
  T last_input;
};

template <typename T, TimeStamp SampleTimeMs>
PID<T, SampleTimeMs>::PID(T setpoint, T kp, T ki, T kd, T output_min, T output_max)
    : setpoint(setpoint),
      kp(kp),
      ki(ki * to_seconds<T>(SampleTimeMs)),
      kd(kd / to_seconds<T>(SampleTimeMs)),
      output_range(output_min, output_max),
      lastTime(0),
      output_sum(0),
      last_input(0)
{
}

template <typename T, TimeStamp SampleTimeMs>
typename PID<T, SampleTimeMs>::ReturnType PID<T, SampleTimeMs>::compute(T input, TimeStamp now)
{
  TimeStamp const timeChange = now - lastTime;
  if (timeChange < SampleTimeMs)
  {
    return std::make_tuple(false, T{});
  }

  T const error = setpoint - input;
  T const dInput = (input - last_input);

  output_sum = output_range.clamp(output_sum + ki * error);
  T const output_raw = output_sum + kp * error - kd * dInput;
  T const output = output_range.clamp(output_raw);

  last_input = input;
  lastTime = now;
  return std::make_tuple(true, output);
}

#endif
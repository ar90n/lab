#include <Arduino.h>

void setup()
{
  // put your setup code here, to run once:
  Serial.begin(115200);
  pinMode(4, INPUT);
  pinMode(5, OUTPUT);
  pinMode(7, OUTPUT);
  pinMode(8, OUTPUT);
  digitalWrite(5, LOW);
  digitalWrite(7, HIGH);
  digitalWrite(8, HIGH);
}

bool is_drive = false;
uint8_t pwm_reg_value = 0;

void loop()
{
  if (Serial.available())
  {
    int incomingByte = Serial.read();
    switch (incomingByte)
    {
    case 'q':
    {
      is_drive = !is_drive;
    } break;
    case 'u':
    {
      pwm_reg_value += 1;
    } break;
    case 'd':
    {
      pwm_reg_value -= 1;
    } break;
    }
    analogWrite(6, pwm_reg_value * uint8_t(is_drive));
    Serial.print(pwm_reg_value);
    Serial.print(" ");
    Serial.print(is_drive);
    Serial.print(" ");
    Serial.print(pwm_reg_value * uint8_t(is_drive));
    Serial.print("\n");
    Serial.print("> ");
  }
}
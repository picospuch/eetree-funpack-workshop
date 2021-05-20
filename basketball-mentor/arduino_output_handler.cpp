/* Copyright 2019 The TensorFlow Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
==============================================================================*/

#if defined(ARDUINO) && !defined(ARDUINO_ARDUINO_NANO33BLE)
#define ARDUINO_EXCLUDE_CODE
#endif  // defined(ARDUINO) && !defined(ARDUINO_ARDUINO_NANO33BLE)

#ifndef ARDUINO_EXCLUDE_CODE

#include "output_handler.h"

#include "Arduino.h"
#include "HardwareBLESerial.h"

#define high "有点高了"
#define low "有点低了"
#define good "应该会投进"
#define fail "三不沾"

void HandleOutput(tflite::ErrorReporter* error_reporter, int kind) {
  // The first time this method runs, set up our LED
  static bool is_initialized = false;
  static HardwareBLESerial &bleSerial = HardwareBLESerial::getInstance();
  
  if (!is_initialized) {
    pinMode(LED_BUILTIN, OUTPUT);
    is_initialized = true;
  }

  if (kind == 0) {
    digitalWrite(LED_BUILTIN, LOW);
    TF_LITE_REPORT_ERROR(
        error_reporter,
        high);
    bleSerial.println(high);
  } else if (kind == 1) {
    digitalWrite(LED_BUILTIN, LOW);
    TF_LITE_REPORT_ERROR(
        error_reporter,
        low);
    bleSerial.println(low);
  } else if (kind == 2) {
    digitalWrite(LED_BUILTIN, HIGH);
    TF_LITE_REPORT_ERROR(
        error_reporter,
        good);
    bleSerial.println(good);
  } else if (kind == 3) {
    digitalWrite(LED_BUILTIN, LOW);
    TF_LITE_REPORT_ERROR(
        error_reporter,
        fail);
    bleSerial.println(fail);
  }
} 

#endif  // ARDUINO_EXCLUDE_CODE

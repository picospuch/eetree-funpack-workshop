#ifndef MAIN_H_
#define MAIN_H_

/* Freedom metal includes. */
#include <metal/machine.h>
#include <metal/machine/platform.h>

#include <metal/gpio.h>

/* console.  */
#define USE_TASK_EPRINTF static char _logbuf[100];
#define USE_UNIT_EPRINTF static char _logbuf[100];
#define log() write(STDOUT_FILENO, _logbuf, strlen(_logbuf));
#define eprintf(...) sprintf(_logbuf, __VA_ARGS__); log()

/* arduino right1 pin name to gpio number */
#define ARDUINO_SCL 13
#define ARDUINO_SDA 12
#define ARDUINO_AREF
//#define ARDUINO_GND
#define ARDUINO_SPI_SCK 5 // D13
#define ARDUINO_SPI_MISO 4 // D12
#define ARDUINO_SPI_MOSI 3 // D11
#define ARDUINO_SPI_CS 2 // D10
#define ARDUINO_D9 1 // D9
#define ARDUINO_D8 0 // D8
#define ARDUINO_D6 22 // D6

extern struct metal_gpio *gpio;

#endif // MAIN_H_


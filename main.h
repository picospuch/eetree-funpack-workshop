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

#define ARDUINO_SCL 13
#define ARDUINO_SDA 12

extern struct metal_gpio *gpio;

#endif

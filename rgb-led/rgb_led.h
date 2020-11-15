#ifndef RGB_LED_H_
#define RGB_LED_H_

#include "main.h"

#include "hsb2rgb_led.h"

#define P9813_PIN_CIN_Clr()        metal_gpio_set_pin(gpio, ARDUINO_SCL, 0)
#define P9813_PIN_CIN_Set()        metal_gpio_set_pin(gpio, ARDUINO_SCL, 1)

#define P9813_PIN_DIN_Clr()        metal_gpio_set_pin(gpio, ARDUINO_SDA, 0)
#define P9813_PIN_DIN_Set()        metal_gpio_set_pin(gpio, ARDUINO_SDA, 1)

#define ext_rgb_led_log(M, ...) eprintf(M, ##__VA_ARGS__)

#define RGB_MODE 0
#define HSB_MODE 1

//-------------------- user interfaces ---------------------------

/**
 * @brief Initialize RGB LED device.
 *
 * @return none
 */
void rgb_led_init();


/**
 * @brief Set light parameters for RGB LED
 *
 * @param red:    Red light parameter
 * @param green:  Green light parameter
 * @param blue:   Blue light parameter
 *
 * @return none
 */
void rgb_led_open(uint8_t red, uint8_t green, uint8_t blue);


/**
 * @brief Close RGB LED
 *
 * @return none
 */
void rgb_led_close(void);

#endif // RGB_LED_H_

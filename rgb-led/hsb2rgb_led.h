#ifndef HSB2RGB_LED_H_
#define HSB2RGB_LED_H_

#include "main.h"

/**
 * @brief Initialize hsb2rgb device.
 *
 * @return none
 */
void hsb2rgb_led_init();


/**
 * @brief Set light parameters for hsb2rgb.
 *
 * @param hues:         hues data of hsb2rgb
 * @param saturation:   saturation data of hsb2rgb
 * @param brightness:   brightness data of hsb2rgb
 *
 * @return none
 */
void hsb2rgb_led_open(float hues, float saturation, float brightness);

/**
 * @brief Close hsb2rgb
 *
 * @return none
 */
void hsb2rgb_led_close(void);

#endif   // HSB2RGB_LED_H_

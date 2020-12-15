/* generated HAL header file - do not edit */
#ifndef HAL_DATA_H_
#define HAL_DATA_H_
#include <stdint.h>
#include "bsp_api.h"
#include "common_data.h"
#include "r_usb_basic.h"
#include "r_usb_basic_api.h"
#include "r_usb_pcdc_api.h"
#include "r_gpt.h"
#include "r_timer_api.h"
#include "r_icu.h"
#include "r_external_irq_api.h"
#include "r_adc.h"
#include "r_adc_api.h"
FSP_HEADER
/* Basic on USB Instance. */
extern const usb_instance_t g_basic0;

/** Access the USB instance using these structures when calling API functions directly (::p_api is not used). */
extern usb_instance_ctrl_t g_basic0_ctrl;
extern const usb_cfg_t g_basic0_cfg;

#ifndef NULL
void NULL(usb_event_info_t*, usb_hdl_t, usb_onoff_t);
#endif
/** CDC Driver on USB Instance. */
/** Timer on GPT Instance. */
extern const timer_instance_t g_blinker;

/** Access the GPT instance using these structures when calling API functions directly (::p_api is not used). */
extern gpt_instance_ctrl_t g_blinker_ctrl;
extern const timer_cfg_t g_blinker_cfg;

#ifndef gpt_blink_callback
void gpt_blink_callback(timer_callback_args_t *p_args);
#endif
/** External IRQ on ICU Instance. */
extern const external_irq_instance_t g_external_irq06;

/** Access the ICU instance using these structures when calling API functions directly (::p_api is not used). */
extern icu_instance_ctrl_t g_external_irq06_ctrl;
extern const external_irq_cfg_t g_external_irq06_cfg;

#ifndef button_irq06_callback
void button_irq06_callback(external_irq_callback_args_t *p_args);
#endif
/** Timer on GPT Instance. */
extern const timer_instance_t g_gpt_red;

/** Access the GPT instance using these structures when calling API functions directly (::p_api is not used). */
extern gpt_instance_ctrl_t g_gpt_red_ctrl;
extern const timer_cfg_t g_gpt_red_cfg;

#ifndef NULL
void NULL(timer_callback_args_t *p_args);
#endif
/** ADC on ADC Instance. */
extern const adc_instance_t g_adc;

/** Access the ADC instance using these structures when calling API functions directly (::p_api is not used). */
extern adc_instance_ctrl_t g_adc_ctrl;
extern const adc_cfg_t g_adc_cfg;
extern const adc_channel_cfg_t g_adc_channel_cfg;

#ifndef NULL
void NULL(adc_callback_args_t *p_args);
#endif
void hal_entry(void);
void g_hal_init(void);
FSP_FOOTER
#endif /* HAL_DATA_H_ */

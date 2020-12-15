/* generated HAL source file - do not edit */
#include "hal_data.h"

/* Macros to tie dynamic ELC links to ADC_TRIGGER_SYNC_ELC option in adc_trigger_t. */
#define ADC_TRIGGER_ADC0        ADC_TRIGGER_SYNC_ELC
#define ADC_TRIGGER_ADC0_B      ADC_TRIGGER_SYNC_ELC
#define ADC_TRIGGER_ADC1        ADC_TRIGGER_SYNC_ELC
#define ADC_TRIGGER_ADC1_B      ADC_TRIGGER_SYNC_ELC
usb_instance_ctrl_t g_basic0_ctrl;

#if !defined(usb_descriptor)
extern usb_descriptor_t usb_descriptor;
#endif
#define RA_NOT_DEFINED (1)
const usb_cfg_t g_basic0_cfg =
{ .usb_mode = USB_MODE_PERI, .usb_speed = USB_SPEED_FS, .module_number = 0, .type = USB_CLASS_PCDC,
#if defined(usb_descriptor)
                .p_usb_reg = usb_descriptor,
#else
  .p_usb_reg = &usb_descriptor,
#endif
  .usb_complience_cb = NULL,
#if defined(VECTOR_NUMBER_USBFS_INT)
                .irq       = VECTOR_NUMBER_USBFS_INT,
#else
  .irq = FSP_INVALID_VECTOR,
#endif
#if defined(VECTOR_NUMBER_USBFS_RESUME)
                .irq_r     = VECTOR_NUMBER_USBFS_RESUME,
#else
  .irq_r = FSP_INVALID_VECTOR,
#endif
  .irq_d0 = FSP_INVALID_VECTOR,
  .irq_d1 = FSP_INVALID_VECTOR, .hsirq = FSP_INVALID_VECTOR, .hsirq_d0 = FSP_INVALID_VECTOR, .hsirq_d1 =
          FSP_INVALID_VECTOR,
  .ipl = (2), .ipl_r = (2), .ipl_d0 = 15, .ipl_d1 = 15, .hsipl = 15, .hsipl_d0 = 15, .hsipl_d1 = 15,
#if (BSP_CFG_RTOS == 2)
                .p_usb_apl_callback = NULL,
#else
  .p_usb_apl_callback = NULL,
#endif
#if defined(NULL)
                .p_context = NULL,
#else
  .p_context = &NULL,
#endif
        };
#undef RA_NOT_DEFINED

/* Instance structure to use this module. */
const usb_instance_t g_basic0 =
{ .p_ctrl = &g_basic0_ctrl, .p_cfg = &g_basic0_cfg, .p_api = &g_usb_on_usb, };

gpt_instance_ctrl_t g_blinker_ctrl;
#if 0
const gpt_extended_pwm_cfg_t g_blinker_pwm_extend =
{
    .trough_ipl          = (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_GPT0_COUNTER_UNDERFLOW)
    .trough_irq          = VECTOR_NUMBER_GPT0_COUNTER_UNDERFLOW,
#else
    .trough_irq          = FSP_INVALID_VECTOR,
#endif
    .poeg_link           = GPT_POEG_LINK_POEG0,
    .output_disable      =  GPT_OUTPUT_DISABLE_NONE,
    .adc_trigger         =  GPT_ADC_TRIGGER_NONE,
    .dead_time_count_up  = 0,
    .dead_time_count_down = 0,
    .adc_a_compare_match = 0,
    .adc_b_compare_match = 0,
    .interrupt_skip_source = GPT_INTERRUPT_SKIP_SOURCE_NONE,
    .interrupt_skip_count  = GPT_INTERRUPT_SKIP_COUNT_0,
    .interrupt_skip_adc    = GPT_INTERRUPT_SKIP_ADC_NONE,
    .gtioca_disable_setting = GPT_GTIOC_DISABLE_PROHIBITED,
    .gtiocb_disable_setting = GPT_GTIOC_DISABLE_PROHIBITED,
};
#endif
const gpt_extended_cfg_t g_blinker_extend =
        { .gtioca =
        { .output_enabled = false, .stop_level = GPT_PIN_LEVEL_LOW },
          .gtiocb =
          { .output_enabled = false, .stop_level = GPT_PIN_LEVEL_LOW },
          .start_source = (gpt_source_t) (GPT_SOURCE_NONE), .stop_source = (gpt_source_t) (GPT_SOURCE_NONE), .clear_source =
                  (gpt_source_t) (GPT_SOURCE_NONE),
          .count_up_source = (gpt_source_t) (GPT_SOURCE_NONE), .count_down_source = (gpt_source_t) (GPT_SOURCE_NONE), .capture_a_source =
                  (gpt_source_t) (GPT_SOURCE_NONE),
          .capture_b_source = (gpt_source_t) (GPT_SOURCE_NONE), .capture_a_ipl = (BSP_IRQ_DISABLED), .capture_b_ipl =
                  (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_GPT0_CAPTURE_COMPARE_A)
    .capture_a_irq       = VECTOR_NUMBER_GPT0_CAPTURE_COMPARE_A,
#else
          .capture_a_irq = FSP_INVALID_VECTOR,
#endif
#if defined(VECTOR_NUMBER_GPT0_CAPTURE_COMPARE_B)
    .capture_b_irq       = VECTOR_NUMBER_GPT0_CAPTURE_COMPARE_B,
#else
          .capture_b_irq = FSP_INVALID_VECTOR,
#endif
          .capture_filter_gtioca = GPT_CAPTURE_FILTER_NONE,
          .capture_filter_gtiocb = GPT_CAPTURE_FILTER_NONE,
#if 0
    .p_pwm_cfg                   = &g_blinker_pwm_extend,
#else
          .p_pwm_cfg = NULL,
#endif
        };
const timer_cfg_t g_blinker_cfg =
{ .mode = TIMER_MODE_PERIODIC,
  /* Actual period: 2 seconds. Actual duty: 50%. */.period_counts = (uint32_t) 0x2dc6c00,
  .duty_cycle_counts = 0x16e3600,
  .source_div = (timer_source_div_t) 0,
  .channel = 0,
  .p_callback = gpt_blink_callback,
  .p_context = NULL,
  .p_extend = &g_blinker_extend,
  .cycle_end_ipl = (2),
#if defined(VECTOR_NUMBER_GPT0_COUNTER_OVERFLOW)
    .cycle_end_irq       = VECTOR_NUMBER_GPT0_COUNTER_OVERFLOW,
#else
  .cycle_end_irq = FSP_INVALID_VECTOR,
#endif
        };
/* Instance structure to use this module. */
const timer_instance_t g_blinker =
{ .p_ctrl = &g_blinker_ctrl, .p_cfg = &g_blinker_cfg, .p_api = &g_timer_on_gpt };
icu_instance_ctrl_t g_external_irq06_ctrl;
const external_irq_cfg_t g_external_irq06_cfg =
{ .channel = 6,
  .trigger = EXTERNAL_IRQ_TRIG_FALLING,
  .filter_enable = true,
  .pclk_div = EXTERNAL_IRQ_PCLK_DIV_BY_64,
  .p_callback = button_irq06_callback,
  .p_context = NULL,
  .p_extend = NULL,
  .ipl = (2),
#if defined(VECTOR_NUMBER_ICU_IRQ6)
    .irq                 = VECTOR_NUMBER_ICU_IRQ6,
#else
  .irq = FSP_INVALID_VECTOR,
#endif
        };
/* Instance structure to use this module. */
const external_irq_instance_t g_external_irq06 =
{ .p_ctrl = &g_external_irq06_ctrl, .p_cfg = &g_external_irq06_cfg, .p_api = &g_external_irq_on_icu };
gpt_instance_ctrl_t g_gpt_red_ctrl;
#if 0
const gpt_extended_pwm_cfg_t g_gpt_red_pwm_extend =
{
    .trough_ipl          = (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_GPT3_COUNTER_UNDERFLOW)
    .trough_irq          = VECTOR_NUMBER_GPT3_COUNTER_UNDERFLOW,
#else
    .trough_irq          = FSP_INVALID_VECTOR,
#endif
    .poeg_link           = GPT_POEG_LINK_POEG0,
    .output_disable      =  GPT_OUTPUT_DISABLE_NONE,
    .adc_trigger         =  GPT_ADC_TRIGGER_NONE,
    .dead_time_count_up  = 0,
    .dead_time_count_down = 0,
    .adc_a_compare_match = 0,
    .adc_b_compare_match = 0,
    .interrupt_skip_source = GPT_INTERRUPT_SKIP_SOURCE_NONE,
    .interrupt_skip_count  = GPT_INTERRUPT_SKIP_COUNT_0,
    .interrupt_skip_adc    = GPT_INTERRUPT_SKIP_ADC_NONE,
    .gtioca_disable_setting = GPT_GTIOC_DISABLE_PROHIBITED,
    .gtiocb_disable_setting = GPT_GTIOC_DISABLE_PROHIBITED,
};
#endif
const gpt_extended_cfg_t g_gpt_red_extend =
        { .gtioca =
        { .output_enabled = false, .stop_level = GPT_PIN_LEVEL_LOW },
          .gtiocb =
          { .output_enabled = true, .stop_level = GPT_PIN_LEVEL_LOW },
          .start_source = (gpt_source_t) (GPT_SOURCE_NONE), .stop_source = (gpt_source_t) (GPT_SOURCE_NONE), .clear_source =
                  (gpt_source_t) (GPT_SOURCE_NONE),
          .count_up_source = (gpt_source_t) (GPT_SOURCE_NONE), .count_down_source = (gpt_source_t) (GPT_SOURCE_NONE), .capture_a_source =
                  (gpt_source_t) (GPT_SOURCE_NONE),
          .capture_b_source = (gpt_source_t) (GPT_SOURCE_NONE), .capture_a_ipl = (BSP_IRQ_DISABLED), .capture_b_ipl =
                  (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_GPT3_CAPTURE_COMPARE_A)
    .capture_a_irq       = VECTOR_NUMBER_GPT3_CAPTURE_COMPARE_A,
#else
          .capture_a_irq = FSP_INVALID_VECTOR,
#endif
#if defined(VECTOR_NUMBER_GPT3_CAPTURE_COMPARE_B)
    .capture_b_irq       = VECTOR_NUMBER_GPT3_CAPTURE_COMPARE_B,
#else
          .capture_b_irq = FSP_INVALID_VECTOR,
#endif
          .capture_filter_gtioca = GPT_CAPTURE_FILTER_NONE,
          .capture_filter_gtiocb = GPT_CAPTURE_FILTER_NONE,
#if 0
    .p_pwm_cfg                   = &g_gpt_red_pwm_extend,
#else
          .p_pwm_cfg = NULL,
#endif
        };
const timer_cfg_t g_gpt_red_cfg =
{ .mode = TIMER_MODE_PWM,
  /* Actual period: 0.0004166666666666667 seconds. Actual duty: 50%. */.period_counts = (uint32_t) 0x2710,
  .duty_cycle_counts = 0x1388,
  .source_div = (timer_source_div_t) 0,
  .channel = 3,
  .p_callback = NULL,
  .p_context = NULL,
  .p_extend = &g_gpt_red_extend,
  .cycle_end_ipl = (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_GPT3_COUNTER_OVERFLOW)
    .cycle_end_irq       = VECTOR_NUMBER_GPT3_COUNTER_OVERFLOW,
#else
  .cycle_end_irq = FSP_INVALID_VECTOR,
#endif
        };
/* Instance structure to use this module. */
const timer_instance_t g_gpt_red =
{ .p_ctrl = &g_gpt_red_ctrl, .p_cfg = &g_gpt_red_cfg, .p_api = &g_timer_on_gpt };
adc_instance_ctrl_t g_adc_ctrl;
const adc_extended_cfg_t g_adc_cfg_extend =
{ .add_average_count = ADC_ADD_OFF,
  .clearing = ADC_CLEAR_AFTER_READ_ON,
  .trigger_group_b = ADC_TRIGGER_SYNC_ELC,
  .double_trigger_mode = ADC_DOUBLE_TRIGGER_DISABLED,
  .adc_vref_control = ADC_VREF_CONTROL_VREFH, };
const adc_cfg_t g_adc_cfg =
{ .unit = 0,
  .mode = ADC_MODE_SINGLE_SCAN,
  .resolution = ADC_RESOLUTION_16_BIT,
  .alignment = (adc_alignment_t) (0),
  .trigger = ADC_TRIGGER_SOFTWARE,
  .p_callback = NULL,
  .p_context = NULL,
  .p_extend = &g_adc_cfg_extend,
#if defined(VECTOR_NUMBER_ADC0_SCAN_END)
    .scan_end_irq        = VECTOR_NUMBER_ADC0_SCAN_END,
#else
  .scan_end_irq = FSP_INVALID_VECTOR,
#endif
  .scan_end_ipl = (BSP_IRQ_DISABLED),
#if defined(VECTOR_NUMBER_ADC0_SCAN_END_B)
    .scan_end_b_irq      = VECTOR_NUMBER_ADC0_SCAN_END_B,
#else
  .scan_end_b_irq = FSP_INVALID_VECTOR,
#endif
  .scan_end_b_ipl = (BSP_IRQ_DISABLED), };
const adc_channel_cfg_t g_adc_channel_cfg =
{ .scan_mask = ADC_MASK_CHANNEL_8 | ADC_MASK_TEMPERATURE | 0, .scan_mask_group_b = 0, .priority_group_a =
          ADC_GROUP_A_PRIORITY_OFF,
  .add_mask = 0, .sample_hold_mask = 0, .sample_hold_states = 24, };
/* Instance structure to use this module. */
const adc_instance_t g_adc =
{ .p_ctrl = &g_adc_ctrl, .p_cfg = &g_adc_cfg, .p_channel_cfg = &g_adc_channel_cfg, .p_api = &g_adc_on_adc };
void g_hal_init(void)
{
    g_common_init ();
}

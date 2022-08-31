#ifndef TINYML_H
#define TINYML_H

/* Include ----------------------------------------------------------------- */
#include <cstdint>

typedef void (*tinyml_result_callback) (const char *label, float value);

void tinyml_start_impulse(tinyml_result_callback result_callback, bool continuous, bool debug, bool use_max_uart_speed = false);
void tinyml_run_impulse(void);
void tinyml_stop_impulse(void);
bool tinyml_is_inference_running(void);

#endif /* TINYML_H */

/*
 * The Queue Send Task:
 * The queue send task is implemented by the prvQueueSendTask() function in
 * this file.  prvQueueSendTask() sits in a loop that causes it to repeatedly
 * block for 1000 milliseconds, before sending the value 100 to the queue that
 * was created within main().  Once the value is sent, the task loops
 * back around to block for another 1000 milliseconds...and so on.
 *
 * The Queue Receive Task:
 * The queue receive task is implemented by the prvQueueReceiveTask() function
 * in this file.  prvQueueReceiveTask() sits in a loop where it repeatedly
 * blocks on attempts to read data from the queue that was created within
 * blinky().  When data is received, the task checks the value of the
 * data, and if the value equals the expected 100, writes 'Blink' to the UART
 * (the UART is used in place of the LED to allow easy execution in QEMU).  The
 * 'block time' parameter passed to the queue receive function specifies that
 * the task should be held in the Blocked state indefinitely to wait for data to
 * be available on the queue.  The queue receive task will only leave the
 * Blocked state when the queue send task writes to the queue.  As the queue
 * send task writes to the queue every 1000 milliseconds, the queue receive
 * task leaves the Blocked state every 1000 milliseconds, and therefore toggles
 * the LED every 1 second.
 */

#include "main.h"

/* Standard includes. */
#include <stdio.h>
#include <string.h>
#include <unistd.h>

/* Kernel includes. */
#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"

/* Freedom metal includes. */
#include <metal/machine.h>
#include <metal/machine/platform.h>

#include <metal/lock.h>
#include <metal/uart.h>
#include <metal/interrupt.h>
#include <metal/clock.h>
#include <metal/led.h>
#include <metal/spi.h>
#include <metal/pwm.h>

#include "rgb-led/rgb_led.h"
#include "oled/oled.h"

#define TASK_PRIORITY		(tskIDLE_PRIORITY + 1)

/* The 1s value is converted to ticks using the pdMS_TO_TICKS() macro. */
#define mainQUEUE_TICK_COUNT_FOR_1S			pdMS_TO_TICKS( WAIT_MS )

static void prvSetupHardware( void );

/*
 * The tasks as described in the comments at the top of this file.
 */
static void prvBlinkTask( void *pvParameters );
static void prvOledTask( void *pvParameters );
static void prvBlink2Task( void *pvParameters );
/*-----------------------------------------------------------*/



struct metal_cpu *cpu0;
struct metal_interrupt *cpu_intr, *tmr_intr;
struct metal_led *led0_red, *led0_green, *led0_blue;

struct metal_gpio *gpio;
struct metal_spi *spi;
struct metal_spi_config *spi_config;
struct metal_pwm *pwm;

USE_UNIT_EPRINTF

int oled_setup_hardware(void) {
	/* Get SPI 1 */
	spi = metal_spi_get_device(1);

	if(spi == NULL) {
		eprintf("Failed to get spi device\n");
		return 1;
	}

	/* Initialize the SPI device to 100_000 baud */
	metal_spi_init(spi, 1000000);

	/* CPOL = 0, CPHA = 0, MSB-first, CS active low */
	static struct metal_spi_config spi_config0 = {
		.protocol = METAL_SPI_SINGLE,
		.polarity = 0,
		.phase = 0,
		.little_endian = 0,
		.cs_active_high = 0,
		.csid = 0,
	};
	
	spi_config = &spi_config0;

	metal_gpio_enable_output(gpio, ARDUINO_SPI_MISO);
	metal_gpio_disable_input(gpio, ARDUINO_SPI_MISO);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SPI_MISO);
	metal_gpio_set_pin(gpio, ARDUINO_SPI_MISO, 0);

	metal_gpio_enable_output(gpio, ARDUINO_SPI_CS);
	metal_gpio_disable_input(gpio, ARDUINO_SPI_CS);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SPI_CS);
	metal_gpio_set_pin(gpio, ARDUINO_SPI_CS, 0);

	/* Transfer three bytes */
	//char tx_buf[3] = {0x55, 0xAA, 0xA5};
	//char rx_buf[3] = {0};
	return 0;
}

void oled_dc_clear(void) {
	// use arduino miso
		metal_gpio_set_pin(gpio, ARDUINO_SPI_MISO, 0);
}
void oled_dc_set(void) {
	// use arduino miso
		metal_gpio_set_pin(gpio, ARDUINO_SPI_MISO, 1);
}
void oled_cs_clear(void) {
	// use arduino cs
		metal_gpio_set_pin(gpio, ARDUINO_SPI_CS, 0);

}
void oled_cs_set(void) {
	// user arduino cs
		metal_gpio_set_pin(gpio, ARDUINO_SPI_CS, 1);
}
void oled_rst_clear(void) {
	// not used
}
void oled_rst_set(void) {
	// not used
}
void oled_spi_transfer(uint8_t *dat, uint8_t len) {
	// use hardware spi1
	metal_spi_transfer(spi, spi_config, len, dat, NULL);
}

void delay_ms(u16 nms) {
	// a freertos tick is 10ms.
	vTaskDelay(nms / 10);
}

int do_oled(void) {
    uint32_t display_row_num[4] = {OLED_DISPLAY_ROW_1, OLED_DISPLAY_ROW_2,
    OLED_DISPLAY_ROW_3, OLED_DISPLAY_ROW_4};

    char oled_show_line[4][OLED_DISPLAY_MAX_CHAR_PER_ROW + 1] =
    { "hello", "eetree", "phase2", "funpack" };   // max char each line

    // init OLED
    OLED_Init();
    OLED_FillAll();
    OLED_Clear();

		//		OLED_ShowString(OLED_DISPLAY_COLUMN_START, display_row_num[0], oled_show_line[0]);
		
		int d[4] = {0, 1, 2, 3};
		int t = 0;
		while (1) {
			OLED_Clear();
			for (int i = 0; i < 4; ++i) {
				OLED_ShowString(OLED_DISPLAY_COLUMN_START, display_row_num[i], oled_show_line[d[i]]);
			}
			t = d[0]; d[0] = d[1]; d[1] = d[2]; d[2] = d[3]; d[3] = t;
			delay_ms(1000);
		}
}

/* light a rgb led.  */
int do_led( void )
{
	ext_rgb_led_log("rgb led conrtol demo(RGB_MODE)\r\n");
	eprintf("do_led\r\n");

#define COLOR_MODE HSB_MODE

#if (COLOR_MODE == HSB_MODE)
	while(1) {
    for (float i = 0; i < 10.0; i+=0.1) {
			hsb2rgb_led_open(120, 100, i);
			vTaskDelay(1);
		}
		for (float i = 10.0; i>=0; i-=0.1) {
			hsb2rgb_led_open(120, 100, i);
			vTaskDelay(1);
		}
  }
#else
  while(1) {
    /*open red led,#FF0000*/
    rgb_led_open(255, 0, 0);
    vTaskDelay(100);
    /*open green led #00FF00*/
    rgb_led_open(0, 255, 0);
		vTaskDelay(100);
    /*open blue led,#0000FF*/
    rgb_led_open(0, 0, 255);
		vTaskDelay(100);
  }
#endif
	
}

int main( void )
{
	eprintf("\r\n\r\n===FreeRTOS START===\r\n");
	eprintf("hz of tick: %d\r\n", configTICK_RATE_HZ);
	// @todo time slicing no effect
	eprintf("task type: %d, %d\r\n", configUSE_PREEMPTION, configUSE_TIME_SLICING);
	eprintf("\r\n");
	
	TaskHandle_t xHandle_BlinkTask, xHandle_OledTask, xHandle_Blink2Task;

	prvSetupHardware();

	xTaskCreate( prvBlinkTask,
							 "blink",
							 configMINIMAL_STACK_SIZE,
							 NULL,
							 TASK_PRIORITY,
							 &xHandle_BlinkTask);
	xTaskCreate( prvOledTask,
							 "oled",
							 configMINIMAL_STACK_SIZE,
							 NULL,
							 TASK_PRIORITY+1,
							 &xHandle_OledTask);
	xTaskCreate( prvBlink2Task,
							 "blink2",
							 configMINIMAL_STACK_SIZE,
							 NULL,
							 TASK_PRIORITY,
							 &xHandle_Blink2Task);

	/* Start the tasks and timer running. */
	vTaskStartScheduler();
	
	/**
	 * If all is well, the scheduler will now be running, and the following 
	 * line will never be reached.  If the following line does execute, then 
	 * there was insufficient FreeRTOS heap memory available for the Idle and/or 
	 * timer tasks to be created. or task have stoppped the Scheduler 
	 * 
	 */
	vTaskDelete( xHandle_BlinkTask );
	vTaskDelete( xHandle_OledTask );
	vTaskDelete( xHandle_Blink2Task );
	
	eprintf("\r\n\r\n===FreeRTOS END===\r\n\r\n");
}

static void prvBlink2Task( void *pvParameters ) {
	USE_TASK_EPRINTF
	eprintf("\r\n===in blink2 task===\r\n");

	// ARDUINO_D6, PWM1_3
	metal_pwm_enable(pwm);
		
	metal_pwm_set_freq(pwm, 3, 10000);

	/* Set Duty cycle and phase correct mode */
	metal_pwm_set_duty(pwm, 3, 90, METAL_PWM_PHASE_CORRECT_DISABLE);

	/* Start in continuous mode */
	metal_pwm_trigger(pwm, 3, METAL_PWM_CONTINUOUS);

	while (1) {
		for (int i = 0; i < 100; ++i) {
			metal_pwm_set_duty(pwm, 3, i, METAL_PWM_PHASE_CORRECT_DISABLE);
			delay_ms(10);
		}
		for (int i = 100; i > 0; --i) {
			metal_pwm_set_duty(pwm, 3, i, METAL_PWM_PHASE_CORRECT_DISABLE);
			delay_ms(10);
		}
	}
	
	for (;;) vTaskDelay(0);
}

static void prvBlinkTask( void *pvParameters ) {
	USE_TASK_EPRINTF
	eprintf("\r\n===in blink task===\r\n");

	do_led();

	for (;;) vTaskDelay(0);
}

static void prvOledTask( void *pvParameters ) {
  USE_TASK_EPRINTF
	eprintf("\r\n===in oled task===\r\n");

	do_oled();
	// do not let it run into metal_shutdown!
	// no tick interrupt will be triggered after that!
	for (;;) vTaskDelay(0);
}

static void prvSetupHardware( void ) {
	gpio = metal_gpio_get_device(0);

	oled_setup_hardware();
	
	metal_gpio_enable_output(gpio, ARDUINO_SCL); // SCL
	metal_gpio_disable_input(gpio, ARDUINO_SCL);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SCL);
	metal_gpio_set_pin(gpio, ARDUINO_SCL, 0);
	
	metal_gpio_enable_output(gpio, ARDUINO_SDA); // SDA
	metal_gpio_disable_input(gpio, ARDUINO_SDA);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SDA);
	metal_gpio_set_pin(gpio, ARDUINO_SDA, 0);   

	pwm = metal_pwm_get_device(1);

}

void vApplicationMallocFailedHook( void )
{
	/* vApplicationMallocFailedHook() will only be called if
	configUSE_MALLOC_FAILED_HOOK is set to 1 in FreeRTOSConfig.h.  It is a hook
	function that will get called if a call to pvPortMalloc() fails.
	pvPortMalloc() is called internally by the kernel whenever a task, queue,
	timer or semaphore is created.  It is also called by various parts of the
	demo application.  If heap_1.c or heap_2.c are used, then the size of the
	heap available to pvPortMalloc() is defined by configTOTAL_HEAP_SIZE in
	FreeRTOSConfig.h, and the xPortGetFreeHeapSize() API function can be used
	to query the size of free heap space that remains (although it does not
	provide information on how the remaining heap might be fragmented). */
	taskDISABLE_INTERRUPTS();

	if ( led0_red != NULL )
	{
		// Red light on
		metal_led_off(led0_red);
	}

	_exit(1);
}
/*-----------------------------------------------------------*/

void vApplicationIdleHook( void )
{
	/* vApplicationIdleHook() will only be called if configUSE_IDLE_HOOK is set
	to 1 in FreeRTOSConfig.h.  It will be called on each iteration of the idle
	task.  It is essential that code added to this hook function never attempts
	to block in any way (for example, call xQueueReceive() with a block time
	specified, or call vTaskDelay()).  If the application makes use of the
	vTaskDelete() API function (as this demo application does) then it is also
	important that vApplicationIdleHook() is permitted to return to its calling
	function, because it is the responsibility of the idle task to clean up
	memory allocated by the kernel to any task that has since been deleted. */
}
/*-----------------------------------------------------------*/

void vApplicationStackOverflowHook( TaskHandle_t pxTask, char *pcTaskName )
{
	( void ) pcTaskName;
	( void ) pxTask;

	/* Run time stack overflow checking is performed if
	configCHECK_FOR_STACK_OVERFLOW is defined to 1 or 2.  This hook
	function is called if a stack overflow is detected. */
	taskDISABLE_INTERRUPTS();

	write( STDOUT_FILENO, "ERROR Stack overflow on func: ", 30 );
	write( STDOUT_FILENO, pcTaskName, strlen( pcTaskName ) );


	if ( led0_red != NULL )
	{
		// Red light on
		metal_led_off(led0_red);
	}

	_exit(1);
}
/*-----------------------------------------------------------*/

void vApplicationTickHook( void )
{
	/* The tests in the full demo expect some interaction with interrupts. */
}
/*-----------------------------------------------------------*/

void vAssertCalled( void )
{
	taskDISABLE_INTERRUPTS();

	if ( led0_red != NULL )
	{
		// Red light on
		metal_led_off(led0_red);
	}

	_exit(1);
}

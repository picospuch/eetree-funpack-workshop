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

#include "rgb-led/rgb_led.h"

#define TASK_PRIORITY		(tskIDLE_PRIORITY + 1)

/* The 1s value is converted to ticks using the pdMS_TO_TICKS() macro. */
#define mainQUEUE_TICK_COUNT_FOR_1S			pdMS_TO_TICKS( WAIT_MS )

static void prvSetupHardware( void );

/*
 * The tasks as described in the comments at the top of this file.
 */
static void prvBlinkTask( void *pvParameters );
static void prvOledTask( void *pvParameters );
/*-----------------------------------------------------------*/



struct metal_cpu *cpu0;
struct metal_interrupt *cpu_intr, *tmr_intr;
struct metal_led *led0_red, *led0_green, *led0_blue;
struct metal_gpio *gpio;

USE_UNIT_EPRINTF

/* light a rgb led.  */
int do_led( void )
{
	ext_rgb_led_log("rgb led conrtol demo(RGB_MODE)\r\n");
	eprintf("do_led\r\n");

  while(1)
  {
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
}

int main( void )
{
	eprintf("\r\n\r\n===FreeRTOS START===\r\n");
	eprintf("hz of tick: %d\r\n", configTICK_RATE_HZ);
	// @todo time slicing no effect
	eprintf("task type: %d, %d\r\n", configUSE_PREEMPTION, configUSE_TIME_SLICING);
	eprintf("\r\n");
	
	TaskHandle_t xHandle_BlinkTask, xHandle_OledTask;

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
							 TASK_PRIORITY,
							 &xHandle_OledTask);

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
	
	eprintf("\r\n\r\n===FreeRTOS END===\r\n\r\n");
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

	// do not let it run into metal_shutdown!
	// no tick interrupt will be triggered after that!
	for (;;) vTaskDelay(0);
}

static void prvSetupHardware( void ) {

	gpio = metal_gpio_get_device(0);
	metal_gpio_enable_output(gpio, ARDUINO_SCL); // SCL
	metal_gpio_disable_input(gpio, ARDUINO_SCL);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SCL);
	metal_gpio_set_pin(gpio, ARDUINO_SCL, 0);
	
	metal_gpio_enable_output(gpio, ARDUINO_SDA); // SDA
	metal_gpio_disable_input(gpio, ARDUINO_SDA);
	metal_gpio_disable_pinmux(gpio, ARDUINO_SDA);
	metal_gpio_set_pin(gpio, ARDUINO_SDA, 0);   
   



	/* This demo will toggle LEDs colors so we define them here */
	//led0_red = metal_led_get_rgb("LD0", "red");
	//	metal_led_enable(led0_red);
	//		metal_led_on(led0_red);
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

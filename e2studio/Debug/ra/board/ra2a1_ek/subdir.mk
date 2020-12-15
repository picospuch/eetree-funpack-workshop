################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../ra/board/ra2a1_ek/board_init.c \
../ra/board/ra2a1_ek/board_leds.c 

OBJS += \
./ra/board/ra2a1_ek/board_init.o \
./ra/board/ra2a1_ek/board_leds.o 

C_DEPS += \
./ra/board/ra2a1_ek/board_init.d \
./ra/board/ra2a1_ek/board_leds.d 


# Each subdirectory must supply rules for building sources it contributes
ra/board/ra2a1_ek/%.o: ../ra/board/ra2a1_ek/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GNU ARM Cross C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m23 -mthumb -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections -Wunused -Wuninitialized -Wall -Wextra -Wmissing-declarations -Wconversion -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal  -g -D_RENESAS_RA_ -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\arm\CMSIS_5\CMSIS\Core\Include" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\src" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc\api" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc\instances" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_gen" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_cfg\fsp_cfg" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_cfg\fsp_cfg\bsp" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\src\r_usb_basic\src\driver\inc" -std=c99 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -c -o "$@" -x c "$<"
	@echo 'Finished building: $<'
	@echo ' '



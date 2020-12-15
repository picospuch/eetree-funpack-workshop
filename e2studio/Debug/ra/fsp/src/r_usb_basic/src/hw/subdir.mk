################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_abs.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_access.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_dma.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_hostelectrical.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_abs.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_access.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_mcu.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_abs.c \
../ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_access.c 

OBJS += \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_abs.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_access.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_dma.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hostelectrical.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_abs.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_access.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_mcu.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_abs.o \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_access.o 

C_DEPS += \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_abs.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_creg_access.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_dma.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hostelectrical.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_abs.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_hreg_access.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_mcu.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_abs.d \
./ra/fsp/src/r_usb_basic/src/hw/r_usb_preg_access.d 


# Each subdirectory must supply rules for building sources it contributes
ra/fsp/src/r_usb_basic/src/hw/%.o: ../ra/fsp/src/r_usb_basic/src/hw/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GNU ARM Cross C Compiler'
	arm-none-eabi-gcc -mcpu=cortex-m23 -mthumb -O2 -fmessage-length=0 -fsigned-char -ffunction-sections -fdata-sections -Wunused -Wuninitialized -Wall -Wextra -Wmissing-declarations -Wconversion -Wpointer-arith -Wshadow -Wlogical-op -Waggregate-return -Wfloat-equal  -g -D_RENESAS_RA_ -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\arm\CMSIS_5\CMSIS\Core\Include" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\src" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc\api" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\inc\instances" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_gen" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_cfg\fsp_cfg" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra_cfg\fsp_cfg\bsp" -I"Z:\eetree-ra2a1\example-quickstart\quickstart_ek_ra2a1_ep\e2studio\ra\fsp\src\r_usb_basic\src\driver\inc" -std=c99 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -c -o "$@" -x c "$<"
	@echo 'Finished building: $<'
	@echo ' '



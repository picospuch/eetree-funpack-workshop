/* generated vector header file - do not edit */
#ifndef VECTOR_DATA_H
#define VECTOR_DATA_H
/* Number of interrupts allocated */
#ifndef VECTOR_DATA_IRQ_COUNT
#define VECTOR_DATA_IRQ_COUNT    (4)
#endif
/* ISR prototypes */
void r_icu_isr(void);
void gpt_counter_overflow_isr(void);
void usbfs_interrupt_handler(void);
void usbfs_resume_handler(void);

/* Vector table allocations */
#define VECTOR_NUMBER_ICU_IRQ6 ((IRQn_Type) 0) /* ICU IRQ6 (External pin interrupt 6) */
#define VECTOR_NUMBER_GPT0_COUNTER_OVERFLOW ((IRQn_Type) 1) /* GPT0 COUNTER OVERFLOW (Overflow) */
#define VECTOR_NUMBER_USBFS_INT ((IRQn_Type) 2) /* USBFS INT (USBFS interrupt) */
#define VECTOR_NUMBER_USBFS_RESUME ((IRQn_Type) 3) /* USBFS RESUME (USBFS resume interrupt) */
typedef enum IRQn
{
    Reset_IRQn = -15,
    NonMaskableInt_IRQn = -14,
    HardFault_IRQn = -13,
    MemoryManagement_IRQn = -12,
    BusFault_IRQn = -11,
    UsageFault_IRQn = -10,
    SecureFault_IRQn = -9,
    SVCall_IRQn = -5,
    DebugMonitor_IRQn = -4,
    PendSV_IRQn = -2,
    SysTick_IRQn = -1,
    ICU_IRQ6_IRQn = 0, /* ICU IRQ6 (External pin interrupt 6) */
    GPT0_COUNTER_OVERFLOW_IRQn = 1, /* GPT0 COUNTER OVERFLOW (Overflow) */
    USBFS_INT_IRQn = 2, /* USBFS INT (USBFS interrupt) */
    USBFS_RESUME_IRQn = 3, /* USBFS RESUME (USBFS resume interrupt) */
} IRQn_Type;
#endif /* VECTOR_DATA_H */

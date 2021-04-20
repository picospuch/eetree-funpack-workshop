/*
 * File:   main.c
 * Author: spuch
 *
 * Created on April 16, 2021, 6:09 PM
 */

#include "main.h"

#include <xc.h>

#define _XTAL_FREQ 8000000
#define LED_TRIS_B TRISBbits.TRISB6 // BLUE
#define LED_TRIS_G TRISBbits.TRISB4 // Green
#define LED_TRIS_R TRISCbits.TRISC7 // RED
//#define LED_TRIS TRISAbits.TRISA2
#define LED_STATE_B LATBbits.LATB6
#define LED_STATE_G LATBbits.LATB4
#define LED_STATE_R LATCbits.LATC7

#define KEY_SCAN_TRIS TRISCbits.TRISC6
#define KEY_SCAN_STATE LATCbits.LATC6

#define KEY_TEST_TRIS TRISCbits.TRISC2
#define KEY_TEST_STATE PORTCbits.RC2
#define KEY_TEST_WEAK_PULL_UP WPUCbits.WPUC2

void initOSC(void) {
    OSCCON1 = 0x10;
    OSCCON3 = 0x00;
    OSCEN = 0x00;
    OSCFRQ = 0x03;
    OSCTUNE = 0x00;
    while (PLLR == 0);
}

void initWDT(void) {
    WDTCON0bits.SWDTEN = 0;
    WDTCON0bits.SEN = 0;
}

void initIO() {
    ANSELA = 0x00;
    ANSELB = 0x00;
    ANSELC = 0x00;
    
    LED_TRIS_R = 0; // output
    LED_STATE_R = 1;

    LED_TRIS_G = 0; // output
    LED_STATE_G = 1;

    LED_TRIS_B = 0; // output
    LED_STATE_B = 1;
    
    KEY_SCAN_TRIS = 0; // output
    KEY_SCAN_STATE = 1; // high
    
    KEY_TEST_WEAK_PULL_UP = 0;
    KEY_TEST_TRIS = 1; // input
    KEY_TEST_STATE = 0  ; // switch off
}

void main(void) {
    initOSC();
    initWDT();
    initIO();
    while (1) {
        if (KEY_TEST_STATE == 1) {
            LED_STATE_B = 0;
            __delay_ms(100);
            LED_STATE_B = 1;
            __delay_ms(100);
            LED_STATE_G = 0;
            __delay_ms(100);
            LED_STATE_G = 1;
            __delay_ms(100);
            LED_STATE_R = 0;
            __delay_ms(100);
            LED_STATE_R = 1;
            __delay_ms(100);
        }
    }
    return;
}

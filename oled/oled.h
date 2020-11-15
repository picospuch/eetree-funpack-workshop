#ifndef OLED_H_
#define OLED_H_

#include <stdint.h>

#include "main.h"


#define OLED_MODE 0
#define SIZE 16
#define XLevelL		0x00
#define XLevelH		0x10
#define Max_Column	128
#define Max_Row		64
#define	Brightness	0xFF 
#define X_WIDTH 	128
#define Y_WIDTH 	64	

// typedef
#define u8     uint8_t
#define u16    uint16_t
#define u32    uint32_t

// interface implemented by system
extern int oled_setup_hardware(void);
extern void oled_dc_clear(void);
extern void oled_dc_set(void);
extern void oled_cs_clear(void);
extern void oled_cs_set(void);
extern void oled_rst_clear(void);
extern void oled_rst_set(void);
extern void oled_spi_transfer(uint8_t *dat, uint8_t len);
extern void delay_ms(u16 nms);

//----------------- OLED PIN ----------------  	
#define OLED_CS_Clr()      oled_cs_clear()
#define OLED_CS_Set()      oled_cs_set()

#define OLED_DC_Clr()      oled_dc_clear()
#define OLED_DC_Set()      oled_dc_set()

#define OLED_RST_Clr()      oled_rst_clear()
#define OLED_RST_Set()      oled_rst_set()

#define OLED_CMD  0
#define OLED_DATA 1

//-------------------------------- display define ------------------------------
// for 8*16 char can only display 4 rows, 16 chars each row.
#define OLED_DISPLAY_ROW_1    0    // yellow
#define OLED_DISPLAY_ROW_2    2    // blue
#define OLED_DISPLAY_ROW_3    4    // blue
#define OLED_DISPLAY_ROW_4    6    // blue

#define OLED_DISPLAY_COLUMN_START    0    // colloum from left pos 0

#define OLED_DISPLAY_MAX_CHAR_PER_ROW    16   // max 16 chars each row


void OLED_WR_Byte(u8 dat, u8 cmd);   
void OLED_WR_Bytes(u8 *dat, u8 len, u8 cmd);   
void OLED_Display_On(void);
void OLED_Display_Off(void);	  

void OLED_DrawPoint(u8 x,u8 y,u8 t);
void OLED_Fill(u8 x1,u8 y1,u8 x2,u8 y2,u8 dot);
void OLED_ShowChar(u8 x,u8 y,u8 chr);
void OLED_FillAll(void);
void OLED_ShowNum(u8 x,u8 y,u32 num,u8 len,u8 size);

void OLED_Set_Pos(unsigned char x, unsigned char y);
void OLED_ShowCHinese(u8 x,u8 y,u8 no);
void OLED_DrawBMP(unsigned char x0, unsigned char y0,unsigned char x1, unsigned char y1,unsigned char BMP[]);


/*-------------------------------------------------------- USER INTERFACES -----------------------------------------*/


/**
 * @brief Initialize OLED device.
 *
 * @return none
 */
void OLED_Init(void);


/**
 * @brief Clear up all data shown on OLED
 *
 * @return none
 */
void OLED_Clear(void);


/**
 * @brief show string in OLED specified place
 *
 * @param x: Position the X axis of the stiring to display
 * @param y: Position the Y axis of the string to display
 * @param p: String to be displayed in OLED
 * 
 * @return none
 */
void OLED_ShowString(u8 x,u8 y, char *p);

#endif // OLED_H_

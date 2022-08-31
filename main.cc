/* A start of esp-idf project.

Copyright (C) 2022 picospuch.  */

#include <M5StickCPlus.h>

#include "tinyml.h"

static const char *TAG = "MY_MAIN";

unsigned char LEDS [] =
{
  0x00,
  0x00,
  0x00,
  0x00,
  0x00,
  0x00,
  0x00,
  0x00
};

const char* program_name;
bool lcd_direction_mode = true; // The direction, false= left bottom to right up, true= right bottom to left up
bool leds_on = true;

// We need these for program updates
int counter = 0;
//int i = 0;
int scroll_speed = 1;
int program_index = 100;
// Program - Wave
int p_w_col0 = 0;
// Program - Circle
int p_c_index = 0;
bool p_c_flp;

//// fps counter
int fps_counted_frames = 0;
unsigned long fps_start_time = 0;
static float fps_value = 0;
static int spi_speed = 40000000;

void fps_print();
inline void change_spi_speed();

//// led driver
void setup_led_display(void);
void ChooseProgram(int index);
void UpdateProgram();
void UpdateProgram_MiddWave();
void UpdateProgram_Random();
void UpdateProgram_Full();
void UpdateProgram_Wave();
void UpdateProgram_Heart2();
// Optional methods, plays around with leds.
void SetChar(char* c);
// Rendering methods
void Render();


inline void setup() {
  M5.Lcd.setTextSize(3);  // Set font size.
  M5.Lcd.setRotation(1);  // Rotate the screen.
  // LCD display.  Lcd显示
  M5.Lcd.print("hello");

  SetChar("heart");
}

void cell() {
  int cell_size = 0;
  SPI.beginTransaction(SPISettings(1000, MSBFIRST, SPI_MODE0));
  if (cell_size < 64) ++cell_size; else cell_size = 0;
  digitalWrite(25, HIGH);
  digitalWrite(25, LOW);
  SPI.transfer(cell_size);
  SPI.transfer(cell_size);
  digitalWrite(25, HIGH);
  SPI.endTransaction();
}

int c = 0;
inline void loop() {
  // m5stack
  M5.update();
  
  M5.Lcd.setCursor(30, 50);
  M5.Lcd.printf("led pannel fps: %.2f", fps_value);

  int style[] = {2,4,5,7,18};
  if (M5.BtnA.wasReleased()) {
    // if (c < sizeof(style) / sizeof(style[0]) - 1) {
    //   c++;
    // } else {
    //   c = 0;
    // }

    // program_index = style[c];
    // ChooseProgram(program_index);

    change_spi_speed();
    M5.Lcd.fillScreen(BLACK);
    M5.Lcd.setCursor(0, 0);
    M5.Lcd.printf("spi: %d", spi_speed);
    //    if (program_name) M5.Lcd.print(program_name);
  } else if (M5.BtnB.wasReleased()) {
    M5.Lcd.print('B');
  } else if (M5.BtnB.wasReleasefor(700)) {
    M5.Lcd.fillScreen(BLACK);
    M5.Lcd.setCursor(0, 0);
  }
}

void result_callback(const char *label, float value) {
  if (strcmp(label, "yes") == 0) {
    M5.Lcd.setTextColor(GREEN);
    //    SetChar("Y");
  } else if (strcmp(label, "no") == 0) {
    M5.Lcd.setTextColor(RED);
    //    SetChar("N");
  }
  M5.Lcd.fillScreen(BLACK);
  M5.Lcd.setCursor(0, 0);
  M5.Lcd.print(label);
}

extern "C" void app_main() {
  initArduino();

  M5.begin();  // Initialize M5StickC Plus.

  //// GPIO
  // D -> G26, SRCLK -> G0, G36/G25 -> RCLK
  // using G25
  pinMode(36, INPUT);
  gpio_pulldown_dis(GPIO_NUM_36);
  gpio_pullup_dis(GPIO_NUM_36);
  pinMode(25, OUTPUT);

  //// SPI
  // SS -> G36, MOSI -> G26, SCK -> G33, MISO -> -1
  SPI.begin(/*sck*/ 33, /*miso*/ -1, /*mosi*/ 26, /*ss*/ 25);

  //// led_display
  setup_led_display();
  
  //// edge-impulse
  tinyml_start_impulse(result_callback, true, false);

  setup();
  
  // Arduino-like loop()
  while(true){
    // delay(1000);
    loop();
    //tinyml_run_impulse();
  }
  
  // WARNING: if program reaches end of function app_main() the MCU will restart.
}

void led_display( void * pvParameters )
{
  fps_start_time = millis();
  
  for( ;; )
  {
    // eetree
    Render();
    UpdateProgram();
  }
}

// Function that creates a task.
void setup_led_display(void)
{
  //  static uint8_t ucParameterToPass;
  TaskHandle_t xHandle = NULL;

  // 1: run on APPlication core, 0: run on PROtocol core
  xTaskCreatePinnedToCore(led_display, "LED_DISPLAY", 2048, NULL, tskIDLE_PRIORITY + 2, &xHandle, 1);
  //xTaskCreate(led_display, "LED_DISPLAY", 1024, NULL, tskIDLE_PRIORITY, &xHandle);
  configASSERT(xHandle);

  // if(xHandle != NULL)
  // {
  //    vTaskDelete(xHandle);
  // }
}

inline void UpdateProgram()
{
  if (!leds_on)
    return;
  counter++;
  if (counter >= scroll_speed)
  {
    fps_print();
    counter = 0;
    ChooseProgram(program_index);
  }
}

void ChooseProgram(int index)
{
  switch (index)
  {
    // case 0: UpdateProgram_SCAN(); program_name = "Scan"; break;
    // case 1: UpdateProgram_Count(); program_name = "Fill"; break;
    case 2: UpdateProgram_Full(); program_name = "All";  break;
    // case 3: UpdateProgram_Analiser(); program_name = "Bars"; break;
    case 4: UpdateProgram_Wave(); program_name = "Wave 1"; break;
    case 5: UpdateProgram_MiddWave(); program_name = "Wave 2"; break;
    // case 6: UpdateProgram_FILL(); program_name = "Fill"; break;
    case 7: UpdateProgram_Random(); program_name = "Random"; break;
    // case 8: UpdateProgram_River(); program_name = "River"; break;
    // case 9: UpdateProgram_Yellows(); program_name = "Yellows"; break;
    // case 10: UpdateProgram_Greens(); program_name = "Greens"; break;
    // case 11: UpdateProgram_Reds(); program_name = "Reds"; break;
    // case 12: UpdateProgram_AlphaBet(); program_name = "Alphabet"; break;
    // case 13: UpdateProgram_CountNumbers(); program_name = "Numbers"; break;
    // case 14: UpdateProgram_ILoveYou(); program_name = "Love You"; break;
    // case 15: UpdateProgram_Circle(); program_name = "Circle"; break;
    // case 16: UpdateProgram_Tunnle(); program_name = "Tunnle"; break;
    // case 17: UpdateProgram_Heart1(); program_name = "Heart 1"; break;
    case 18: UpdateProgram_Heart2(); program_name = "Heart 2"; break;
    // case 19: UpdateProgram_Heart3(); program_name = "Heart 3"; break;
    // case 20: UpdateProgram_Heart4(); program_name = "Heart 4"; break;
    // case 21: UpdateProgram_CyclePrograms(); program_name = "Cycle Programs"; break;
    // case 22: UpdateProgram_CycleProgramsRandom(); program_name = "Random Prog"; break;
  }
}

void UpdateProgram_Full()
{
  for (int i = 0; i < 8; i++)
  {
    LEDS[i] = 0xFF;
  }
}

void SetChar(char* c)
{
  // The scaning process starts from row 0 to row 7. Since the leds are aranged from bottom to top, we need to setup them from top to bottom.

  if (strcmp(c, "-") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00000000;
    LEDS [5] = B00000000;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B00000000;
    LEDS [1] = B00000000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "heart") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B11100111;
    LEDS [5] = B10011001;
    LEDS [4] = B10000001;
    LEDS [3] = B10000001;
    LEDS [2] = B01000010;
    LEDS [1] = B00100100;
    LEDS [0] = B00011000;
  }
  else if (strcmp(c, "heartfull") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B11100111;
    LEDS [5] = B11111111;
    LEDS [4] = B11111111;
    LEDS [3] = B11111111;
    LEDS [2] = B01111110;
    LEDS [1] = B00111100;
    LEDS [0] = B00011000;
  }
  else if (strcmp(c, "1") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00001110;
    LEDS [5] = B00010110;
    LEDS [4] = B00100110;
    LEDS [3] = B01000110;
    LEDS [2] = B00000110;
    LEDS [1] = B00000110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "2") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B00000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B01000000;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "3") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B00000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B00000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "4") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B00000010;
    LEDS [1] = B00000010;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "5") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B00000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "6") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B01000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "7") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B00000110;
    LEDS [4] = B00001100;
    LEDS [3] = B00011000;
    LEDS [2] = B00110000;
    LEDS [1] = B01100000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "8") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00111100;
    LEDS [5] = B01000010;
    LEDS [4] = B00111100;
    LEDS [3] = B00111100;
    LEDS [2] = B01000010;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "9") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B00000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "0") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00111100;
    LEDS [5] = B01000110;
    LEDS [4] = B01001010;
    LEDS [3] = B01010010;
    LEDS [2] = B01100010;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "a") == 0 || strcmp(c, "A") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B01000010;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "b") == 0 || strcmp(c, "B") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111100;
    LEDS [5] = B01000010;
    LEDS [4] = B01111100;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B01111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "c") == 0 || strcmp(c, "C") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01000000;
    LEDS [3] = B01000000;
    LEDS [2] = B01000000;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "d") == 0 || strcmp(c, "D") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111100;
    LEDS [5] = B01000010;
    LEDS [4] = B01000010;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B01111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "e") == 0 || strcmp(c, "E") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B01000000;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "f") == 0 || strcmp(c, "F") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B01000000;
    LEDS [1] = B01000000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "g") == 0 || strcmp(c, "G") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01011110;
    LEDS [3] = B01010010;
    LEDS [2] = B01000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "h") == 0 || strcmp(c, "H") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B01000010;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "i") == 0 || strcmp(c, "I") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00111100;
    LEDS [5] = B00011000;
    LEDS [4] = B00011000;
    LEDS [3] = B00011000;
    LEDS [2] = B00011000;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "j") == 0 || strcmp(c, "J") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00011110;
    LEDS [5] = B00001100;
    LEDS [4] = B00001100;
    LEDS [3] = B00001100;
    LEDS [2] = B00101100;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "k") == 0 || strcmp(c, "K") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01001000;
    LEDS [5] = B01010000;
    LEDS [4] = B01100000;
    LEDS [3] = B01100000;
    LEDS [2] = B01011000;
    LEDS [1] = B01000100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "l") == 0 || strcmp(c, "L") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000000;
    LEDS [5] = B01000000;
    LEDS [4] = B01000000;
    LEDS [3] = B01000000;
    LEDS [2] = B01000000;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "m") == 0 || strcmp(c, "M") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01011010;
    LEDS [4] = B01011010;
    LEDS [3] = B01011010;
    LEDS [2] = B01011010;
    LEDS [1] = B01011010;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "n") == 0 || strcmp(c, "N") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B01100010;
    LEDS [4] = B01010010;
    LEDS [3] = B01001010;
    LEDS [2] = B01000110;
    LEDS [1] = B01000010;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "o") == 0 || strcmp(c, "O") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B00111100;
    LEDS [5] = B01000010;
    LEDS [4] = B01000010;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "p") == 0 || strcmp(c, "P") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01111110;
    LEDS [2] = B01000000;
    LEDS [1] = B01000000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "q") == 0 || strcmp(c, "Q") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000010;
    LEDS [4] = B01000010;
    LEDS [3] = B01001010;
    LEDS [2] = B01000110;
    LEDS [1] = B01111110;
    LEDS [0] = B00000001;
  }
  else if (strcmp(c, "r") == 0 || strcmp(c, "R") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000010;
    LEDS [4] = B01111110;
    LEDS [3] = B01010000;
    LEDS [2] = B01001000;
    LEDS [1] = B01000100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "s") == 0 || strcmp(c, "S") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B01000000;
    LEDS [4] = B01111110;
    LEDS [3] = B00000010;
    LEDS [2] = B00000010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "t") == 0 || strcmp(c, "T") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B00011000;
    LEDS [4] = B00011000;
    LEDS [3] = B00011000;
    LEDS [2] = B00011000;
    LEDS [1] = B00011000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "u") == 0 || strcmp(c, "U") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B01000010;
    LEDS [4] = B01000010;
    LEDS [3] = B01000010;
    LEDS [2] = B01000010;
    LEDS [1] = B00111100;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "v") == 0 || strcmp(c, "V") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B01000010;
    LEDS [4] = B01000010;
    LEDS [3] = B01000010;
    LEDS [2] = B00100100;
    LEDS [1] = B00011000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "w") == 0 || strcmp(c, "W") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01011010;
    LEDS [5] = B01011010;
    LEDS [4] = B01011010;
    LEDS [3] = B01011010;
    LEDS [2] = B01011010;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "x") == 0 || strcmp(c, "X") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B00100100;
    LEDS [4] = B00011000;
    LEDS [3] = B00100100;
    LEDS [2] = B01000010;
    LEDS [1] = B00000000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "y") == 0 || strcmp(c, "Y") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01000010;
    LEDS [5] = B00100100;
    LEDS [4] = B00011000;
    LEDS [3] = B00011000;
    LEDS [2] = B00011000;
    LEDS [1] = B00011000;
    LEDS [0] = B00000000;
  }
  else if (strcmp(c, "z") == 0 || strcmp(c, "Z") == 0)
  {
    LEDS [7] = B00000000;
    LEDS [6] = B01111110;
    LEDS [5] = B00000100;
    LEDS [4] = B00001000;
    LEDS [3] = B00010000;
    LEDS [2] = B00100000;
    LEDS [1] = B01111110;
    LEDS [0] = B00000000;
  }

  for (int i = 0; i < 8; i ++) {
    byte reg = (LEDS[i] & 0x80) >> 7;
    reg |= (LEDS[i] & 0x40) >> 5;
    reg |= (LEDS[i] & 0x20) >> 3;
    reg |= (LEDS[i] & 0x10) >> 1;
    reg |= (LEDS[i] & 0x08) << 1;
    reg |= (LEDS[i] & 0x04) << 3;
    reg |= (LEDS[i] & 0x02) << 5;
    reg |= (LEDS[i] & 0x01) << 7;

    LEDS[i] = reg;
  }
}

// (120hz frame) (12000hz refresh), (192000hz clk)
inline void change_spi_speed() {
  if (spi_speed < 10000) {
    spi_speed = 40000000;
  }
  if (spi_speed > 5000000) {
    spi_speed -= 5000000;
  } else if (spi_speed > 100000) {
    spi_speed -= 100000;
  } else if (spi_speed > 10000) {
    spi_speed -= 10000;
  }
}

inline void Render() {
  int n = 3; // n = refresh-freq / frame-freq
  SPI.beginTransaction(SPISettings(spi_speed, MSBFIRST, SPI_MODE0));
  digitalWrite(25, HIGH);

  if (!leds_on)
    return;
  for (int j = 0; j < n; ++j) {
    if (!lcd_direction_mode)
      {
        for (int i = 0; i < 8; ++i) {
          // row, then col
          digitalWrite(25, LOW);
          SPI.transfer(1 << i);
          SPI.transfer(LEDS[i]);
          digitalWrite(25, HIGH);
        }
      }
    else // Render method 2. Optional
      {
        int i = 0;
        for (; i < 8; ++i) {
          digitalWrite(25, LOW);
          SPI.transfer(1 << i);
          SPI.transfer(LEDS[7 - i]);
          digitalWrite(25, HIGH);
        }
        digitalWrite(25, LOW);
        SPI.transfer(1 << i);
        SPI.transfer(0);
        digitalWrite(25, HIGH);
      }
  }
  SPI.endTransaction();
  ++fps_counted_frames;
}

inline void fps_print() {
  fps_value = fps_counted_frames / ((millis() - fps_start_time) / 1000.f);
  //ESP_LOGI(TAG, "fps: %.2f", fps_value);
}

void UpdateProgram_Wave()
{
  int col_0 = 0;
  int col_1 = 0;
  int col_2 = 0;
  int col_3 = 0;
  int col_4 = 0;
  int col_5 = 0;
  int col_6 = 0;
  int col_7 = 0;

  p_w_col0++;
  if (p_w_col0 > 14)
  {
    p_w_col0 = 0;
  }

  if (p_w_col0 > 7)
  {
    col_0 = 14 - p_w_col0;

    col_1 = col_0 - 1;
    if (col_1 < 0)
    {
      col_1 *= -1;
    }

    col_2 = col_0 - 2;
    if (col_2 < 0)
    {
      col_2 *= -1;
    }

    col_3 = col_0 - 3;
    if (col_3 < 0)
    {
      col_3 *= -1;
    }

    col_4 = col_0 - 4;
    if (col_4 < 0)
    {
      col_4 *= -1;
    }

    col_5 = col_0 - 5;
    if (col_5 < 0)
    {
      col_5 *= -1;
    }
    col_6 = col_0 - 6;
    if (col_6 < 0)
    {
      col_6 *= -1;
    }
    col_7 = col_0 - 7;
    if (col_7 < 0)
    {
      col_7 *= -1;
    }
  }
  else
  {
    col_0 = p_w_col0;

    col_1 = col_0 + 1;
    if (col_1 > 7)
    {
      col_1 = 14 - col_1;
    }

    col_2 = col_0 + 2;
    if (col_2 > 7)
    {
      col_2 = 14 - col_2;
    }

    col_3 = col_0 + 3;
    if (col_3 > 7)
    {
      col_3 = 14 - col_3;
    }

    col_4 = col_0 + 4;
    if (col_4 > 7)
    {
      col_4 = 14 - col_4;
    }

    col_5 = col_0 + 5;
    if (col_5 > 7)
    {
      col_5 = 14 - col_5;
    }

    col_6 = col_0 + 6;
    if (col_6 > 7)
    {
      col_6 = 14 - col_6;
    }

    col_7 = col_0 + 7;
    if (col_7 > 7)
    {
      col_7 = 14 - col_7;
    }
  }

  // Clear all leds
  for (int i = 0; i < 8; i++)
  {
    LEDS[i] = 0;
  }
  // Set first column
  for (int c = 0; c <= col_0; c++)
  {
    LEDS[c] = 1;
  }
  // Set second column
  for (int c = 0; c <= col_1; c++)
  {
    LEDS[c] |= 0x2;
  }
  // Set third column
  for (int c = 0; c <= col_2; c++)
  {
    LEDS[c] |= 0x4;
  }
  // Set fourth column
  for (int c = 0; c <= col_3; c++)
  {
    LEDS[c] |= 0x8;
  }
  // Set fifth column
  for (int c = 0; c <= col_4; c++)
  {
    LEDS[c] |= 0x10;
  }
  // Set sixth column
  for (int c = 0; c <= col_5; c++)
  {
    LEDS[c] |= 0x20;
  }
  // Set seventh column
  for (int c = 0; c <= col_6; c++)
  {
    LEDS[c] |= 0x40;
  }
  // Set eighth column
  for (int c = 0; c <= col_7; c++)
  {
    LEDS[c] |= 0x80;
  }
}

void UpdateProgram_Heart2()
{
  switch (p_c_index)
  {
    case 0 :
      {
        LEDS [7] = B00000000;
        LEDS [6] = B00000000;
        LEDS [5] = B00000000;
        LEDS [4] = B00111100;
        LEDS [3] = B00011000;
        LEDS [2] = B00000000;
        LEDS [1] = B00000000;
        LEDS [0] = B00000000;
        break;
      }
    case 1 :
      {
        LEDS [7] = B00000000;
        LEDS [6] = B00000000;
        LEDS [5] = B00100100;
        LEDS [4] = B00111100;
        LEDS [3] = B00100100;
        LEDS [2] = B00011000;
        LEDS [1] = B00000000;
        LEDS [0] = B00000000;
        break;
      }
    case 2 :
      {
        LEDS [7] = B00000000;
        LEDS [6] = B00000000;
        LEDS [5] = B01100110;
        LEDS [4] = B01011010;
        LEDS [3] = B01000010;
        LEDS [2] = B00100100;
        LEDS [1] = B00011000;
        LEDS [0] = B00000000;
        break;
      }
    case 3 :
      {
        LEDS [7] = B00000000;
        LEDS [6] = B11100111;
        LEDS [5] = B10011001;
        LEDS [4] = B10000001;
        LEDS [3] = B10000001;
        LEDS [2] = B01000010;
        LEDS [1] = B00100100;
        LEDS [0] = B00011000;
        break;
      }
  }

  p_c_index ++;
  if (p_c_index > 3)
  {
    p_c_index = 0;
  }

}
void UpdateProgram_Random()
{
  for (int i = 0; i < 8; i++)
  {
    LEDS[i] = random(0, 0xFF);
  }
}

void UpdateProgram_MiddWave()
{
  int col_0 = 0;
  int col_1 = 0;
  int col_2 = 0;
  int col_3 = 0;
  int col_4 = 0;
  int col_5 = 0;
  int col_6 = 0;
  int col_7 = 0;

  p_w_col0++;
  if (p_w_col0 > 14)
  {
    p_w_col0 = 0;
  }

  if (p_w_col0 > 7)
  {
    col_0 = 14 - p_w_col0;

    col_1 = col_0 - 2;
    if (col_1 < 0)
    {
      col_1 *= -1;
    }

    col_2 = col_0 - 4;
    if (col_2 < 0)
    {
      col_2 *= -1;
    }

    col_3 = col_0 - 6;
    if (col_3 < 0)
    {
      col_3 *= -1;
    }

    col_4 = col_0 - 6;
    if (col_4 < 0)
    {
      col_4 *= -1;
    }

    col_5 = col_0 - 6;
    if (col_5 < 0)
    {
      col_5 *= -1;
    }
    col_6 = col_0 - 4;
    if (col_6 < 0)
    {
      col_6 *= -1;
    }
    col_7 = col_0 - 2;
    if (col_7 < 0)
    {
      col_7 *= -1;
    }
  }
  else
  {
    col_0 = p_w_col0;

    col_1 = col_0 + 2;
    if (col_1 > 7)
    {
      col_1 = 14 - col_1;
    }

    col_2 = col_0 + 4;
    if (col_2 > 7)
    {
      col_2 = 14 - col_2;
    }

    col_3 = col_0 + 6;
    if (col_3 > 7)
    {
      col_3 = 14 - col_3;
    }

    col_4 = col_0 + 6;
    if (col_4 > 7)
    {
      col_4 = 14 - col_4;
    }

    col_5 = col_0 + 6;
    if (col_5 > 7)
    {
      col_5 = 14 - col_5;
    }

    col_6 = col_0 + 4;
    if (col_6 > 7)
    {
      col_6 = 14 - col_6;
    }

    col_7 = col_0 + 2;
    if (col_7 > 7)
    {
      col_7 = 14 - col_7;
    }
  }

  // Clear all leds
  for (int i = 0; i < 8; i++)
  {
    LEDS[i] = 0;
  }
  // Set first column
  for (int c = 0; c <= col_0; c++)
  {
    LEDS[c] = 1;
  }
  // Set second column
  for (int c = 0; c <= col_1; c++)
  {
    LEDS[c] |= 0x2;
  }
  // Set third column
  for (int c = 0; c <= col_2; c++)
  {
    LEDS[c] |= 0x4;
  }
  // Set fourth column
  for (int c = 0; c <= col_3; c++)
  {
    LEDS[c] |= 0x8;
  }
  // Set fifth column
  for (int c = 0; c <= col_4; c++)
  {
    LEDS[c] |= 0x10;
  }
  // Set sixth column
  for (int c = 0; c <= col_5; c++)
  {
    LEDS[c] |= 0x20;
  }
  // Set seventh column
  for (int c = 0; c <= col_6; c++)
  {
    LEDS[c] |= 0x40;
  }
  // Set eighth column
  for (int c = 0; c <= col_7; c++)
  {
    LEDS[c] |= 0x80;
  }
}

### yasnake
## copyright: picospuch 2021
## license: GPLv3
import random
import time
import uasyncio as asyncio
from machine import Pin, SPI

import st7789
import vga2_8x8 as font

from buzzer_music import music

class pin_cfg:
    sck = 2
    mosi = 3
    rst = 0
    dc = 1
    rotary_encoder_a = 4
    rotary_encoder_b = 5
    rotary_push_button = 6
    k1 = 7
    piezo = 16
    led = 25

class color_cfg:
    black = st7789.BLACK
    magenta = st7789.MAGENTA
    blue = st7789.BLUE
    snake = st7789.MAGENTA
    food = st7789.RED
    background = st7789.color565(0, 255, 120)

class tile_cfg:
    block0 = b"\xfe" # ■
    block1 = b"\xb2" # ▓
    block = b"\xfe"
tile_cfg.block = tile_cfg.block1

class board_cfg:
    width = 30
    height = 29
    dashboard_x = 0
    dashboard_y = 29
    dashboard_width = 30
    dashboard_height = 1

class button:
    def __init__(self, pin, callback, trigger=Pin.IRQ_RISING, min_ago=200):
        print("button init")
        self.callback = callback
        self.min_ago = min_ago
        self._next_call = time.ticks_ms() + self.min_ago

        self.pin = Pin(pin, Pin.IN)

        self.pin.irq(trigger=trigger, handler=self.debounce_handler)

    def call_callback(self, pin):
        self.callback(pin)

    def debounce_handler(self, pin):
        print("debounce")
        if time.ticks_ms() > self._next_call:
            self._next_call = time.ticks_ms() + self.min_ago
            self.call_callback(pin)

class rotary_encoder:
    def __init__(self, pin_a, pin_b, callback):
        print(" init")
        self.callback = callback

        self.min_ago = 500
        self._next_call = time.ticks_ms() + self.min_ago

        self.pin_a = Pin(pin_a, Pin.IN)
        self.pin_b = Pin(pin_b, Pin.IN)

        self.pin_a.irq(trigger=Pin.IRQ_RISING|Pin.IRQ_FALLING, handler=self.debounce_handler)

    def call_callback(self, pin):
        if self.pin_b.value() != self.pin_a.value():
            self.callback("cw")
        else:
            self.callback("ccw")

    def debounce_handler(self, pin):
        #print("debounce")
        if time.ticks_ms() > self._next_call:
            self._next_call = time.ticks_ms() + self.min_ago
            self.call_callback(pin)

class hardware():
    def init():
        # screen
        spi = SPI(0, baudrate=31250000, polarity=1, phase=0, sck=Pin(pin_cfg.sck, Pin.OUT), mosi=Pin(pin_cfg.mosi, Pin.OUT))
        tft = st7789.ST7789(
            spi,
            240,
            240,
            reset=Pin(pin_cfg.rst, Pin.OUT),
            dc=Pin(pin_cfg.dc, Pin.OUT),
            #xstart=0,
            #ystart=0,
            rotation=0)

        tft.offset(0, 0)
        tft.init()
        tft.fill(color_cfg.background)
        tft.fill_rect(0, 232, 240, 8, color_cfg.blue)

        hardware.tft = tft

pen_color = color_cfg.background

def outtextxy(x, y, c):
    global pen_color
    hardware.tft.text(font, c, x*8, y*8, pen_color, color_cfg.background)

def setcolor(c):
    global pen_color
    pen_color = c

class location ():
    def __init__(self, x, y):
        self.x = x
        self.y = y

class snake():
    UP = 1
    DOWN = 2
    LEFT = 3
    RIGHT = 4

    PAUSE = 1
    RUNNING = 2
    STOP = 3

    n = 6 # 6 body nodes + 1 head + 1 tail(invisible) = 8 nodes, at very first
    
    def __init__(self):
        # bgm
        self.bgm = p_music(p_music.song0, tempo=1, duty=500, pins=[Pin(pin_cfg.piezo, Pin.OUT)])
        
        # controller
        self.re = rotary_encoder(pin_cfg.rotary_encoder_a, pin_cfg.rotary_encoder_b, self.direction_callback)
        self.k1 = button(pin_cfg.k1, self.k1_callback)

        self.haskey = False
        self.k = "cw"
        self.last_k = self.k

        # iot
        self.led = Pin(25, Pin.OUT)

        # interest
        self.score = 0
        self.n = snake.n
        self.state = self.RUNNING

    def direction_callback(self, d):
        print(d + "\n")
        self.haskey = True
        self.last_k = self.k
        self.k = d

    def k1_callback(self, p):
        print("k1 pressed")
        self.bgm.toggle_pause()

    async def opening(self):
        setcolor(color_cfg.snake)
        for j in range(board_cfg.height):
            for i in range(board_cfg.width):
                if j % 2 == 1:
                    x = 29 - i
                else:
                    x = i
                await asyncio.sleep_ms(5)
                outtextxy(x, j, tile_cfg.block)
        setcolor(color_cfg.background)
        for j in range(board_cfg.height):
            for i in range(board_cfg.width):
                if j % 2 == 1:
                    x = 29 - i
                else:
                    x = i
                await asyncio.sleep_ms(5)
                outtextxy(x, j, tile_cfg.block)

    def closing(self):
        self.state = snake.STOP
        setcolor(color_cfg.magenta)
        outtextxy(0, 29, "GAME OVER")
    
    async def blink(self):
        self.led.value(1)
        await asyncio.sleep_ms(50)
        self.led.value(0)
        await asyncio.sleep_ms(50)
        
    async def process(self):
        bgm = asyncio.create_task(self.bgm_process())

        await self.opening()

        self.init_run()
        
        # main loop
        while True:
            self.dir_select()
            self.run()
            self.draw()
            self.judge()
            await self.blink()

    def dir_select(self):
        key = self.getch()
        
        if key == "cw":
            if self.d == snake.RIGHT:
                self.d = snake.DOWN
            elif self.d == snake.DOWN:
                self.d = snake.LEFT
            elif self.d == snake.LEFT:
                self.d = snake.UP
            elif self.d == snake.UP:
                self.d = snake.RIGHT
        if key == "ccw":
            if self.d == snake.RIGHT:
                self.d = snake.UP
            elif self.d == snake.UP:
                self.d = snake.LEFT
            elif self.d == snake.LEFT:
                self.d = snake.DOWN
            elif self.d == snake.DOWN:
                self.d = snake.RIGHT

    def init_run(self):
        self.body = []

        for i in range(self.n):
            self.body.append(location((self.n - 1 - i), 0))

        self.head = location(self.n, 0)
        self.tail = location(-1, 0)

        self.food = location(-1, -1)

        setcolor(color_cfg.snake)
        outtextxy(0, 0, (self.n + 1) * tile_cfg.block)

        self.d = self.RIGHT

        self.fod()
        
    def run(self):
        if self.state != snake.RUNNING:
            return
        
        self.tail.x = self.body[self.n - 1].x
        self.tail.y = self.body[self.n - 1].y

        for i in range(self.n - 1, 0, -1):
            self.body[i].x = self.body[i - 1].x
            self.body[i].y = self.body[i - 1].y

        self.body[0].x = self.head.x
        self.body[0].y = self.head.y

        if self.d == snake.UP:
            self.head.y -= 1
        elif self.d == snake.DOWN:
            self.head.y += 1
        elif self.d == snake.LEFT:
            self.head.x -= 1
        elif self.d == snake.RIGHT:
            self.head.x += 1

        if self.head.x == -1:
            self.head.x = board_cfg.width - 1
        elif self.head.x == board_cfg.width:
            self.head.x = 0
            
        if self.head.y == -1:
            self.head.y = board_cfg.height - 1
        elif self.head.y == board_cfg.height:
            self.head.y = 0    
            
    def draw(self):
        setcolor(color_cfg.background)
        outtextxy(self.tail.x, self.tail.y, tile_cfg.block)
        setcolor(color_cfg.snake)
        outtextxy(self.head.x, self.head.y, tile_cfg.block)
        
    def judge(self):
        if self.head.x == self.food.x and self.head.y == self.food.y:
            self.body.append(location(-1, -1))
            self.n += 1
            self.fod()
            # add score
            self.score += 100
            setcolor(color_cfg.food)
            outtextxy(board_cfg.dashboard_x + 20, board_cfg.dashboard_y, "SCORE: " + str(self.score))

        for i in range(self.n):
            if self.head.x == self.body[i].x and self.head.y == self.body[i].y:
                self.closing()
        
    def fod(self):
        self.food.x = random.randrange(0, board_cfg.width)
        self.food.y = random.randrange(0, board_cfg.height)
        setcolor(color_cfg.food)
        outtextxy(self.food.x, self.food.y, tile_cfg.block)

    def getch(self):
        if self.haskey:
            self.haskey = False
            return self.k
        else:
            return None

    async def bgm_process(self):
        while True:
            await asyncio.sleep_ms(100)
            self.bgm.tick()

class p_music(music):
    song0 = '0 A5 2 26 0.6299212574958801;2 B5 2 26 0.6299212574958801;4 C6 6 26 0.6299212574958801;10 B5 2 26 0.6299212574958801;12 C6 4 26 0.6299212574958801;16 E6 4 26 0.6299212574958801;20 B5 8 26 0.6299212574958801;32 E5 4 26 0.6299212574958801;36 A5 6 26 0.6299212574958801;42 G5 2 26 0.6299212574958801;44 A5 4 26 0.6299212574958801;48 C6 4 26 0.6299212574958801;52 G5 8 26 0.6299212574958801;64 F5 2 26 0.6299212574958801;66 E5 2 26 0.6299212574958801;68 F5 6 26 0.6299212574958801;74 E5 2 26 0.6299212574958801;76 F5 4 26 0.6299212574958801;80 C6 4 26 0.6299212574958801;84 E5 8 26 0.6299212574958801;94 C6 2 26 0.6299212574958801;96 C6 2 26 0.6299212574958801;98 C6 2 26 0.6299212574958801;100 B5 6 26 0.6299212574958801;106 F#5 2 26 0.6299212574958801;108 F#5 4 26 0.6299212574958801;112 B5 4 26 0.6299212574958801;116 B5 8 26 0.6299212574958801;128 A5 2 26 0.6299212574958801;130 B5 2 26 0.6299212574958801;132 C6 6 26 0.6299212574958801;138 B5 2 26 0.6299212574958801;140 C6 4 26 0.6299212574958801;144 E6 4 26 0.6299212574958801;148 B5 8 26 0.6299212574958801;160 E5 2 26 0.6299212574958801;162 E5 2 26 0.6299212574958801;164 A5 6 26 0.6299212574958801;170 G5 2 26 0.6299212574958801;172 A5 4 26 0.6299212574958801;176 C6 4 26 0.6299212574958801;180 G5 8 26 0.6299212574958801;192 E5 4 26 0.6299212574958801;196 F5 4 26 0.6299212574958801;200 C6 2 26 0.6299212574958801;202 B5 2 26 0.6299212574958801;204 B5 4 26 0.6299212574958801;208 C6 4 26 0.6299212574958801;212 D6 4 26 0.6299212574958801;216 E6 2 26 0.6299212574958801;218 C6 2 26 0.6299212574958801;220 C6 8 26 0.6299212574958801;228 C6 2 26 0.6299212574958801;230 B5 2 26 0.6299212574958801;232 A5 4 26 0.6299212574958801;236 B5 4 26 0.6299212574958801;240 G#5 4 26 0.6299212574958801;244 A5 11 26 0.6299212574958801;256 C6 2 26 0.6299212574958801;258 D6 2 26 0.6299212574958801;260 E6 6 26 0.6299212574958801;266 D6 2 26 0.6299212574958801;268 E6 4 26 0.6299212574958801;272 G6 4 26 0.6299212574958801;276 D6 8 26 0.6299212574958801;288 G5 2 26 0.6299212574958801;290 G5 2 26 0.6299212574958801;292 C6 6 26 0.6299212574958801;298 B5 2 26 0.6299212574958801;300 C6 4 26 0.6299212574958801;304 E6 4 26 0.6299212574958801;308 E6 11 26 0.6299212574958801;324 A5 2 26 0.6299212574958801;326 B5 2 26 0.6299212574958801;328 C6 4 26 0.6299212574958801;332 B5 2 26 0.6299212574958801;334 C6 2 26 0.6299212574958801;336 D6 4 26 0.6299212574958801;340 C6 6 26 0.6299212574958801;346 G5 2 26 0.6299212574958801;348 G5 8 26 0.6299212574958801;356 F6 4 26 0.6299212574958801;360 E6 4 26 0.6299212574958801;364 D6 4 26 0.6299212574958801;368 C6 4 26 0.6299212574958801;372 E6 15 26 0.6299212574958801;388 E6 11 26 0.6299212574958801;400 E6 4 26 0.6299212574958801;404 A6 8 26 0.6299212574958801;412 G6 8 26 0.6299212574958801;420 E6 4 26 0.6299212574958801;424 D6 2 26 0.6299212574958801;426 C6 2 26 0.6299212574958801;428 C6 8 26 0.6299212574958801;436 D6 4 26 0.6299212574958801;440 C6 2 26 0.6299212574958801;442 D6 2 26 0.6299212574958801;444 D6 4 26 0.6299212574958801;448 G6 4 26 0.6299212574958801;452 E6 11 26 0.6299212574958801;464 E6 4 26 0.6299212574958801;468 A6 8 26 0.6299212574958801;476 G6 8 26 0.6299212574958801;484 E6 4 26 0.6299212574958801;488 D6 2 26 0.6299212574958801;490 C6 2 26 0.6299212574958801;492 C6 8 26 0.6299212574958801;500 D6 4 26 0.6299212574958801;504 C6 2 26 0.6299212574958801;506 D6 2 26 0.6299212574958801;508 D6 4 26 0.6299212574958801;512 B5 4 26 0.6299212574958801;516 A5 11 26 0.6299212574958801;528 A5 2 26 0.6299212574958801;530 B5 2 26 0.6299212574958801;532 C6 6 26 0.6299212574958801;538 B5 2 26 0.6299212574958801;540 C6 4 26 0.6299212574958801;544 E6 4 26 0.6299212574958801;548 B5 8 26 0.6299212574958801;560 E5 4 26 0.6299212574958801;564 A5 6 26 0.6299212574958801;570 G5 2 26 0.6299212574958801;572 A5 4 26 0.6299212574958801;576 C6 4 26 0.6299212574958801;580 G5 8 26 0.6299212574958801;592 F5 2 26 0.6299212574958801;594 E5 2 26 0.6299212574958801;596 F5 6 26 0.6299212574958801;602 E5 2 26 0.6299212574958801;604 F5 4 26 0.6299212574958801;608 C6 4 26 0.6299212574958801;612 E5 8 26 0.6299212574958801;622 C6 2 26 0.6299212574958801;624 C6 2 26 0.6299212574958801;626 C6 2 26 0.6299212574958801;628 B5 6 26 0.6299212574958801;634 F#5 2 26 0.6299212574958801;636 F#5 4 26 0.6299212574958801;640 B5 4 26 0.6299212574958801;644 B5 8 26 0.6299212574958801;656 A5 2 26 0.6299212574958801;658 B5 2 26 0.6299212574958801;660 C6 6 26 0.6299212574958801;666 B5 2 26 0.6299212574958801;668 C6 4 26 0.6299212574958801;672 E6 4 26 0.6299212574958801;676 B5 8 26 0.6299212574958801;688 E5 2 26 0.6299212574958801;690 E5 2 26 0.6299212574958801;692 A5 6 26 0.6299212574958801;698 G5 2 26 0.6299212574958801;700 A5 4 26 0.6299212574958801;704 C6 4 26 0.6299212574958801;708 G5 8 26 0.6299212574958801;720 E5 4 26 0.6299212574958801;724 F5 4 26 0.6299212574958801;728 C6 2 26 0.6299212574958801;730 B5 2 26 0.6299212574958801;732 B5 4 26 0.6299212574958801;736 C6 4 26 0.6299212574958801;740 D6 4 26 0.6299212574958801;744 E6 2 26 0.6299212574958801;746 C6 2 26 0.6299212574958801;748 C6 8 26 0.6299212574958801;756 C6 2 26 0.6299212574958801;758 B5 2 26 0.6299212574958801;760 A5 4 26 0.6299212574958801;764 B5 4 26 0.6299212574958801;768 G#5 4 26 0.6299212574958801;772 A5 11 26 0.6299212574958801'
    def __init__(self, songString='0 D4 8 0', looping=True, tempo=3, duty=2512, pin=None, pins=[Pin(0)]):
        super().__init__(songString, looping, tempo, duty, pin, pins)
        self.pause = True

    def toggle_pause(self):
        self.pause = not self.pause
        if self.pause:
            print("mute\n")
            self.mute()
        else:
            print("unmute\n")
            self.unmute()
    
    def mute(self):
        for pwm in self.pwms:
            pwm.duty_u16(0)
    def unmute(self):
        for pwm in self.pwms:
            pwm.duty_u16(self.duty)

    def tick(self):
        if self.pause:
            pass
        else:
            super().tick()

def main():
    hardware.init()
    s = snake()
    asyncio.run(s.process())

main()

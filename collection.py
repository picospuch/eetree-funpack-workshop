import cv2
import time
camera = cv2.VideoCapture(0)
camera.set(3, 320)  # width=320
camera.set(4, 240)  # height=320

path = "train/0613/b"
ext=".jpg"
cnt=0

def control_white_led(value):
    open('/sys/class/gpio/export', 'w').write('0') # Export the GPIO0
    open('/sys/class/gpio/gpio0/direction', 'w').write('out') # Set the direction of the GPIO
    open('/sys/class/gpio/gpio0/value', 'w').write(str(value)) # Set the calute, '1' or '0'

try:
    while True:
        ret, frame = camera.read()
        if ret:
            cnt+=1
            fname=path+str(cnt)+ext
            #led blink
            control_white_led(0)
            print(fname+" is saved")
            cv2.imwrite(fname,frame)
            time.sleep(0.2)
            control_white_led(1)
            #time.sleep(2.8)
        else:
            print('OOps, we get some trouble!')
            time.sleep(2.8)

except KeyboardInterrupt:
    pass

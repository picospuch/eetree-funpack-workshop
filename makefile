cdc = /dev/cu.usbmodem141101
objects = $(shell find . -depth 1 -name "*.py" \! -name ".*")
m ?= main.py

## run on board
run : $(m)
	cp -f $< main.py
	pyboard --device $(cdc) main.py
#	ampy -p $(cdc) run main.py

## flash to board
build : reset ot $(objects:%=ot/%)
	@echo "built all"

ot :
	mkdir -p ot

ot/%.py : %.py
	cp $< $@
	rshell -p $(cdc) cp $@ /pyboard

## others
.PHONY : reset
reset :
	ampy -p $(cdc) reset

clean :
	rm -r ot || true
	rshell -p $(cdc) rm -rf /pyboard

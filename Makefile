# ca65 -g -l clock.lst clock.s
# ld65 -t none -vm -m clock.map  -o clock.bin clock.o

# papertape
# srec_cat -line-length 41 clock.bin -Binary -offset 0x2000 -o clock.ptp -MOS_Technologies

PROGRAM = adventure
CC65_HOME = /Users/ryan.roth/Downloads/cc65

ifdef CC65_HOME
  AS = $(CC65_HOME)/bin/ca65
  CC = $(CC65_HOME)/bin/cc65
  CL = $(CC65_HOME)/bin/cl65
  LD = $(CC65_HOME)/bin/ld65
else
  AS := $(if $(wildcard ../../bin/ca65*),../../bin/ca65,ca65)
  CC := $(if $(wildcard ../../bin/cc65*),../../bin/cc65,cc65)
  CL := $(if $(wildcard ../../bin/cl65*),../../bin/cl65,cl65)
  LD := $(if $(wildcard ../../bin/ld65*),../../bin/ld65,ld65)
endif

all: $(PROGRAM).ptp

$(PROGRAM).ptp: $(PROGRAM).bin Makefile
	srec_cat $(PROGRAM).bin -binary -offset 0x0000 -o $(PROGRAM).ptp -MOS_Technologies

$(PROGRAM).bin: $(PROGRAM).o
	ld65 --cfg-path /usr/local/Cellar/cc65/2.19/share/cc65/cfg -C kim1-60k.cfg -vm -m $(PROGRAM).map -o $(PROGRAM).bin $(PROGRAM).o

$(PROGRAM).o:	$(PROGRAM).s
	ca65 -g -l $(PROGRAM).lst $(PROGRAM).s

clean:
	$(RM) *.o *.lst *.map *.bin *.ptp

flash:
	minipro -p AT28C64B -w $(PROGRAM).bin -s

distclean: clean

text:
	$(RM) -r data/*
	python3 export.py

fat.bin: main.c
	$(CL) -t kim1 -C kim1-60k.cfg -Oi -o fat.bin main.c

fat.ptp: fat.bin
	srec_cat fat.bin -binary -offset 0x0000 -o fat.ptp -MOS_Technologies

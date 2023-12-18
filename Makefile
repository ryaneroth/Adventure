# ca65 -g -l clock.lst clock.s
# ld65 -t none -vm -m clock.map  -o clock.bin clock.o

# papertape 
# srec_cat -line-length 41 clock.bin -Binary -offset 0x2000 -o clock.ptp -MOS_Technologies

PROGRAM = adventure

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
	minipro -p AT28C256 -w $(PROGRAM).bin -s

distclean: clean

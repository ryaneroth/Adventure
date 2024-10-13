PORTA = $1700
DDRA = $1701
PORTB = $1702
DDRB = $1703

PRTBYT = $1E3B

SD_CS   = %00010000
SD_SCK  = %00001000
SD_MOSI = %00000100
SD_MISO = %00000010

PORTA_OUTPUTPINS = SD_CS | SD_SCK | SD_MOSI

via_init:
  lda #%11111111          ; Set all pins on port B to output
  sta DDRB
  lda #PORTA_OUTPUTPINS   ; Set various pins on port A to output
  sta DDRA
  rts

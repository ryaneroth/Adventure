EXIT  = $1c4f
OUTCH = $1ea0
GETCH = $1e5a
PRINT_BUFFER = $00
CPBUF = $02
WORDS = $04
COUNT = $06
SPACE = $08
LIST_ADDRESS = $0a
LOCATION = $0c
LOCATION_INDEX = $0e
JUMP_ADDRESS = $10
ITEM_ADDRESS = $12

ITEMS_INDEX = $a0       ; Inventory
ITEMS = $a1             ; Inventory
INBUF = $e0             ; TODO    Store towards the end of zero page for now, move closer 
                        ; TODO    to other addresses once code is all done
;------------------------------
; Macros
;------------------------------
.macro print text
  ldx #<text            ; String address LSB
  ldy #>text            ; String address MSB
  stx PRINT_BUFFER             ; Zero page pointer LSB
  sty PRINT_BUFFER+1           ; Zero page pointer MSB
  jsr print_string
.endmacro

.macro bootloader_print text
  ldx #<text            ; String address LSB
  ldy #>text            ; String address MSB
  stx PRINT_BUFFER             ; Zero page pointer LSB
  sty PRINT_BUFFER+1           ; Zero page pointer MSB
  jsr bootloader_print_string
.endmacro


.macro compareinput input
  ldx #<input           ; String address LSB
  ldy #>input           ; String address MSB
  stx CPBUF             ; Zero page pointer LSB
  sty CPBUF+1           ; Zero page pointer MSB
  jsr compareinput_string
.endmacro

.org $a000
;------------------------------
; Bootloader
;------------------------------
startcopy:
  bootloader_print copying_started
;------------------------------
; Relocate Code
;------------------------------
lda #$a8           ;set our source memory address to copy from, $a000
sta $FB
lda #$a0
sta $FC
lda #$00           ;set our destination memory to copy to, $2000
sta $FD
lda #$20
sta $FE
ldy #$00           ;reset x and y for our loop
ldx #$00

copy_loop:
  lda ($FB),Y      ;indirect index source memory address, starting at $00
  sta ($FD),Y      ;indirect index dest memory address, starting at $00
  iny
  bne copy_loop    ;loop until our dest goes over 255

  inc $FC          ;increment high order source memory address, starting at $80
  inc $FE          ;increment high order dest memory address, starting at $60
  lda $FE          ;load high order mem address into a
  cmp #$35         ;compare with the last address we want to write TODO dial in
  bne copy_loop    ;if we're not there yet, loop

stopcopy:
  bootloader_print copying_complete
jmp init
;------------------------------
; Bootloader specific functions
;------------------------------
bootloader_print_string:
  lda #$0D                                ; CR
  jsr OUTCH                               ; Send a carriage retuen  
  lda #$0A                                ; LF
  jsr OUTCH                               ; Send the line feed
  ldx #0    
bootloader_print_string_loop:
  txa
  tay
  lda (PRINT_BUFFER), y                   ; get from string
  beq bootloader_print_string_exit        ; end of string
  jsr OUTCH                               ; write to output
  inx
  bne bootloader_print_string_loop        ; do next char
bootloader_print_string_exit:
  rts
;------------------------------
; Bootloader specific data
;------------------------------
copying_started:
  .asciiz "Copying program to RAM..."

copying_complete:
  .asciiz "Copy complete. Swtich ROM to upper 16k and press ENTER"
;------------------------------
; Game entrypoint
;------------------------------
.org $2000

init:
  lda #0
  sta ITEMS_INDEX
intro:
  jsr newline
  jsr GETCH
  jsr clearscreen
  print instructions0
intro_input:
  jsr getinput
  cmp #1
  beq instructions
  cmp #2
  beq location1
  print please_answer_yes_or_no
  jmp intro_input
instructions:
  print instructions1
  print instructions2
  jsr newline
;-----------------------------
; Load starting location
;-----------------------------
location1:
  ldx #<data1
  ldy #>data1
  stx LOCATION                ; The first data byte in our location
  stx PRINT_BUFFER            ; is the description we want to print
  sty LOCATION+1
  sty PRINT_BUFFER+1
;-----------------------------
; Location loop
;-----------------------------
location_loop:
  jsr print_string
  stx LOCATION_INDEX
  jsr list_location_items
location_getinput:
  jsr getinput
  ldy LOCATION_INDEX
location_parseinput:
  iny
  cmp (LOCATION), y
  beq location_jump
  cmp #$00                    ; Check for unknown word in the A register
  beq unknown_word
  pha
  lda (LOCATION), y
  cmp #$fe                    ; Compare location index to delimeter
  beq unknown_direction
  pla
  iny
  iny
  jmp location_parseinput
location_jump:
  iny
  lda (LOCATION), y
  sta JUMP_ADDRESS
  iny
  lda (LOCATION), y
  sta JUMP_ADDRESS+1
  ; Swap addresses
  lda JUMP_ADDRESS
  sta LOCATION                ; The first data byte in our location
  sta PRINT_BUFFER            ; is the description we want to print
  ; Swap addresses
  lda JUMP_ADDRESS+1
  sta LOCATION+1
  sta PRINT_BUFFER+1
  jmp location_loop
unknown_word:
  print i_dont_know_that_word
  jmp location_getinput
unknown_direction:
  print there_is_no_way_to_go_that_direction
  jmp location_getinput
;-----------------------------
; Location items loop
;-----------------------------
list_location_items:
  ldx #0
  ldx #<itemlist
  ldy #>itemlist
  stx ITEM_ADDRESS
  sty ITEM_ADDRESS+1
list_next_item:
  ldy #0
  lda (ITEM_ADDRESS), y
  cmp #$ff
  beq list_location_exit
  cmp #$01                    ; Check if item has been picked up already
  beq check_item_location
  jmp increase_item_index
check_item_location:
  inc ITEM_ADDRESS
  ldy #0
  lda (ITEM_ADDRESS), y
  cmp LOCATION                ; Check if item is in our location
  beq print_item
increase_item_index:
  ldy #0
  lda (ITEM_ADDRESS), y
  inc ITEM_ADDRESS
  cmp #$fe
  beq list_next_item
  jmp increase_item_index
print_item:
  inc ITEM_ADDRESS
  inc ITEM_ADDRESS
  lda ITEM_ADDRESS
  sta PRINT_BUFFER
  lda ITEM_ADDRESS+1
  sta PRINT_BUFFER+1
  jsr print_string
  jmp list_next_item
list_location_exit:
  rts
;-----------------------------
; Get user input
;-----------------------------
getinput:
  jsr newline
  lda #'>'
  jsr OUTCH
  lda #' '
  jsr OUTCH
  ldx #0
getinput_next:
  jsr GETCH
  sta INBUF, x
  inx
  cmp #$0D                   ; Enter key
  bne getinput_next
  jsr newline
  jsr newline
;-----------------------
; Parse string for match TODO debug leaving house in here
;-----------------------
  ldx #<actions              ; Action list address LSB
  ldy #>actions              ; Action list  address MSB
  stx LIST_ADDRESS                  ; Zero page pointer LSB
  sty LIST_ADDRESS+1                ; Zero page pointer MSB
  lda #0
  sta WORDS                  ; Set words value to zero
  sta SPACE                  ; Set detected space to 0
  ldx #0                     ; Get the first letter to narrow down our search
  stx COUNT                  ; Store X counter
  ldy #0
get_input_key:
  lda INBUF, x
  cmp #$20                   ; Compare to space character
  bne compare_action
  lda SPACE
  cmp #0
  bne space_exists
  stx SPACE                  ; Store first space postions
space_exists:
  iny                        ; Increment y to get string value
  lda (LIST_ADDRESS), y      ; Get string value
  clc                        ; No incoming carry; make sure carry is clear
  adc WORDS
  sta WORDS
  inx
  stx COUNT                  ; Store X counter
  ldy #0
  jmp get_input_key
compare_action:
  cmp (LIST_ADDRESS), y
  bne not_equal
  cmp #$0D                   ; Compare to end of string character
  beq endchar
next_char:
  inx
  jmp loop
not_equal:
  ldx COUNT                  ; Load X counter
loop:
  iny
  lda (LIST_ADDRESS), y
  cmp #$ff                   ; Compare to end of list character
  beq endchar
  jmp get_input_key
endchar:
  iny
  lda (LIST_ADDRESS), y
  cmp #255
  bne getinput_exit
  lda WORDS
  cmp #0
  bne get_items
getinput_exit:
  clc                        ; No incoming carry; make sure carry is clear
  adc WORDS                  ; Add string value with words value
  sta WORDS
  ; cmp #30
  ; beq print_inventory
  ; cmp #31
  ; beq print_help
  rts
get_items:
  ldx #<items                ; Items list address LSB
  ldy #>items                ; Items list address MSB
  stx LIST_ADDRESS                  ; Zero page pointer LSB
  sty LIST_ADDRESS+1                ; Zero page pointer MSB
  ldx SPACE
  inx
  ldy #0
  jmp get_input_key

print_help:
  print help1
  print help2
  print help3
  print help4
  print help5
  jmp getinput

print_inventory:
  ldy #0
print_inventory_loop:
  ldx ITEMS, y 
  stx PRINT_BUFFER
  iny
  ldx ITEMS, y
  stx PRINT_BUFFER + 1
  iny
  tya
  pha
  jsr print_string
  pla
  tay
  cpy ITEMS_INDEX
  bne print_inventory_loop
print_inventory_exit:
  jsr newline
  jmp getinput

print_string:
  ldx #0
print_string_loop:
  txa
  tay
  lda (PRINT_BUFFER), y        ; get from string
  beq print_string_exit        ; end of string
  jsr OUTCH                    ; write to output
  inx
  bne print_string_loop        ; do next char
print_string_exit:
  jsr newline
  rts

compareinput_string:
  ldx #0
  ldy #0
compareinput_string_next:
  lda (CPBUF), y
  cmp INBUF, x
  bne compareinput_string_false
  inx
  iny
  lda INBUF, x
  cmp #$0D
  bne compareinput_string_next
  lda #1
  jmp compareinput_string_true
compareinput_string_false:
  lda #0
compareinput_string_true:
  rts

newline:
  lda #$0D                   ; CR
  jsr OUTCH                  ; Send a carriage retuen  
  lda #$0A                   ; LF
  jsr OUTCH                  ; Send the line feed
  rts

clearscreen:
  ldx #$19                   ; Load X - we're going tp print 25 lines
  lda #$0D                   ; CR
  jsr OUTCH                  ; Send a carriage retuen
clearscreen_loop:    
  lda #$0A                   ; LF
  jsr OUTCH                  ; Send the line feed
  dex                        ; One less to do
  bne clearscreen_loop       ; Go send another untill we're done
  rts                        ; Return

store_item:
  ldx ITEMS_INDEX
  sta ITEMS, x
  inx
  sty ITEMS, x
  inx
  stx ITEMS_INDEX
  rts

quit:
  jsr EXIT                   ; Return to the KIM-1 monitor

.include "items.inc"
.include "help.inc"
.include "instructions.inc"
.include "words.inc"
.include "commands.inc"
.include "padding.inc"
;-------------------------------------
; This will be address 4000 in the ROM
;-------------------------------------
.org $a000
.include "locations.inc"
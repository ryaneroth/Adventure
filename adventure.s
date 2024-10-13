EXIT  = $1c4f
OUTCH = $1ea0
GETCH = $1e5a
CURRENT_DIRECTORY_NAME = $1000                            ; 12 bytes
INPUT_BUFFER = $100D                                      ; 8 bytes


.org $a000
  jsr newline
  jsr initiallize_fat16
  jsr read_master_boot_record
  jsr read_fat_boot_record
  jsr read_file_allocation_table
  jsr EXIT












































; display_welcome_message:
;   lda #'1'
;   jsr print_char
;   jsr fat32_openroot
;   lda #'2'
;   jsr print_char
;   ldx #<messagesdirname
;   ldy #>messagesdirname
;   jsr finddirent
;   lda #'3'
;   jsr print_char
;   ldx #<instructions_prompt
;   ldy #>instructions_prompt
;   ; jsr load_file
;   jsr print_file
;   jsr get_input
; ; ; TODO Y, N, YES, NO
; ;   ldx #<instructions
; ;   ldy #>instructions
; ;   jsr load_file
; ;   jsr print_file

; _initlocation:
;   ldx #0
; _initlocationloop:
;   lda subdirname,x
;   sta CURRENT_DIRECTORY_NAME,x
;   inx
;   cpx #12
;   bne _initlocationloop
; _initlocationdone:
;   jsr newline

; load_current_location:
;   jsr open_location_directory
;   ldx #<location
;   ldy #>location
; ;  jsr load_file
;   jsr print_file

;   jsr get_input
;   jsr newline

;   jsr open_location_directory
;   ldx #<traveltable
;   ldy #>traveltable
; ;  jsr load_file

;   ldx #255
;   ldy #255
; tt_loop:
;   inx
;   iny
;   lda INPUT_BUFFER,y
;   cmp #$20
;   beq tt_loop_space
;   cmp buffer,x
;   bne tt_loop_next_line
;   jmp tt_loop
; tt_loop_space:
;   lda buffer,x
;   cmp #$20
;   beq tt_loop_match
;   jmp tt_loop
; tt_loop_next_line:
;   ldy #0
;   lda buffer,x
;   inx
;   cmp #$0A                                 ; advance to the next entry
;   beq tt_loop
;   jmp tt_loop_next_line
; tt_loop_match:
;   inx
;   ldy #0
; tt_loop_update_location:
;   lda buffer,x
;   cmp #$0D                                 ; done update location
;   beq tt_loop_end
;   sta CURRENT_DIRECTORY_NAME,y
;   inx
;   iny
;   jmp tt_loop_update_location
; tt_loop_end:
;   jmp load_current_location

; _exit:
;   jsr EXIT

; get_input:
;   jsr newline
;   lda #'>'
;   jsr print_char
;   lda #' '
;   jsr print_char

;   ldx #0
; getcharacter:
;   jsr get_char
;   sta INPUT_BUFFER,x
;   inx
;   cmp #$0D                   ; Enter key
;   bne getcharacter           ; Keep getting characters until we see enter
;   dex
;   lda #$20                   ; Replace CR with space
;   sta INPUT_BUFFER,x
;   jsr newline
;   rts

; ; directory names
; subdirname:
;   .asciiz "1          "

; messagesdirname:
;   .asciiz "MESSAGES   "

; ; generic text filenames
; location:
;   .asciiz "LOCATIONTXT"

; short_description:
;   .asciiz "SHORTDESTXT"

; traveltable:
;   .asciiz "TVLTABLETXT"

; ; message filenames
; instructions:
;   .asciiz "1       TXT"

; instructions_prompt:
;   .asciiz "65      TXT"



; .include "file_operations.s"
.include "libfat16.s"
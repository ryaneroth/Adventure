; Save A, X, and Y during destructive operations
SAVE_A = $100               ; 1 bytes
SAVE_X = $101               ; 1 bytes
SAVE_Y = $102               ; 1 byte


print_char:
; JSR OUTCH    1EA0     Print ASCII char  A     -       Xis preserved
;                       in A on TTY                     Y = FF
;                                                       A = FF
  sta SAVE_A
  sty SAVE_Y
  jsr OUTCH
  ldy SAVE_Y
  lda SAVE_A
  rts

print_hex:
; JSR PRTBYT   1E3B     Prints A as       A     -       A preserved
;                       2 Hex Char.                     X preserved
;                                                       Y = FF
  sty SAVE_Y
  jsr PRTBYT
  ldy SAVE_Y
  rts

get_char:
; JSR GETCH    1E5A     Put character     -     A       X preserved
;                       from TTY in A                   Y = FF
  sty SAVE_Y
  jsr GETCH
  ldy SAVE_A
  rts

newline:
  lda #$0D                        ; CR
  jsr print_char                  ; Send a carriage retuen
  lda #$0A                        ; LF
  jsr print_char                  ; Send the line feed
  rts

clearscreen:
  ldx #0
clearscreenloop:
  jsr newline
  inx
  cpx #25
  bne clearscreenloop
  rts
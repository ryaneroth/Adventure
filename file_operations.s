zp_sd_address = $40         ; 2 bytes
zp_sd_currentsector = $42   ; 4 bytes
zp_fat32_variables = $46    ; 49 bytes

fat32_workspace = $200      ; two pages

buffer = $400

init_filesystem:
  ldx #$ff
  txs
  ; Initialise
  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc _initsuccess
  ; Error during FAT32 initialization
  lda #'Z'
  jsr print_char
  lda fat32_errorstage
  jsr print_hex
  jmp EXIT
_initsuccess:
  rts

open_file:
  ; Find file by name
  jsr fat32_finddirent
  bcc _foundfile

  ; File not found
  jsr newline
  lda #'F'
  jsr print_char
  lda #'N'
  jsr print_char
  lda #'F'
  jsr print_char
  jmp EXIT

_foundfile:
  ; Open file
  jsr fat32_opendirent
  ; Read file contents into buffer
  lda #<buffer
  sta fat32_address
  lda #>buffer
  sta fat32_address+1
  rts


;;;;;;;;;;; Everythign above is new

open_location_directory:
  jsr fat32_openroot                      ; open root directory
  ; Find subdirectory by name
  ldx #<CURRENT_DIRECTORY_NAME
  ldy #>CURRENT_DIRECTORY_NAME

finddirent:
  jsr fat32_finddirent
  bcc _foundsubdir

  ; Subdirectory not found
  jsr newline
  lda #'S'
  jsr print_char
  lda #'D'
  jsr print_char
  lda #'N'
  jsr print_char
  lda #'F'
  jsr print_char
  jmp EXIT

_foundsubdir:
  ; Open subdirectory
  jsr fat32_opendirent
  rts


  ; Dump data to terminal
print_file:
  ldx #0
_printloop:
  lda buffer,x
  cmp #0
  beq _end_of_file
  jsr print_char
  inx
  bne _printloop
_end_of_file:
  rts


  .include "hwconfig.s"
  .include "libsd.s"
  .include "libio.s"
  .include "libfat32.s"

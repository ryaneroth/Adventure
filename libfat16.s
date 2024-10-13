; Boot sector    2048 = 0x100000/512
; FAT Tables:
;     2048 + 0x0400 (0x04 sectors)
; Primary FAT    2052 = 0x100800/512
; Secondary FAT  2152 = 0x10D000/512
; Root Directory:
;     2048 + 0x0400 + 0x64 (100) x 2
;     size: max entries (32) x 512
; Root directory 2252
; Data Area:
;     2048 + 0x0400 + 0x64 (100) x 2 + ((512 * 32) / 512)
; Data area      2284
CURRENT_BUFFER_LOCATION = $00                             ; 2 bytes
SECTOR_INDEX = $02                                        ; 4 bytes
FIRST_PARTITION_STATE = $06                               ; 1 byte
FIRST_PARTITION_BEGINNING_HEAD = $07                      ; 1 byte
FIRST_PARTITION_BEGINNING_SECTOR = $08                    ; 2 bytes
FIRST_PARTITION_TYPE = $0A                                ; 1 byte
FIRST_PARTITION_ENDING_HEAD = $0B                         ; 1 byte
FIRST_PARTITION_ENDING_SECTOR = $0C                       ; 2 bytes
FIRST_PARTITION_SECTOR_OFFSET = $0D                       ; 4 bytes
FIRST_PARTITION_TOTAL_SECTORS = $11                       ; 4 bytes
FAT16_BYTES_PER_SECTOR = $15                              ; 2 bytes
FAT16_BYTES_SECTORS_PER_CLUSTER = $17                     ; 1 byte
FAT16_RESERVED_SECTORS = $18                              ; 2 bytes
FAT16_FAT_COPIES = $1A                                    ; 1 byte
FAT16_MAXIMUM_ROOT_DIRECTORY_ENTRIES = $1B                ; 2 bytes
FAT16_NUMBER_OF_SECTORS_LESS_THAN_32M = $1D               ; 2 bytes
FAT16_MEDIA_DESCRIPTOR = $1F                              ; 1 byte
FAT16_SECTORS_PER_FAT = $20                               ; 2 bytes
FAT16_SECTORS_PER_TRACK = $22                             ; 2 bytes
FAT16_NUBMER_OF_HEADS = $24                               ; 2 bytes
FAT16_NUBMER_OF_HIDDEN_SECTORS = $26                      ; 4 bytes
FAT16_NUBMER_OF_SECTORS = $2A                             ; 4 bytes
FAT16_LOGICAL_DRIVE_NUMBER_OF_PARTITION = $2E             ; 2 bytes
FAT16_EXTENDED_SIGNATURE = $30                            ; 1 byte
FAT16_SERIAL_NUMBER = $31                                 ; 4 bytes
FAT16_VOLUME_NAME = $35                                   ; 11 bytes
FAT16_FAT_NAME = $40                                      ; 8 bytes
; SD card constants
zp_sd_address = $48                                       ; 2 bytes
zp_sd_currentsector = $4A                                 ; 4 bytes

BUFFER = $2000


initiallize_fat16:
  ; Initialise SD card
  jsr via_init
  jsr sd_init
  ; Target buffer
  lda #<BUFFER
  sta zp_sd_address
  lda #>BUFFER
  sta zp_sd_address+1
  rts

read_master_boot_record:
  ; Load a pointer to the data buffer
  lda #<BUFFER
  sta CURRENT_BUFFER_LOCATION
  lda #>BUFFER
  sta CURRENT_BUFFER_LOCATION+1
  ; Set current sector
  lda #0
  sta zp_sd_currentsector
  sta zp_sd_currentsector+1
  sta zp_sd_currentsector+2
  sta zp_sd_currentsector+3
  ; Read MBR table
  jsr sd_readsector
  ; Skip the first 446 bytes containing Executable Code
  ldy #190
  inc CURRENT_BUFFER_LOCATION+1
  ; 1st Partition Entry
  ; Current State of Partition (00h=Inactive, 80h=Active)
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_STATE
  iny
  ; Beginning of Partition - Head
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_BEGINNING_HEAD
  iny
  ; Beginning of Partition - Cylinder/Sector TODO bitwise operations
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_BEGINNING_SECTOR
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_BEGINNING_SECTOR+1
  iny
  ; Partition type
  ; 04 FAT16 <32M
  ; 06 FAT16 (currently the only tested partitioin type)
  ; 14 Hidden FAT16 <32
  ; 16 Hidden FAT16
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_TYPE
  iny
  ; End of Partition - Head
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_ENDING_HEAD
  iny
  ; End of Partition - Cyl/Sec TODO bitwise math
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_ENDING_SECTOR
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_ENDING_SECTOR+1
  iny
  ; Number of Sectors Between the MBR and the First Sector in the Partition (reverse bits gives you 2048)
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_SECTOR_OFFSET
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_SECTOR_OFFSET+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_SECTOR_OFFSET+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_SECTOR_OFFSET+3
  iny
  jsr newline
  ; Number of Sectors in the Partition
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_TOTAL_SECTORS
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_TOTAL_SECTORS+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_TOTAL_SECTORS+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FIRST_PARTITION_TOTAL_SECTORS+3
  rts

read_fat_boot_record:
  ; Load a pointer to the data buffer
  lda #<BUFFER
  sta CURRENT_BUFFER_LOCATION
  lda #>BUFFER
  sta CURRENT_BUFFER_LOCATION+1
  ; Read FAT boot record
  ; 2048 00000000 00000000 00001000 00000000 / 0x00 0x08 0x00 0x00 - little indian
  ; Set current sector
  lda FIRST_PARTITION_SECTOR_OFFSET
  sta zp_sd_currentsector
  lda FIRST_PARTITION_SECTOR_OFFSET+1
  sta zp_sd_currentsector+1
  lda FIRST_PARTITION_SECTOR_OFFSET+2
  sta zp_sd_currentsector+2
  lda FIRST_PARTITION_SECTOR_OFFSET+3
  sta zp_sd_currentsector+3
  jsr sd_readsector
  ; FAT bytes per sector
  ldy #11
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_BYTES_PER_SECTOR
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_BYTES_PER_SECTOR+1
  iny
  ; FAT bytes per cluster
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_BYTES_SECTORS_PER_CLUSTER
  iny
  ; FAT reserved sectors
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_RESERVED_SECTORS
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_RESERVED_SECTORS+1
  iny
  ; FAT copies
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_COPIES
  iny
  ; Maximum Root Directory Entries
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_MAXIMUM_ROOT_DIRECTORY_ENTRIES
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_MAXIMUM_ROOT_DIRECTORY_ENTRIES+1
  iny
  ; Number of Sectors in Partition Smaller than 32MB
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUMBER_OF_SECTORS_LESS_THAN_32M
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUMBER_OF_SECTORS_LESS_THAN_32M+1
  iny
  ; Media Descriptor (F8h for Hard Disks)
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_MEDIA_DESCRIPTOR
  iny
  ; Sectors per fat
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SECTORS_PER_FAT
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SECTORS_PER_FAT+1
  iny
  ; Sectors Per Track
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SECTORS_PER_TRACK
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SECTORS_PER_TRACK+1
  iny
  ; Number of Heads
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HEADS
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HEADS+1
  iny
  ; Number of Hidden Sectors in Partition
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HIDDEN_SECTORS
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HIDDEN_SECTORS+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HIDDEN_SECTORS+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_HIDDEN_SECTORS+3
  iny
  ; Number of Sectors in Partition
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_SECTORS
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_SECTORS+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_SECTORS+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_NUBMER_OF_SECTORS+3
  iny
  ; Logical Drive Number of Partition
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_LOGICAL_DRIVE_NUMBER_OF_PARTITION
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_LOGICAL_DRIVE_NUMBER_OF_PARTITION+1
  iny
  ; Extended Signature (29h)
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_EXTENDED_SIGNATURE
  iny
  ; Serial Number of Partition
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SERIAL_NUMBER
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SERIAL_NUMBER+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SERIAL_NUMBER+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_SERIAL_NUMBER+3
  iny
  ; Volume Name
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+3
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+4
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+5
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+6
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+7
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+8
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+9
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_VOLUME_NAME+10
  iny
  ; FAT Name (FAT16)
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+1
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+2
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+3
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+4
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+5
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+6
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  sta FAT16_FAT_NAME+7
  iny
  rts

; TODO add support for second FAT table
read_file_allocation_table:
  ; Start (2048) + # of Reserved Sectors (4)
  ; primary fat    2052 = 0x100800/512
  ; secondary fat  2152 = 0x10D000/512
  ; 100 fat tables
  ; primary fat    2052 = 0x100800/512
  ; 00000000 00000000 00001000 00000100
  lda FIRST_PARTITION_SECTOR_OFFSET
  adc FAT16_RESERVED_SECTORS
  sta zp_sd_currentsector
  lda FIRST_PARTITION_SECTOR_OFFSET+1
  adc FAT16_RESERVED_SECTORS+1
  sta zp_sd_currentsector+1
  lda FIRST_PARTITION_SECTOR_OFFSET+2
  sta zp_sd_currentsector+2
  lda FIRST_PARTITION_SECTOR_OFFSET+3
  sta zp_sd_currentsector+3
_fat_loop:
  ; Load a pointer to the data buffer
  lda #<BUFFER
  sta CURRENT_BUFFER_LOCATION
  lda #>BUFFER
  sta CURRENT_BUFFER_LOCATION+1
  jsr sd_readsector
  jsr newline
  lda SECTOR_INDEX
  jsr print_hex
  jsr newline
  ldy #0
  ldx #0
print_sector_loop2:
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  cpy #255
  bne print_sector_loop2
  ldy #0
  inc CURRENT_BUFFER_LOCATION+1
  inx
  cpx #2
  bne print_sector_loop2
  jsr inc_zp_sd_currentsector
  inc SECTOR_INDEX
  lda SECTOR_INDEX
  cmp #100
  bne _fat_loop
  rts


read_root_directoy:
  ; Root Directory:
  ;     2048 + 0x0400 + 0x64 (100) x 2
  ;     size: max entries (32) x 512
  lda #0
  sta SECTOR_INDEX
  ; root dir    2252
  ; 00000000 00000000 00001000 11001100
  lda #204
  sta zp_sd_currentsector
  lda #8
  sta zp_sd_currentsector+1
  lda #0
  sta zp_sd_currentsector+2
  sta zp_sd_currentsector+3
_root_dir_loop:
  lda #<BUFFER
  sta CURRENT_BUFFER_LOCATION
  lda #>BUFFER
  sta CURRENT_BUFFER_LOCATION+1
  ; Read MBR table
  jsr sd_readsector
  jsr newline
  lda SECTOR_INDEX
  jsr print_hex
  jsr newline
  ldy #0
  ldx #0
print_sector_loop3:
  lda #'"'
  jsr print_char
  ; Short file name, len 11
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_char
  iny
  lda #'"'
  jsr print_char
  ; FFWD to end of entry
  tya
  adc #15
  tay
  lda #'"'
  jsr print_char
  ; sector and size
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
    lda #'"'
  jsr print_char
    lda #'"'
  jsr print_char
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  lda (CURRENT_BUFFER_LOCATION),y
  jsr print_hex
  iny
  lda #'"'
  jsr print_char
  cpy #255
  iny
  beq inc_y
  jmp print_sector_loop3
inc_y:
  ldy #0
  inc CURRENT_BUFFER_LOCATION+1
  inx
  cpx #2
  beq next_sector
  jmp print_sector_loop3
next_sector:
  jsr inc_zp_sd_currentsector
  inc SECTOR_INDEX
  lda SECTOR_INDEX
  cmp #32
  beq exit_loop
  jmp _root_dir_loop
exit_loop:
  rts


inc_zp_sd_currentsector:
  inc zp_sd_currentsector
  bne inc_zp_sd_currentsector_exit
  inc zp_sd_currentsector+1
  bne inc_zp_sd_currentsector_exit
  inc zp_sd_currentsector+2
  bne inc_zp_sd_currentsector_exit
  inc zp_sd_currentsector+3
inc_zp_sd_currentsector_exit:
  rts



  .include "hwconfig.s"
  .include "libsd.s"
  .include "libio.s"
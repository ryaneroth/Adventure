zp_sd_address = $40         ; 2 bytes
zp_sd_currentsector = $42   ; 4 bytes
zp_fat32_variables = $46    ; 49 bytes

fat32_workspace = $200      ; two pages

target_room = $700          ; 1 byte
target_verb = $701          ; 1 byte
input_seen = $702           ; 1 byte

field_lo = $703             ; 2 bytes
field_hi = $704
dest_lo = $705              ; 2 bytes
dest_hi = $706
verb_lo = $707              ; 2 bytes
verb_hi = $708
cond_lo = $709              ; 2 bytes
cond_hi = $70A

tmp_digit = $70B            ; 1 byte
old_lo = $70C               ; 2 bytes
old_hi = $70D
mul_lo = $70E               ; 2 bytes
mul_hi = $70F

work_lo = $710              ; 2 bytes
work_hi = $711
hundreds = $712             ; 1 byte
tens = $713                 ; 1 byte
line_idx = $714             ; 1 byte
pct_roll = $715             ; 1 byte (0..99)
carried_obj = $716          ; 1 byte carried object id entry input
present_obj = $717          ; 1 byte present object id entry input
cond_group = $718           ; 1 byte rcond/100
cond_object = $719          ; 1 byte rcond%100
prop_obj_id = $71A          ; 1 byte object id for prop override
prop_obj_state = $71B       ; 1 byte prop state override
entry_count = $71C          ; 1 byte number of entries to read
entries_left = $71D         ; 1 byte loop counter
room_in_section = $71E      ; 1 byte room text section state
room_line_start = $71F      ; 1 byte room text line-start flag

line_buf = $720             ; 80 bytes
prop_table = $780           ; 100-byte prop[] table for object ids 0..99
toting_table = $7E4         ; 100-byte toting[] table for object ids 0..99
present_table = $848        ; 100-byte atloc[] table for object ids 0..99
room_marker_num = $8AC      ; 1 byte parsed room marker number
room_char = $8AD            ; 1 byte room text char buffer
visited_table = $900        ; 200-byte room visited flags
place_table = $2000         ; 100-byte place[] table for object ids 0..99
cmd_len = $770              ; 1 byte command token length
cmd_idx = $771              ; 1 byte command parser index
cmd_buf = $720              ; 16-byte command token buffer (known-good RAM)
noun_len = $772             ; 1 byte noun token length
command_type = $773         ; 0=motion,1=take,2=drop,3=inventory,4=look,5=quit,6=examine,7=read,8=parse_error,9=openish,10=closeish,11=help,12=light,13=dark,14=eat,15=drink,16=fill,17=pour,18=wave,19=find,20=say,21=kill,22=throw,23=feed,24=break,25=rub,26=wake,27=blast,28=score,29=calm,30=walk,31=nothing,32=brief,33=suspend,34=hours,35=log,36=load,37=foo
command_object = $774       ; object id for action command
parse_error = $775          ; 0=ok,1=unknown word,2=unknown object,3=missing object
desc_mode = $776            ; 0=long room text, 1=short room text
msg_target = $777           ; 1 byte target message id for ADVENT4
prl_guard_lo = $778         ; 2-byte read guard for print_room_long
prl_guard_hi = $779
rng_state = $77A            ; 1-byte RNG state
num_deaths = $77B           ; 1 byte: times player has died (0..3)
gave_up    = $77C           ; 1 byte: 1 if player used QUIT, 0 if natural end
dflag      = $77D           ; 1 byte: 1 once player reaches deep cave (room>=15)
tally      = $77E           ; 1 byte: treasures not yet returned to building
closing    = $77F           ; 1 byte: 1 if cave-closing sequence has triggered
closed     = $8AE           ; 1 byte: 1 once player is teleported to the repository
bonus      = $8AF           ; 1 byte: 0=no blast; 133=45pts, 134=30pts, 135=25pts
lamp_life_lo = $8B0         ; 2 bytes: turns of lamp fuel remaining
lamp_life_hi = $8B1
lamp_warned  = $8B2         ; 1 byte: 1 once "lamp getting dim" warning shown
ndwarves     = $8B3         ; 1 byte: active dwarf count (0-5; init'd when dflag fires)
dfirst       = $8B4         ; 1 byte: 1 once first-encounter scene has played
pflag        = $8B5         ; 1 byte: 1 after pirate has stolen (won't steal again)
noun_buf = $730             ; 16-byte noun token buffer

snapshot_base = $700        ; save/load snapshot base address
snapshot_buf = $2200        ; packed snapshot staging buffer
snapshot_size_lo = $28      ; packed snapshot size = $0328 bytes
snapshot_size_hi = $03
snapshot_block1_base = $700
snapshot_block1_len_lo = $C8 ; $02C8 bytes ($700..$9C7)
snapshot_block1_len_hi = $02
snapshot_block2_base = $2000
snapshot_block2_len_lo = $64 ; $0064 bytes (place_table)
snapshot_block2_len_hi = $00

  .org $A000
reset:
  ldx #$ff
  txs

  jsr via_init
  jsr sd_init
  jsr fat32_init
  bcc _initsuccess

  lda #'Z'
  jsr print_char
  lda fat32_errorstage
  jsr print_hex
  jmp halt_loop

_initsuccess:
  lda #$5D
  sta rng_state

  jsr newline
  ldx #<msg_welcome_1
  ldy #>msg_welcome_1
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_2
  ldy #>msg_welcome_2
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_3
  ldy #>msg_welcome_3
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_4
  ldy #>msg_welcome_4
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_5
  ldy #>msg_welcome_5
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_6
  ldy #>msg_welcome_6
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_welcome_7
  ldy #>msg_welcome_7
  jsr print_zstr_xy
  jsr newline
  jsr newline
  ldx #<msg_ask_instructions
  ldy #>msg_ask_instructions
  jsr print_zstr_xy
  jsr newline
  jsr newline
  jsr prompt_yes_no
  php
  jsr newline
  plp
  bcc :+
  ldx #<msg_intro_help_1
  ldy #>msg_intro_help_1
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_2
  ldy #>msg_intro_help_2
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_3
  ldy #>msg_intro_help_3
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_4
  ldy #>msg_intro_help_4
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_5
  ldy #>msg_intro_help_5
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_6
  ldy #>msg_intro_help_6
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_intro_help_7
  ldy #>msg_intro_help_7
  jsr print_zstr_xy
  jsr newline
  jsr newline
:

  jsr init_toting_table
  jsr init_present_table
  jsr init_prop_table
  jsr init_place_table
  jsr init_visited_table

  lda #0
  sta num_deaths
  sta gave_up
  sta dflag
  sta closing
  lda #15
  sta tally
  lda #0
  sta closed
  sta bonus
  lda #$4A                 ; lamp_life = 330 ($014A)
  sta lamp_life_lo
  lda #$01
  sta lamp_life_hi
  lda #0
  sta lamp_warned
  sta ndwarves
  sta dfirst
  sta pflag

  lda #1
  sta target_room
  lda #0
  sta input_seen
  jsr newline
  lda #0
  sta desc_mode
  jsr print_room_long
  jsr print_room_objects
  ldx target_room
  lda #1
  sta visited_table,x
  jsr newline

turn_loop:
  ; Guard against cumulative stack drift across long play sessions.
  ldx #$ff
  txs
  jsr per_turn_updates     ; lamp fuel, dwarf AI

  jsr sync_present_from_room
  jsr read_verb_id
  lda command_type
  beq _turn_motion
  jsr execute_action_command
  jmp turn_loop

_turn_motion:
  jsr read_pct_roll

  jsr fat32_openroot
  ldx #<filename
  ldy #>filename
  jsr fat32_finddirent
  bcc _found_file

  lda #'F'
  jsr print_char
  lda #'N'
  jsr print_char
  lda #'F'
  jsr print_char
  jsr newline
  jmp turn_loop

_found_file:
  jsr fat32_opendirent

_find_room:
  jsr read_line
  bcc _fr_got
  jmp _room_not_found
_fr_got:
  jsr find_hash_in_line
  bcc _fr_parse
  jmp _find_room
_fr_parse:
  jsr parse_u16_field
  lda field_hi
  bne _find_room
  lda field_lo
  cmp target_room
  bne _find_room
  lda #0
  sta input_seen           ; dotrav hitflag

_scan_room:
  jsr read_line
  bcc _sr_got
  jmp _no_move
_sr_got:
  jsr find_hash_in_line
  bcs _sr_not_marker
  jmp _no_move
_sr_not_marker:

  ldy #0
  jsr parse_u16_field
  lda field_lo
  sta dest_lo
  lda field_hi
  sta dest_hi

  jsr parse_u16_field
  lda field_lo
  sta verb_lo
  lda field_hi
  sta verb_hi

  jsr parse_u16_field
  lda field_lo
  sta cond_lo
  lda field_hi
  sta cond_hi

  ; dotrav verb gating:
  ; before first hit, require rverb==1 or rverb==motion.
  ; after first hit, all following entries are candidates.
  lda input_seen
  bne _sr_have_hitflag
  lda verb_hi
  bne _scan_room
  lda verb_lo
  cmp #1
  beq _sr_set_hitflag
  cmp target_verb
  bne _scan_room
_sr_set_hitflag:
  lda #1
  sta input_seen
_sr_have_hitflag:

  ; Evaluate rcond.
  jsr cond_matches
  bcc _scan_room

  ; Handle special destinations without changing room:
  ;   >500: message id (dest-500), print from ADVENT4
  ;   >300: scaffold path (dest-300)
  lda dest_hi
  bne :+
  jmp _sr_dest_room_check
:
  cmp #1
  bne _sr_dest_over300
  lda dest_lo
  cmp #$2D                ; 301 decimal
  bcs :+
  jmp _sr_dest_room_check
:

_sr_dest_over300:
  ; Check >500 (501+)
  lda dest_hi
  cmp #1
  bcc _sr_dest_spc300
  bne _sr_dest_msg500
  lda dest_lo
  cmp #$F5                ; 501 decimal
  bcc _sr_dest_spc300

_sr_dest_msg500:
  sec
  lda dest_lo
  sbc #$F4
  sta work_lo
  lda dest_hi
  sbc #$01
  sta work_hi
  lda work_hi
  bne _sr_msg500_fallback
  lda work_lo
  sta msg_target
  jsr newline
  jsr print_special_message
  bcs _sr_msg500_fallback
  jmp _sr_dest_ok
_sr_msg500_fallback:
  jsr newline
  lda #'M'
  jsr print_char
  lda #'='
  jsr print_char
  lda #' '
  jsr print_char
  jsr print_u16_dec
  jmp _sr_dest_ok

_sr_dest_spc300:
  sec
  lda dest_lo
  sbc #$2C                ; 300 decimal
  sta work_lo
  lda dest_hi
  sbc #$01
  sta work_hi
  lda work_hi
  beq :+
  jmp _sr_spc_fallback
:
  lda work_lo
  cmp #1
  beq _sr_spc1_plover
  cmp #2
  beq _sr_spc2_drop_emerald
  cmp #3
  beq _sr_spc3_troll
  jmp _sr_spc_fallback

_sr_spc1_plover:
  jsr count_holding
  lda entry_count
  beq _sr_spc1_move
  cmp #1
  bne _sr_spc1_blocked
  ldx #59                  ; EMERALD
  lda toting_table,x
  bne _sr_spc1_move
_sr_spc1_blocked:
  lda #117
  sta msg_target
  jsr newline
  jsr print_special_message
  bcs :+
  jmp _sr_dest_ok
:
  jmp _sr_spc_fallback
_sr_spc1_move:
  lda #199
  sec
  sbc target_room
  sta dest_lo
  lda #0
  sta dest_hi
  jmp _sr_dest_room_check

_sr_spc2_drop_emerald:
  ldx #59                  ; EMERALD
  lda toting_table,x
  bne :+
  jmp _sr_dest_ok
:
  lda #0
  sta toting_table,x
  sta present_table,x
  lda target_room
  sta place_table,x
  jmp _sr_dest_ok

_sr_spc3_troll:
  ldx #33                  ; TROLL
  lda prop_table,x
  beq _sr_spc3_troll_block  ; prop=0: troll refuses to let you cross
  cmp #2
  beq _sr_spc3_bridge_gone  ; prop=2: bridge destroyed
  ; prop=1: troll was bribed; withdraws as player crosses
  lda #0
  sta prop_table,x
  ldx #34                  ; TROLL2
  lda #0
  sta toting_table,x
  sta present_table,x
  sta place_table,x
  ldx #33                  ; TROLL
  lda #117
  sta place_table,x
  jmp _sr_spc3_move         ; allow crossing

_sr_spc3_troll_block:
  lda #160
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jmp _sr_spc_fallback

_sr_spc3_bridge_gone:
  lda #161
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jmp _sr_spc_fallback

_sr_spc3_move:
  ; Normal crossing toggles side 117<->122.
  lda target_room
  cmp #117
  bne _sr_spc3_chk122
  lda #122
  sta dest_lo
  lda #0
  sta dest_hi
  jmp _sr_spc3_have_dest
_sr_spc3_chk122:
  lda target_room
  cmp #122
  bne _sr_spc_fallback
  lda #117
  sta dest_lo
  lda #0
  sta dest_hi
_sr_spc3_have_dest:
_sr_spc3_check_bear:
  ; Carrying the bear collapses bridge parity state.
  ldx #35                  ; BEAR
  lda toting_table,x
  beq _sr_spc3_finish_move
  ; Bear lumbers at troll — troll flees (msg 163)
  lda #163
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: ; Bridge collapses under the bear's weight (msg 162)
  lda #162
  sta msg_target
  jsr newline
  jsr print_special_message
  bcs _sr_spc_fallback

  ldx #32                  ; CHASM
  lda #1
  sta prop_table,x
  ldx #33                  ; TROLL
  lda #2
  sta prop_table,x
  ldx #35                  ; BEAR
  lda #0
  sta toting_table,x
  sta present_table,x
  lda dest_lo
  sta place_table,x
  lda #3
  sta prop_table,x
  jmp _sr_dest_ok
_sr_spc3_finish_move:
  jmp _sr_dest_room_check

_sr_spc_fallback:
  jmp _no_move

_sr_dest_ok:
  jsr newline
  jmp turn_loop

_sr_dest_room_check:
  ; If destination is a normal room id (1..255), show long description.
  lda dest_hi
  bne _skip_dest_room_text
  lda dest_lo
  beq _skip_dest_room_text
  sta target_room
  ; Set dflag once player reaches the deep cave (room >= 15).
  lda dflag
  bne _sr_dflag_done
  lda target_room
  cmp #15
  bcc _sr_dflag_done
  lda #1
  sta dflag
  lda #5
  sta ndwarves             ; 5 dwarves become active on first deep-cave entry
_sr_dflag_done:
  jsr sync_present_from_room
  lda target_room
  tax
  lda visited_table,x
  beq _sr_first_visit
  lda #1
  sta desc_mode
  jmp _sr_print_desc
_sr_first_visit:
  lda #0
  sta desc_mode
_sr_print_desc:
  jsr newline
  ; Dark room check: cave rooms (>=9) are dark unless lamp (obj 2) is on.
  ldx #2
  lda prop_table,x
  bne _sr_lit              ; lamp on
  lda target_room
  cmp #9
  bcc _sr_lit              ; rooms 1-8 are surface/lit
  lda #16                  ; "It is now pitch dark..."
  sta msg_target
  jsr print_special_message
  bcs _sr_desc_done
  jsr newline
  jmp _sr_desc_done
_sr_lit:
  jsr print_room_long
  jsr print_room_objects
_sr_desc_done:
  ldx target_room
  lda #1
  sta visited_table,x
_skip_dest_room_text:

  jsr newline
  lda #'O'
  jsr print_char
  lda #'K'
  jsr print_char
  jsr newline
  jmp turn_loop

_room_not_found:
  lda #'R'
  jsr print_char
  lda #'N'
  jsr print_char
  lda #'F'
  jsr print_char
  jsr newline
  jmp turn_loop

_no_move:
  ; Parity: from below the grate, ENTER with locked grate should
  ; report the locked-grate text instead of generic NM.
  lda target_room
  cmp #9
  bne _no_move_check_look
  lda target_verb
  cmp #3
  bne _no_move_check_look
  ldx #3
  lda prop_table,x
  bne _no_move_check_look
  jsr newline
  ldx #<msg_locked_grate
  ldy #>msg_locked_grate
  jsr print_zstr_xy
  jsr newline
  jsr newline
  lda #'O'
  jsr print_char
  lda #'K'
  jsr print_char
  jsr newline
  jmp turn_loop
_no_move_check_look:
  jsr motion_no_move_is_look
  bcc _no_move_nm
  jsr sync_present_from_room
  lda #0
  sta desc_mode
  jsr newline
  ldx #2
  lda prop_table,x
  bne _no_move_look_lit
  lda target_room
  cmp #9
  bcc _no_move_look_lit
  lda #16
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jmp turn_loop
_no_move_look_lit:
  jsr print_room_long
  jsr print_room_objects
  jsr newline
  jmp turn_loop
_no_move_nm:
  jsr select_badmove_message
  jsr newline
  jsr print_special_message
  bcc :+
  lda #'N'
  jsr print_char
  lda #'M'
  jsr print_char
:
  jsr newline
  jmp turn_loop

motion_no_move_is_look:
  ; Treat "go to where I already am" as LOOK for core place words.
  lda target_room
  cmp #3
  bne _mnmil_check_depression
  lda target_verb
  cmp #12
  beq _mnmil_yes
  jmp _mnmil_no
_mnmil_check_depression:
  lda target_room
  cmp #8
  bne _mnmil_no
  lda target_verb
  cmp #63
  beq _mnmil_yes
_mnmil_no:
  clc
  rts
_mnmil_yes:
  sec
  rts

get_input_filtered:
  ; Filter idle NUL returns from monitor GETCH and throttle polling
  ; so prompts remain readable instead of flooding with NUL bytes.
_gif_loop:
  jsr get_input
  and #$7F
  bne _gif_done
  ldx #$40
_gif_delay:
  dex
  bne _gif_delay
  jmp _gif_loop
_gif_done:
  rts

select_badmove_message:
  ; Approximate Adventure badmove() messaging by motion id.
  lda #12
  sta msg_target

  lda target_verb
  cmp #43
  bcc _sbm_chk29
  cmp #51
  bcs _sbm_chk29
  lda #9
  sta msg_target
_sbm_chk29:
  lda target_verb
  cmp #29
  beq _sbm_set9
  cmp #30
  bne _sbm_chk7
_sbm_set9:
  lda #9
  sta msg_target
_sbm_chk7:
  lda target_verb
  cmp #7
  beq _sbm_set10
  cmp #36
  beq _sbm_set10
  cmp #37
  bne _sbm_chk11
_sbm_set10:
  lda #10
  sta msg_target
_sbm_chk11:
  lda target_verb
  cmp #11
  beq _sbm_set11
  cmp #19
  bne _sbm_chk62
_sbm_set11:
  lda #11
  sta msg_target
_sbm_chk62:
  lda target_verb
  cmp #62
  beq _sbm_set42
  cmp #65
  bne _sbm_chk17
_sbm_set42:
  lda #42
  sta msg_target
_sbm_chk17:
  lda target_verb
  cmp #17
  bne _sbm_done
  lda #80
  sta msg_target
_sbm_done:
  rts

halt_loop:
  jsr EXIT
  jmp halt_loop

read_room_id:
  lda #1
  sta target_room
  lda #0
  sta input_seen

  lda #'R'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rr_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rr_eol
  cmp #$0A
  beq _rr_eol
  cmp #'0'
  bcc _rr_loop
  cmp #('9'+1)
  bcs _rr_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rr_accum
  lda #0
  sta target_room
_rr_accum:
  lda #1
  sta input_seen

  lda target_room
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta target_room
  jmp _rr_loop

_rr_eol:
  lda input_seen
  beq _rr_loop

_rr_done:
  jsr newline
  lda target_room
  bne _rr_ok
  lda #1
  sta target_room
_rr_ok:
  rts

read_verb_id:
  ; Read command line with up to two tokens: VERB [NOUN].
  lda #1
  sta target_verb
  lda #0
  sta cmd_len
  sta noun_len
  sta command_type
  sta command_object
  sta parse_error
  sta cmd_idx              ; saw-command-char flag
  sta input_seen            ; 0=first token, 1=second token, 2=ignore rest

  lda #'>'
  jsr print_char
  lda #' '
  jsr print_char

_rv_loop:
  jsr get_input_filtered
  cmp #$08
  bne :+
  jmp _rv_backspace
:
  cmp #$7F
  bne :+
  jmp _rv_backspace
:
  cmp #$0D
  bne _rv_chk_lf
  jmp _rv_eol
_rv_chk_lf:
  cmp #$0A
  bne _rv_after_eol_checks
  jmp _rv_eol
_rv_after_eol_checks:

  ; Token separators: space/tab
  cmp #' '
  beq _rv_space
  cmp #$09
  beq _rv_space

  ; Convert lowercase to uppercase
  cmp #'a'
  bcc _rv_store
  cmp #('z'+1)
  bcs _rv_store
  and #$DF

_rv_store:
  ; Ignore non-alphanumeric bytes (e.g. polling noise / NUL).
  cmp #'0'
  bcc _rv_loop
  cmp #('9'+1)
  bcc _rv_store_ok
  cmp #'A'
  bcc _rv_loop
  cmp #('Z'+1)
  bcs _rv_loop

_rv_store_ok:
  sta tmp_digit
  lda input_seen
  cmp #1
  beq _rv_store_noun
  cmp #0
  bne _rv_loop

  ldx cmd_len
  cpx #15
  bcs _rv_loop
  lda tmp_digit
  sta cmd_buf,x
  inx
  stx cmd_len
  lda #1
  sta cmd_idx
  jmp _rv_loop

_rv_store_noun:
  ldx noun_len
  cpx #15
  bcs _rv_loop
  lda tmp_digit
  sta noun_buf,x
  inx
  stx noun_len
  lda #1
  sta cmd_idx
  jmp _rv_loop

_rv_space:
  lda input_seen
  cmp #0
  bne _rv_space_second
  lda cmd_len
  bne _rv_space_set_second
  jmp _rv_loop
_rv_space_set_second:
  lda #1
  sta input_seen
  jmp _rv_loop

_rv_space_second:
  cmp #1
  bne _rv_space_second_not1
  lda noun_len
  bne _rv_space_second_setdone
  jmp _rv_loop
_rv_space_second_setdone:
  jsr noun_is_filler_word
  bcc _rv_space_second_check_desc
  lda #0
  sta noun_len
  jmp _rv_loop
_rv_space_second_check_desc:
  jsr noun_is_descriptor_word
  bcc _rv_space_second_freeze
  lda #0
  sta noun_len
  jmp _rv_loop
_rv_space_second_freeze:
  lda #2
  sta input_seen
  jmp _rv_loop
_rv_space_second_not1:
  jmp _rv_loop

_rv_backspace:
  lda input_seen
  cmp #2
  bne _rv_bs_check_second
  lda #1
  sta input_seen
_rv_bs_check_second:
  lda input_seen
  cmp #1
  bne _rv_bs_first
  lda noun_len
  beq _rv_bs_unspace
  dec noun_len
  jmp _rv_loop
_rv_bs_unspace:
  lda #0
  sta input_seen
  jmp _rv_loop
_rv_bs_first:
  lda cmd_len
  bne :+
  jmp _rv_loop
:
  dec cmd_len
  jmp _rv_loop

_rv_eol:
  ; If the noun token ended as a filler word (e.g. "GO TO"),
  ; drop it before dispatch so command-specific defaults apply.
  lda input_seen
  cmp #1
  bne :+
  lda noun_len
  beq :+
  jsr noun_is_filler_word
  bcc :+
  lda #0
  sta noun_len
:
  lda cmd_idx
  bne _rv_done
  jmp _rv_loop

_rv_done:
  ldx cmd_len
  lda #0
  sta cmd_buf,x
  ldx noun_len
  sta noun_buf,x

  lda cmd_len
  bne _rv_has_token
  jmp _rv_emit_done
_rv_has_token:

  ; Numeric fallback
  ldx #0
  lda cmd_buf,x
  cmp #'0'
  bcc _rv_try_word
  cmp #('9'+1)
  bcs _rv_try_word
  lda #0
  sta target_verb
  stx cmd_idx
_rv_num_loop:
  ldx cmd_idx
  cpx cmd_len
  bcc :+
  jmp _rv_emit_done
:
  lda cmd_buf,x
  cmp #'0'
  bcs :+
  jmp _rv_emit_done
:
  cmp #('9'+1)
  bcc :+
  jmp _rv_emit_done
:
  and #$0F
  sta tmp_digit
  lda target_verb
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta target_verb
  inc cmd_idx
  jmp _rv_num_loop

_rv_try_word:
  jsr decode_action_word
  lda command_type
  bne :+
  jmp _rv_try_motion
:
  cmp #3
  bne :+
  jmp _rv_emit_done
:
  cmp #5
  bne :+
  jmp _rv_emit_done
:
  cmp #4
  bne :+
  jmp _rv_look_object
:
  cmp #12
  bne :+
  jmp _rv_normalize_light
:
  cmp #13
  bne :+
  jmp _rv_normalize_light
:
  cmp #20
  bne :+
  jmp _rv_emit_done
:
  lda command_type
  cmp #27
  bcc :+
  cmp #38
  bcs :+
  jmp _rv_emit_done
:
  lda command_type
  cmp #1
  beq _rv_check_all
  cmp #2
  bne _rv_need_obj
_rv_check_all:
  lda noun_len
  bne :+
  jmp _rv_need_obj
:
  jsr noun_is_all
  bcc _rv_need_obj
  jmp _rv_emit_done
_rv_need_obj:
  lda noun_len
  bne :+
  jmp _rv_need_obj_no_noun
:
  jmp _rv_have_noun
_rv_need_obj_no_noun:
  lda command_type
  cmp #9
  bne :+
  jmp _rv_default_grate
:
  cmp #10
  bne :+
  jmp _rv_default_grate
:
  cmp #12
  bne :+
  jmp _rv_default_lamp
:
  cmp #13
  bne :+
  jmp _rv_default_lamp
:
  cmp #14
  bne :+
  jmp _rv_default_food
:
  cmp #15
  bne :+
  jmp _rv_default_bottle
:
  cmp #16
  bne :+
  jmp _rv_default_bottle
:
  cmp #17
  bne :+
  jmp _rv_default_bottle
:
  cmp #18
  bne :+
  jmp _rv_default_rod
:
  cmp #11
  bne :+
  jmp _rv_emit_done
:
  cmp #21
  bne :+
  jmp _rv_emit_done
:
  cmp #26
  bne :+
  jmp _rv_emit_done
:
  lda command_type
  cmp #27
  bcc :+
  cmp #38
  bcs :+
  jmp _rv_emit_done
:
  lda command_type
  cmp #2
  bne _rv_missing_object_jump
  lda cmd_len
  cmp #5
  bcc _rv_missing_object_jump
  lda cmd_buf
  cmp #'L'
  bne _rv_missing_object_jump
  lda cmd_buf+1
  cmp #'E'
  bne _rv_missing_object_jump
  lda cmd_buf+2
  cmp #'A'
  bne _rv_missing_object_jump
  lda cmd_buf+3
  cmp #'V'
  bne _rv_missing_object_jump
  lda cmd_buf+4
  cmp #'E'
  bne _rv_missing_object_jump
  lda #0
  sta command_type
  lda #11
  sta target_verb
  jmp _rv_emit_done
_rv_default_grate:
  lda #3
  sta command_object
  jmp _rv_emit_done
_rv_default_lamp:
  lda #2
  sta command_object
  jmp _rv_emit_done
_rv_default_food:
  lda #19
  sta command_object
  jmp _rv_emit_done
_rv_default_bottle:
  lda #20
  sta command_object
  jmp _rv_emit_done
_rv_default_rod:
  lda #5
  sta command_object
  jmp _rv_emit_done
_rv_missing_object_jump:
  jmp _rv_missing_object
_rv_have_noun:
  jsr decode_object_word
  lda command_object
  beq :+
  jmp _rv_emit_done
:
  lda #2
  sta parse_error
  lda #8
  sta command_type
  jmp _rv_emit_done

_rv_look_object:
  lda noun_len
  bne :+
  jmp _rv_emit_done
:
  jsr decode_object_word
  lda command_object
  bne :+
  lda #2
  sta parse_error
  lda #8
  sta command_type
  jmp _rv_emit_done
:
  lda #6
  sta command_type
  jmp _rv_emit_done

_rv_try_motion:
  jsr decode_motion_word
  lda target_verb
  cmp #1
  bne _rv_emit_done
  lda noun_len
  beq _rv_motion_unknown
  jsr verb_uses_noun_motion
  bcc _rv_motion_unknown
  ldx #0
_rv_motion_copy:
  cpx noun_len
  bcs _rv_motion_copied
  lda noun_buf,x
  sta cmd_buf,x
  inx
  jmp _rv_motion_copy
_rv_motion_copied:
  lda #0
  sta cmd_buf,x
  lda noun_len
  sta cmd_len
  jsr decode_motion_word
  lda target_verb
  cmp #1
  bne _rv_emit_done
_rv_motion_unknown:
  ; For movement verbs like "GO" with no target, report missing object.
  ; Non-movement unknown words should remain unknown-word.
  lda noun_len
  bne _rv_motion_unknown_have_noun
  jsr verb_uses_noun_motion
  bcs _rv_motion_missing_object
  jmp _rv_motion_unknown_word

_rv_motion_unknown_have_noun:
  ; Treat filler-only GO target like "GO TO" as missing object.
  jsr verb_uses_noun_motion
  bcc _rv_motion_unknown_word
  lda noun_len
  cmp #2
  bne _rv_motion_unknown_word
  lda noun_buf
  cmp #'T'
  bne _rv_motion_unknown_word
  lda noun_buf+1
  cmp #'O'
  bne _rv_motion_unknown_word
_rv_motion_missing_object:
  lda #3
  sta parse_error
  lda #8
  sta command_type
  jmp _rv_emit_done
_rv_motion_unknown_word:
  lda #1
  sta parse_error
  lda #8
  sta command_type
  jmp _rv_emit_done

_rv_missing_object:
  lda #3
  sta parse_error
  lda #8
  sta command_type

_rv_emit_done:
  jsr newline
  rts

_rv_normalize_light:
  ; Accept "TURN ON/OFF" and "SWITCH ON/OFF" style input where noun token
  ; is ON/OFF rather than an object; normalize to lamp command.
  lda noun_len
  beq _rv_norm_light_done
  jsr noun_is_onoff_word
  bcc _rv_norm_light_done
  jsr noun_is_off_word
  bcc :+
  lda #13
  sta command_type
  jmp _rv_norm_light_set_lamp
:
  lda #12
  sta command_type
_rv_norm_light_set_lamp:
  lda #2
  sta command_object
  lda #0
  sta noun_len
_rv_norm_light_done:
  jmp _rv_check_all

noun_is_onoff_word:
  jsr noun_is_on_word
  bcs _nio_yes
  jsr noun_is_off_word
  bcs _nio_yes
  clc
  rts
_nio_yes:
  sec
  rts

noun_is_on_word:
  lda noun_len
  cmp #2
  bne _niow_no
  lda noun_buf
  cmp #'O'
  bne _niow_no
  lda noun_buf+1
  cmp #'N'
  bne _niow_no
  sec
  rts
_niow_no:
  clc
  rts

noun_is_off_word:
  lda noun_len
  cmp #3
  bne _niof_no
  lda noun_buf
  cmp #'O'
  bne _niof_no
  lda noun_buf+1
  cmp #'F'
  bne _niof_no
  lda noun_buf+2
  cmp #'F'
  bne _niof_no
  sec
  rts
_niof_no:
  clc
  rts

decode_action_word:
  ; Writes command_type:
  ; 1=TAKE, 2=DROP, 3=INVENTORY, 4=LOOK, 5=QUIT, 6=EXAMINE, 7=READ
  ; 9=openish,10=closeish,11=help,12=light,13=dark,14=eat,15=drink,16=fill,17=pour,18=wave,19=find
  ; 20=say,21=kill/attack,22=throw/toss,23=feed,24=break,25=rub,26=wake
  ; 27=blast,28=score,29=calm,30=walk,31=nothing,32=brief,33=suspend,34=hours,35=log,36=load,37=foo
  ; Prefix matching makes this robust against trailing chars.
  lda cmd_len
  cmp #1
  bcs _daw_have_len
  rts
_daw_have_len:
  beq _daw_len1
  cmp #2
  beq _daw_len2
  cmp #3
  bcs _daw_ge3
  rts
_daw_len1:
  ; L
  lda cmd_buf
  cmp #'L'
  bne _daw_check_q1
  lda #4
  sta command_type
  rts
_daw_check_q1:
  ; Q
  lda cmd_buf
  cmp #'Q'
  bne _daw_check_x1
  lda #5
  sta command_type
  rts

_daw_check_x1:
  ; X
  lda cmd_buf
  cmp #'X'
  bne _daw_check_i1
  lda #6
  sta command_type
  rts
_daw_check_i1:
  ; I
  lda cmd_buf
  cmp #'I'
  bne _daw_check_r1
  lda #3
  sta command_type
  rts
_daw_check_r1:
  ; R
  lda cmd_buf
  cmp #'R'
  bne _daw_no1
  lda #7
  sta command_type
  rts
_daw_no1:
  rts

_daw_len2:
  ; ON
  lda cmd_buf
  cmp #'O'
  bne _daw_no2
  lda cmd_buf+1
  cmp #'N'
  bne _daw_no2
  lda #12
  sta command_type
  rts
_daw_no2:
  rts
_daw_ge3:

  ; GET*
  lda cmd_buf
  cmp #'G'
  bne _daw_check_read
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_read
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_read
  lda #1
  sta command_type
  rts

_daw_check_read:
  ; HELP* (early catch to avoid noun-required fallthrough)
  lda cmd_len
  cmp #4
  bcc _daw_check_read_real
  lda cmd_buf
  cmp #'H'
  bne _daw_check_read_real
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_read_real
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_read_real
  lda cmd_buf+3
  cmp #'P'
  bne _daw_check_read_real
  lda #11
  sta command_type
  rts
_daw_check_read_real:
  ; READ*
  lda cmd_len
  cmp #4
  bcc _daw_check_light
  lda cmd_buf
  cmp #'R'
  bne _daw_check_light
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_light
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_light
  lda cmd_buf+3
  cmp #'D'
  bne _daw_check_light
  lda #7
  sta command_type
  rts

_daw_check_light:
  ; LIGHT*
  lda cmd_len
  cmp #5
  bcc _daw_check_extinguish
  lda cmd_buf
  cmp #'L'
  bne _daw_check_extinguish
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_extinguish
  lda cmd_buf+2
  cmp #'G'
  bne _daw_check_extinguish
  lda cmd_buf+3
  cmp #'H'
  bne _daw_check_extinguish
  lda cmd_buf+4
  cmp #'T'
  bne _daw_check_extinguish
  lda #12
  sta command_type
  rts

_daw_check_extinguish:
  ; EXTINGUISH* / OFF*
  lda cmd_len
  cmp #10
  bcc _daw_check_off
  lda cmd_buf
  cmp #'E'
  bne _daw_check_off
  lda cmd_buf+1
  cmp #'X'
  bne _daw_check_off
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_off
  lda cmd_buf+3
  cmp #'I'
  bne _daw_check_off
  lda cmd_buf+4
  cmp #'N'
  bne _daw_check_off
  lda cmd_buf+5
  cmp #'G'
  bne _daw_check_off
  lda cmd_buf+6
  cmp #'U'
  bne _daw_check_off
  lda cmd_buf+7
  cmp #'I'
  bne _daw_check_off
  lda cmd_buf+8
  cmp #'S'
  bne _daw_check_off
  lda cmd_buf+9
  cmp #'H'
  bne _daw_check_off
  lda #13
  sta command_type
  rts

_daw_check_off:
  lda cmd_len
  cmp #3
  bcc _daw_check_eat
  lda cmd_buf
  cmp #'O'
  bne _daw_check_eat
  lda cmd_buf+1
  cmp #'F'
  bne _daw_check_eat
  lda cmd_buf+2
  cmp #'F'
  bne _daw_check_eat
  lda #13
  sta command_type
  rts

_daw_check_eat:
  ; EAT*
  lda cmd_len
  cmp #3
  bcc _daw_check_drink
  lda cmd_buf
  cmp #'E'
  bne _daw_check_drink
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_drink
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_drink
  lda #14
  sta command_type
  rts

_daw_check_drink:
  ; DRINK*
  lda cmd_len
  cmp #5
  bcc _daw_check_fill
  lda cmd_buf
  cmp #'D'
  bne _daw_check_fill
  lda cmd_buf+1
  cmp #'R'
  bne _daw_check_fill
  lda cmd_buf+2
  cmp #'I'
  bne _daw_check_fill
  lda cmd_buf+3
  cmp #'N'
  bne _daw_check_fill
  lda cmd_buf+4
  cmp #'K'
  bne _daw_check_fill
  lda #15
  sta command_type
  rts

_daw_check_fill:
  ; FILL*
  lda cmd_len
  cmp #4
  bcc _daw_check_pour
  lda cmd_buf
  cmp #'F'
  bne _daw_check_pour
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_pour
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_pour
  lda cmd_buf+3
  cmp #'L'
  bne _daw_check_pour
  lda #16
  sta command_type
  rts

_daw_check_pour:
  ; POUR*
  lda cmd_len
  cmp #4
  bcc _daw_check_empty
  lda cmd_buf
  cmp #'P'
  bne _daw_check_empty
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_empty
  lda cmd_buf+2
  cmp #'U'
  bne _daw_check_empty
  lda cmd_buf+3
  cmp #'R'
  bne _daw_check_empty
  lda #17
  sta command_type
  rts

_daw_check_empty:
  ; EMPTY*
  lda cmd_len
  cmp #5
  bcc _daw_check_wave
  lda cmd_buf
  cmp #'E'
  bne _daw_check_wave
  lda cmd_buf+1
  cmp #'M'
  bne _daw_check_wave
  lda cmd_buf+2
  cmp #'P'
  bne _daw_check_wave
  lda cmd_buf+3
  cmp #'T'
  bne _daw_check_wave
  lda cmd_buf+4
  cmp #'Y'
  bne _daw_check_wave
  lda #17
  sta command_type
  rts

_daw_check_wave:
  ; WAVE*
  lda cmd_len
  cmp #4
  bcc _daw_check_turn
  lda cmd_buf
  cmp #'W'
  bne _daw_check_turn
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_turn
  lda cmd_buf+2
  cmp #'V'
  bne _daw_check_turn
  lda cmd_buf+3
  cmp #'E'
  bne _daw_check_turn
  lda #18
  sta command_type
  rts

_daw_check_turn:
  ; TURN* / SWITCH* as light control verbs; ON/OFF interpretation happens
  ; after tokenization in _rv_normalize_light.
  lda cmd_len
  cmp #4
  bcc _daw_check_switch
  lda cmd_buf
  cmp #'T'
  bne _daw_check_switch
  lda cmd_buf+1
  cmp #'U'
  bne _daw_check_switch
  lda cmd_buf+2
  cmp #'R'
  bne _daw_check_switch
  lda cmd_buf+3
  cmp #'N'
  bne _daw_check_switch
  lda #12
  sta command_type
  rts
_daw_check_switch:
  lda cmd_len
  cmp #6
  bcc _daw_check_exam
  lda cmd_buf
  cmp #'S'
  bne _daw_check_exam
  lda cmd_buf+1
  cmp #'W'
  bne _daw_check_exam
  lda cmd_buf+2
  cmp #'I'
  bne _daw_check_exam
  lda cmd_buf+3
  cmp #'T'
  bne _daw_check_exam
  lda cmd_buf+4
  cmp #'C'
  bne _daw_check_exam
  lda cmd_buf+5
  cmp #'H'
  bne _daw_check_exam
  lda #12
  sta command_type
  rts

_daw_check_exam:
  ; EXAM* / EXAMINE*
  lda cmd_len
  cmp #4
  bcc _daw_check_open
  lda cmd_buf
  cmp #'E'
  bne _daw_check_open
  lda cmd_buf+1
  cmp #'X'
  bne _daw_check_open
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_open
  lda cmd_buf+3
  cmp #'M'
  bne _daw_check_open
  lda #6
  sta command_type
  rts

_daw_check_open:
  ; OPEN*
  lda cmd_len
  cmp #4
  bcc _daw_check_close
  lda cmd_buf
  cmp #'O'
  bne _daw_check_close
  lda cmd_buf+1
  cmp #'P'
  bne _daw_check_close
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_close
  lda cmd_buf+3
  cmp #'N'
  bne _daw_check_close
  lda #9
  sta command_type
  rts

_daw_check_close:
  ; CLOSE*/LOCK*/UNLOCK*
  lda cmd_len
  cmp #4
  bcc _daw_check_lock
  lda cmd_buf
  cmp #'C'
  bne _daw_check_lock
  lda cmd_buf+1
  cmp #'L'
  bne _daw_check_lock
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_lock
  lda cmd_buf+3
  cmp #'S'
  bne _daw_check_lock
  lda cmd_len
  cmp #5
  bcc _daw_close_match
  lda cmd_buf+4
  cmp #'E'
  bne _daw_check_lock
_daw_close_match:
  lda #10
  sta command_type
  rts
_daw_check_lock:
  lda cmd_buf
  cmp #'L'
  bne _daw_check_unlock
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_unlock
  lda cmd_buf+2
  cmp #'C'
  bne _daw_check_unlock
  lda cmd_buf+3
  cmp #'K'
  bne _daw_check_unlock
  lda #10
  sta command_type
  rts
_daw_check_unlock:
  lda cmd_len
  cmp #6
  bcc _daw_check_inspect
  lda cmd_buf
  cmp #'U'
  bne _daw_check_inspect
  lda cmd_buf+1
  cmp #'N'
  bne _daw_check_inspect
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_inspect
  lda cmd_buf+3
  cmp #'O'
  bne _daw_check_inspect
  lda cmd_buf+4
  cmp #'C'
  bne _daw_check_inspect
  lda cmd_buf+5
  cmp #'K'
  bne _daw_check_inspect
  lda #9
  sta command_type
  rts

_daw_check_inspect:
  ; INSP* / INSPECT*
  lda cmd_len
  cmp #4
  bcc _daw_check_inv
  lda cmd_buf
  cmp #'I'
  bne _daw_check_inv
  lda cmd_buf+1
  cmp #'N'
  bne _daw_check_inv
  lda cmd_buf+2
  cmp #'S'
  bne _daw_check_inv
  lda cmd_buf+3
  cmp #'P'
  bne _daw_check_inv
  lda #6
  sta command_type
  rts

_daw_check_inv:
  ; INV*
  lda cmd_buf
  cmp #'I'
  bne _daw_check_take
  lda cmd_buf+1
  cmp #'N'
  bne _daw_check_take
  lda cmd_buf+2
  cmp #'V'
  bne _daw_check_take
  lda #3
  sta command_type
  rts

_daw_check_take:
  lda cmd_len
  cmp #4
  bcs :+
  jmp _daw_check_put
:
  lda cmd_buf
  cmp #'T'
  bne _daw_check_pick
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_pick
  lda cmd_buf+2
  cmp #'K'
  bne _daw_check_pick
  lda cmd_buf+3
  cmp #'E'
  bne _daw_check_pick
  lda #1
  sta command_type
  rts
_daw_check_pick:
  lda cmd_buf
  cmp #'P'
  bne _daw_check_grab
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_grab
  lda cmd_buf+2
  cmp #'C'
  bne _daw_check_grab
  lda cmd_buf+3
  cmp #'K'
  bne _daw_check_grab
  lda #1
  sta command_type
  rts
_daw_check_grab:
  lda cmd_buf
  cmp #'G'
  bne _daw_check_drop
  lda cmd_buf+1
  cmp #'R'
  bne _daw_check_drop
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_drop
  lda cmd_buf+3
  cmp #'B'
  bne _daw_check_drop
  lda #1
  sta command_type
  rts

_daw_check_drop:
  lda cmd_buf
  cmp #'D'
  bne _daw_check_put
  lda cmd_buf+1
  cmp #'R'
  bne _daw_check_put
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_put
  lda cmd_buf+3
  cmp #'P'
  bne _daw_check_put
  lda #2
  sta command_type
  rts
_daw_check_put:
  lda cmd_buf
  cmp #'P'
  bne _daw_check_carry
  lda cmd_buf+1
  cmp #'U'
  bne _daw_check_carry
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_carry
  lda #2
  sta command_type
  rts

_daw_check_carry:
  lda cmd_len
  cmp #5
  bcc _daw_check_look_quit
  lda cmd_buf
  cmp #'L'
  bne _daw_check_carry_word
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_carry_word
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_carry_word
  lda cmd_buf+3
  cmp #'V'
  bne _daw_check_carry_word
  lda cmd_buf+4
  cmp #'E'
  bne _daw_check_carry_word
  lda #2
  sta command_type
  rts
_daw_check_carry_word:
  lda cmd_buf
  cmp #'C'
  beq :+
  jmp _daw_check_look_quit
:
  lda cmd_buf+1
  cmp #'A'
  beq :+
  jmp _daw_check_look_quit
:
  lda cmd_buf+2
  cmp #'R'
  beq :+
  jmp _daw_check_look_quit
:
  lda cmd_buf+3
  cmp #'R'
  beq :+
  jmp _daw_check_look_quit
:
  lda cmd_buf+4
  cmp #'Y'
  beq :+
  jmp _daw_check_look_quit
:
  lda #1
  sta command_type
  rts

_daw_check_look_quit:
  ; LOOK*
  lda cmd_len
  cmp #4
  bcc _daw_check_quit
  lda cmd_buf
  cmp #'L'
  bne _daw_check_quit
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_quit
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_quit
  lda cmd_buf+3
  cmp #'K'
  bne _daw_check_quit
  lda #4
  sta command_type
  rts

_daw_check_quit:
  ; QUIT*
  lda cmd_len
  cmp #4
  bcc _daw_check_help
  lda cmd_buf
  cmp #'Q'
  bne _daw_check_help
  lda cmd_buf+1
  cmp #'U'
  bne _daw_check_help
  lda cmd_buf+2
  cmp #'I'
  bne _daw_check_help
  lda cmd_buf+3
  cmp #'T'
  bne _daw_check_help
  lda #5
  sta command_type
  rts

_daw_check_help:
  ; HELP*
  lda cmd_len
  cmp #4
  bcs :+
  jmp _daw_check_find
:
  lda cmd_buf
  cmp #'H'
  beq :+
  jmp _daw_check_find
:
  lda cmd_buf+1
  cmp #'E'
  beq :+
  jmp _daw_check_find
:
  lda cmd_buf+2
  cmp #'L'
  beq :+
  jmp _daw_check_find
:
  lda cmd_buf+3
  cmp #'P'
  beq :+
  jmp _daw_check_find
:
  lda #11
  sta command_type
  rts

_daw_check_find:
  ; FIND*
  lda cmd_len
  cmp #4
  bcc _daw_check_say
  lda cmd_buf
  cmp #'F'
  bne _daw_check_say
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_say
  lda cmd_buf+2
  cmp #'N'
  bne _daw_check_say
  lda cmd_buf+3
  cmp #'D'
  bne _daw_check_say
  lda #19
  sta command_type
  rts

_daw_check_say:
  ; SAY*
  lda cmd_len
  cmp #3
  bcc _daw_check_kill
  lda cmd_buf
  cmp #'S'
  bne _daw_check_kill
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_kill
  lda cmd_buf+2
  cmp #'Y'
  bne _daw_check_kill
  lda #20
  sta command_type
  rts

_daw_check_kill:
  ; KILL* / ATTACK*
  lda cmd_len
  cmp #4
  bcc _daw_check_throw
  lda cmd_buf
  cmp #'K'
  bne _daw_check_attack
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_attack
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_attack
  lda cmd_buf+3
  cmp #'L'
  bne _daw_check_attack
  lda #21
  sta command_type
  rts
_daw_check_attack:
  lda cmd_len
  cmp #6
  bcc _daw_check_throw
  lda cmd_buf
  cmp #'A'
  bne _daw_check_throw
  lda cmd_buf+1
  cmp #'T'
  bne _daw_check_throw
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_throw
  lda cmd_buf+3
  cmp #'A'
  bne _daw_check_throw
  lda cmd_buf+4
  cmp #'C'
  bne _daw_check_throw
  lda cmd_buf+5
  cmp #'K'
  bne _daw_check_throw
  lda #21
  sta command_type
  rts

_daw_check_throw:
  ; THROW* / TOSS*
  lda cmd_len
  cmp #5
  bcc _daw_check_toss
  lda cmd_buf
  cmp #'T'
  bne _daw_check_toss
  lda cmd_buf+1
  cmp #'H'
  bne _daw_check_toss
  lda cmd_buf+2
  cmp #'R'
  bne _daw_check_toss
  lda cmd_buf+3
  cmp #'O'
  bne _daw_check_toss
  lda cmd_buf+4
  cmp #'W'
  bne _daw_check_toss
  lda #22
  sta command_type
  rts
_daw_check_toss:
  lda cmd_len
  cmp #4
  bcc _daw_check_feed
  lda cmd_buf
  cmp #'T'
  bne _daw_check_feed
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_feed
  lda cmd_buf+2
  cmp #'S'
  bne _daw_check_feed
  lda cmd_buf+3
  cmp #'S'
  bne _daw_check_feed
  lda #22
  sta command_type
  rts

_daw_check_feed:
  ; FEED*
  lda cmd_len
  cmp #4
  bcc _daw_check_break
  lda cmd_buf
  cmp #'F'
  bne _daw_check_break
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_break
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_break
  lda cmd_buf+3
  cmp #'D'
  bne _daw_check_break
  lda #23
  sta command_type
  rts

_daw_check_break:
  ; BREAK*
  lda cmd_len
  cmp #4
  bcc _daw_check_rub
  lda cmd_buf
  cmp #'B'
  bne _daw_check_rub
  lda cmd_buf+1
  cmp #'R'
  bne _daw_check_rub
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_rub
  lda cmd_buf+3
  cmp #'A'
  bne _daw_check_rub
  lda #24
  sta command_type
  rts

_daw_check_rub:
  ; RUB*
  lda cmd_len
  cmp #3
  bcc _daw_check_wake
  lda cmd_buf
  cmp #'R'
  bne _daw_check_wake
  lda cmd_buf+1
  cmp #'U'
  bne _daw_check_wake
  lda cmd_buf+2
  cmp #'B'
  bne _daw_check_wake
  lda #25
  sta command_type
  rts

_daw_check_wake:
  ; WAKE*
  lda cmd_len
  cmp #4
  bcc _daw_check_blast
  lda cmd_buf
  cmp #'W'
  bne _daw_check_blast
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_blast
  lda cmd_buf+2
  cmp #'K'
  bne _daw_check_blast
  lda cmd_buf+3
  cmp #'E'
  bne _daw_check_blast
  lda #26
  sta command_type
  rts

_daw_check_blast:
  ; BLAST* / BLOWUP* / DETONATE* / IGNITE*
  lda cmd_len
  cmp #5
  bcc _daw_check_blowup
  lda cmd_buf
  cmp #'B'
  bne _daw_check_blowup
  lda cmd_buf+1
  cmp #'L'
  bne _daw_check_blowup
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_blowup
  lda cmd_buf+3
  cmp #'S'
  bne _daw_check_blowup
  lda cmd_buf+4
  cmp #'T'
  bne _daw_check_blowup
  lda #27
  sta command_type
  rts
_daw_check_blowup:
  lda cmd_len
  cmp #6
  bcc _daw_check_detonate
  lda cmd_buf
  cmp #'B'
  bne _daw_check_detonate
  lda cmd_buf+1
  cmp #'L'
  bne _daw_check_detonate
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_detonate
  lda cmd_buf+3
  cmp #'W'
  bne _daw_check_detonate
  lda cmd_buf+4
  cmp #'U'
  bne _daw_check_detonate
  lda cmd_buf+5
  cmp #'P'
  bne _daw_check_detonate
  lda #27
  sta command_type
  rts
_daw_check_detonate:
  lda cmd_len
  cmp #8
  bcc _daw_check_ignite
  lda cmd_buf
  cmp #'D'
  bne _daw_check_ignite
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_ignite
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_ignite
  lda cmd_buf+3
  cmp #'O'
  bne _daw_check_ignite
  lda cmd_buf+4
  cmp #'N'
  bne _daw_check_ignite
  lda cmd_buf+5
  cmp #'A'
  bne _daw_check_ignite
  lda cmd_buf+6
  cmp #'T'
  bne _daw_check_ignite
  lda cmd_buf+7
  cmp #'E'
  bne _daw_check_ignite
  lda #27
  sta command_type
  rts
_daw_check_ignite:
  lda cmd_len
  cmp #6
  bcc _daw_check_score
  lda cmd_buf
  cmp #'I'
  bne _daw_check_score
  lda cmd_buf+1
  cmp #'G'
  bne _daw_check_score
  lda cmd_buf+2
  cmp #'N'
  bne _daw_check_score
  lda cmd_buf+3
  cmp #'I'
  bne _daw_check_score
  lda cmd_buf+4
  cmp #'T'
  bne _daw_check_score
  lda cmd_buf+5
  cmp #'E'
  bne _daw_check_score
  lda #27
  sta command_type
  rts

_daw_check_score:
  ; SCORE*
  lda cmd_len
  cmp #5
  bcc _daw_check_calm
  lda cmd_buf
  cmp #'S'
  bne _daw_check_calm
  lda cmd_buf+1
  cmp #'C'
  bne _daw_check_calm
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_calm
  lda cmd_buf+3
  cmp #'R'
  bne _daw_check_calm
  lda cmd_buf+4
  cmp #'E'
  bne _daw_check_calm
  lda #28
  sta command_type
  rts

_daw_check_calm:
  ; CALM* / PLACATE*
  lda cmd_len
  cmp #4
  bcc _daw_check_placate
  lda cmd_buf
  cmp #'C'
  bne _daw_check_placate
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_placate
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_placate
  lda cmd_buf+3
  cmp #'M'
  bne _daw_check_placate
  lda #29
  sta command_type
  rts
_daw_check_placate:
  lda cmd_len
  cmp #7
  bcc _daw_check_walk2
  lda cmd_buf
  cmp #'P'
  bne _daw_check_walk2
  lda cmd_buf+1
  cmp #'L'
  bne _daw_check_walk2
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_walk2
  lda cmd_buf+3
  cmp #'C'
  bne _daw_check_walk2
  lda cmd_buf+4
  cmp #'A'
  bne _daw_check_walk2
  lda cmd_buf+5
  cmp #'T'
  bne _daw_check_walk2
  lda cmd_buf+6
  cmp #'E'
  bne _daw_check_walk2
  lda #29
  sta command_type
  rts

_daw_check_walk2:
  ; WALK*
  lda cmd_len
  cmp #4
  bcc _daw_check_nothing
  lda cmd_buf
  cmp #'W'
  bne _daw_check_nothing
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_nothing
  lda cmd_buf+2
  cmp #'L'
  bne _daw_check_nothing
  lda cmd_buf+3
  cmp #'K'
  bne _daw_check_nothing
  lda #30
  sta command_type
  rts

_daw_check_nothing:
  ; NOTHING*
  lda cmd_len
  cmp #7
  bcc _daw_check_brief
  lda cmd_buf
  cmp #'N'
  bne _daw_check_brief
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_brief
  lda cmd_buf+2
  cmp #'T'
  bne _daw_check_brief
  lda cmd_buf+3
  cmp #'H'
  bne _daw_check_brief
  lda cmd_buf+4
  cmp #'I'
  bne _daw_check_brief
  lda cmd_buf+5
  cmp #'N'
  bne _daw_check_brief
  lda cmd_buf+6
  cmp #'G'
  bne _daw_check_brief
  lda #31
  sta command_type
  rts

_daw_check_brief:
  ; BRIEF*
  lda cmd_len
  cmp #5
  bcc _daw_check_suspend
  lda cmd_buf
  cmp #'B'
  bne _daw_check_suspend
  lda cmd_buf+1
  cmp #'R'
  bne _daw_check_suspend
  lda cmd_buf+2
  cmp #'I'
  bne _daw_check_suspend
  lda cmd_buf+3
  cmp #'E'
  bne _daw_check_suspend
  lda cmd_buf+4
  cmp #'F'
  bne _daw_check_suspend
  lda #32
  sta command_type
  rts

_daw_check_suspend:
  ; SUSPEND* / PAUSE* / SAVE*
  lda cmd_len
  cmp #7
  bcc _daw_check_pause
  lda cmd_buf
  cmp #'S'
  bne _daw_check_pause
  lda cmd_buf+1
  cmp #'U'
  bne _daw_check_pause
  lda cmd_buf+2
  cmp #'S'
  bne _daw_check_pause
  lda cmd_buf+3
  cmp #'P'
  bne _daw_check_pause
  lda cmd_buf+4
  cmp #'E'
  bne _daw_check_pause
  lda cmd_buf+5
  cmp #'N'
  bne _daw_check_pause
  lda cmd_buf+6
  cmp #'D'
  bne _daw_check_pause
  lda #33
  sta command_type
  rts
_daw_check_pause:
  lda cmd_len
  cmp #5
  bcc _daw_check_save
  lda cmd_buf
  cmp #'P'
  bne _daw_check_save
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_save
  lda cmd_buf+2
  cmp #'U'
  bne _daw_check_save
  lda cmd_buf+3
  cmp #'S'
  bne _daw_check_save
  lda cmd_buf+4
  cmp #'E'
  bne _daw_check_save
  lda #33
  sta command_type
  rts
_daw_check_save:
  lda cmd_len
  cmp #4
  bcc _daw_check_hours
  lda cmd_buf
  cmp #'S'
  bne _daw_check_hours
  lda cmd_buf+1
  cmp #'A'
  bne _daw_check_hours
  lda cmd_buf+2
  cmp #'V'
  bne _daw_check_hours
  lda cmd_buf+3
  cmp #'E'
  bne _daw_check_hours
  lda #33
  sta command_type
  rts

_daw_check_hours:
  ; HOURS*
  lda cmd_len
  cmp #5
  bcc _daw_check_log
  lda cmd_buf
  cmp #'H'
  bne _daw_check_log
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_log
  lda cmd_buf+2
  cmp #'U'
  bne _daw_check_log
  lda cmd_buf+3
  cmp #'R'
  bne _daw_check_log
  lda cmd_buf+4
  cmp #'S'
  bne _daw_check_log
  lda #34
  sta command_type
  rts

_daw_check_log:
  ; LOG*
  lda cmd_len
  cmp #3
  bcc _daw_check_load
  lda cmd_buf
  cmp #'L'
  bne _daw_check_load
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_load
  lda cmd_buf+2
  cmp #'G'
  bne _daw_check_load
  lda #35
  sta command_type
  rts

_daw_check_load:
  ; LOAD* / RESTORE*
  lda cmd_len
  cmp #4
  bcc _daw_check_restore
  lda cmd_buf
  cmp #'L'
  bne _daw_check_restore
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_restore
  lda cmd_buf+2
  cmp #'A'
  bne _daw_check_restore
  lda cmd_buf+3
  cmp #'D'
  bne _daw_check_restore
  lda #36
  sta command_type
  rts
_daw_check_restore:
  lda cmd_len
  cmp #7
  bcc _daw_check_foo
  lda cmd_buf
  cmp #'R'
  bne _daw_check_foo
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_foo
  lda cmd_buf+2
  cmp #'S'
  bne _daw_check_foo
  lda cmd_buf+3
  cmp #'T'
  bne _daw_check_foo
  lda cmd_buf+4
  cmp #'O'
  bne _daw_check_foo
  lda cmd_buf+5
  cmp #'R'
  bne _daw_check_foo
  lda cmd_buf+6
  cmp #'E'
  bne _daw_check_foo
  lda #36
  sta command_type
  rts

_daw_check_foo:
  ; FOO/FEE/FIE/FOE/FUM*
  lda cmd_len
  cmp #3
  bcc _daw_check_fum
  lda cmd_buf
  cmp #'F'
  bne _daw_check_fum
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_fee
  lda cmd_buf+2
  cmp #'O'
  bne _daw_check_fum
  lda #37
  sta command_type
  rts
_daw_check_fee:
  lda cmd_buf+1
  cmp #'E'
  bne _daw_check_fie
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_fie
  lda #37
  sta command_type
  rts
_daw_check_fie:
  lda cmd_buf+1
  cmp #'I'
  bne _daw_check_foe
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_foe
  lda #37
  sta command_type
  rts
_daw_check_foe:
  lda cmd_buf+1
  cmp #'O'
  bne _daw_check_fum
  lda cmd_buf+2
  cmp #'E'
  bne _daw_check_fum
  lda #37
  sta command_type
  rts
_daw_check_fum:
  lda cmd_len
  cmp #3
  bcc _daw_no
  lda cmd_buf
  cmp #'F'
  bne _daw_no
  lda cmd_buf+1
  cmp #'U'
  bne _daw_no
  lda cmd_buf+2
  cmp #'M'
  bne _daw_no
  lda #37
  sta command_type
  rts

_daw_no:
  rts

verb_uses_noun_motion:
  ; Returns C set for GO/WALK/RUN/MOVE/HEAD and common travel aliases.
  lda cmd_len
  cmp #2
  bcs :+
  jmp _vunm_no
:
  ; GO
  lda cmd_len
  cmp #2
  bne _vunm_chk_run
  lda cmd_buf
  cmp #'G'
  bne _vunm_chk_run
  lda cmd_buf+1
  cmp #'O'
  bne _vunm_chk_run
  jmp _vunm_yes

_vunm_chk_run:
  ; RUN
  lda cmd_len
  cmp #3
  bne _vunm_chk_walk
  lda cmd_buf
  cmp #'R'
  bne _vunm_chk_walk
  lda cmd_buf+1
  cmp #'U'
  bne _vunm_chk_walk
  lda cmd_buf+2
  cmp #'N'
  bne _vunm_chk_walk
  jmp _vunm_yes

_vunm_chk_walk:
  ; WALK
  lda cmd_len
  cmp #4
  bne _vunm_chk_move
  lda cmd_buf
  cmp #'W'
  bne _vunm_chk_move
  lda cmd_buf+1
  cmp #'A'
  bne _vunm_chk_move
  lda cmd_buf+2
  cmp #'L'
  bne _vunm_chk_move
  lda cmd_buf+3
  cmp #'K'
  bne _vunm_chk_move
  jmp _vunm_yes

_vunm_chk_move:
  ; MOVE
  lda cmd_len
  cmp #4
  bne _vunm_chk_head
  lda cmd_buf
  cmp #'M'
  bne _vunm_chk_head
  lda cmd_buf+1
  cmp #'O'
  bne _vunm_chk_head
  lda cmd_buf+2
  cmp #'V'
  bne _vunm_chk_head
  lda cmd_buf+3
  cmp #'E'
  bne _vunm_chk_head
  jmp _vunm_yes

_vunm_chk_head:
  ; HEAD
  lda cmd_len
  cmp #4
  bne _vunm_chk_step
  lda cmd_buf
  cmp #'H'
  bne _vunm_chk_step
  lda cmd_buf+1
  cmp #'E'
  bne _vunm_chk_step
  lda cmd_buf+2
  cmp #'A'
  bne _vunm_chk_step
  lda cmd_buf+3
  cmp #'D'
  bne _vunm_chk_step
  jmp _vunm_yes

_vunm_chk_step:
  ; STEP
  lda cmd_len
  cmp #4
  bne _vunm_chk_turn
  lda cmd_buf
  cmp #'S'
  bne _vunm_chk_turn
  lda cmd_buf+1
  cmp #'T'
  bne _vunm_chk_turn
  lda cmd_buf+2
  cmp #'E'
  bne _vunm_chk_turn
  lda cmd_buf+3
  cmp #'P'
  bne _vunm_chk_turn
  jmp _vunm_yes

_vunm_chk_turn:
  ; TURN
  lda cmd_len
  cmp #4
  bne _vunm_chk_climb
  lda cmd_buf
  cmp #'T'
  bne _vunm_chk_climb
  lda cmd_buf+1
  cmp #'U'
  bne _vunm_chk_climb
  lda cmd_buf+2
  cmp #'R'
  bne _vunm_chk_climb
  lda cmd_buf+3
  cmp #'N'
  bne _vunm_chk_climb
  jmp _vunm_yes

_vunm_chk_climb:
  ; CLIMB
  lda cmd_len
  cmp #5
  bne _vunm_chk_travel
  lda cmd_buf
  cmp #'C'
  bne _vunm_chk_travel
  lda cmd_buf+1
  cmp #'L'
  bne _vunm_chk_travel
  lda cmd_buf+2
  cmp #'I'
  bne _vunm_chk_travel
  lda cmd_buf+3
  cmp #'M'
  bne _vunm_chk_travel
  lda cmd_buf+4
  cmp #'B'
  bne _vunm_chk_travel
  jmp _vunm_yes

_vunm_chk_travel:
  ; TRAVEL
  lda cmd_len
  cmp #6
  bne _vunm_chk_advance
  lda cmd_buf
  cmp #'T'
  bne _vunm_chk_advance
  lda cmd_buf+1
  cmp #'R'
  bne _vunm_chk_advance
  lda cmd_buf+2
  cmp #'A'
  bne _vunm_chk_advance
  lda cmd_buf+3
  cmp #'V'
  bne _vunm_chk_advance
  lda cmd_buf+4
  cmp #'E'
  bne _vunm_chk_advance
  lda cmd_buf+5
  cmp #'L'
  bne _vunm_chk_advance
  jmp _vunm_yes

_vunm_chk_advance:
  ; ADVANCE
  lda cmd_len
  cmp #7
  bne _vunm_chk_proceed
  lda cmd_buf
  cmp #'A'
  bne _vunm_chk_proceed
  lda cmd_buf+1
  cmp #'D'
  bne _vunm_chk_proceed
  lda cmd_buf+2
  cmp #'V'
  bne _vunm_chk_proceed
  lda cmd_buf+3
  cmp #'A'
  bne _vunm_chk_proceed
  lda cmd_buf+4
  cmp #'N'
  bne _vunm_chk_proceed
  lda cmd_buf+5
  cmp #'C'
  bne _vunm_chk_proceed
  lda cmd_buf+6
  cmp #'E'
  bne _vunm_chk_proceed
  jmp _vunm_yes

_vunm_chk_proceed:
  ; PROCEED
  lda cmd_len
  cmp #7
  bne _vunm_no
  lda cmd_buf
  cmp #'P'
  bne _vunm_no
  lda cmd_buf+1
  cmp #'R'
  bne _vunm_no
  lda cmd_buf+2
  cmp #'O'
  bne _vunm_no
  lda cmd_buf+3
  cmp #'C'
  bne _vunm_no
  lda cmd_buf+4
  cmp #'E'
  bne _vunm_no
  lda cmd_buf+5
  cmp #'E'
  bne _vunm_no
  lda cmd_buf+6
  cmp #'D'
  bne _vunm_no
_vunm_yes:
  sec
  rts
_vunm_no:
  clc
  rts

decode_object_word:
  ; Converts noun token to object id in command_object.
  lda #0
  sta command_object
  lda noun_len
  cmp #3
  bne _dow_len4

  ; AXE
  lda noun_buf
  cmp #'A'
  bne _dow_rod3
  lda noun_buf+1
  cmp #'X'
  bne _dow_rod3
  lda noun_buf+2
  cmp #'E'
  bne _dow_rod3
  lda #28
  sta command_object
  rts

_dow_rod3:
  ; ROD
  lda noun_buf
  cmp #'R'
  bne _dow_key3
  lda noun_buf+1
  cmp #'O'
  bne _dow_key3
  lda noun_buf+2
  cmp #'D'
  bne _dow_key3
  lda #5
  sta command_object
  rts

_dow_key3:
  ; KEY
  lda noun_buf
  cmp #'K'
  bne _dow_oil3
  lda noun_buf+1
  cmp #'E'
  bne _dow_oil3
  lda noun_buf+2
  cmp #'Y'
  bne _dow_oil3
  lda #1
  sta command_object
  rts
_dow_oil3:
  ; OIL
  lda noun_buf
  cmp #'O'
  bne _dow_no3
  lda noun_buf+1
  cmp #'I'
  bne _dow_no3
  lda noun_buf+2
  cmp #'L'
  bne _dow_no3
  lda #22
  sta command_object
  rts
_dow_no3:
  rts

_dow_len4:
  cmp #4
  beq _dow_do4
  jmp _dow_len5
_dow_do4:

  ; GATE (alias for GRATE)
  lda noun_buf
  cmp #'G'
  bne _dow_lamp4
  lda noun_buf+1
  cmp #'A'
  bne _dow_lamp4
  lda noun_buf+2
  cmp #'T'
  bne _dow_lamp4
  lda noun_buf+3
  cmp #'E'
  bne _dow_lamp4
  lda #3
  sta command_object
  rts

_dow_lamp4:

  ; LAMP
  lda noun_buf
  cmp #'L'
  bne _dow_cage4
  lda noun_buf+1
  cmp #'A'
  bne _dow_cage4
  lda noun_buf+2
  cmp #'M'
  bne _dow_cage4
  lda noun_buf+3
  cmp #'P'
  bne _dow_cage4
  lda #2
  sta command_object
  rts

_dow_cage4:
  ; CAGE
  lda noun_buf
  cmp #'C'
  bne _dow_bird4
  lda noun_buf+1
  cmp #'A'
  bne _dow_bird4
  lda noun_buf+2
  cmp #'G'
  bne _dow_bird4
  lda noun_buf+3
  cmp #'E'
  bne _dow_bird4
  lda #4
  sta command_object
  rts

_dow_bird4:
  ; BIRD
  lda noun_buf
  cmp #'B'
  bne _dow_vase4
  lda noun_buf+1
  cmp #'I'
  bne _dow_vase4
  lda noun_buf+2
  cmp #'R'
  bne _dow_vase4
  lda noun_buf+3
  cmp #'D'
  bne _dow_vase4
  lda #8
  sta command_object
  rts

_dow_vase4:
  ; VASE
  lda noun_buf
  cmp #'V'
  bne _dow_coin4
  lda noun_buf+1
  cmp #'A'
  bne _dow_coin4
  lda noun_buf+2
  cmp #'S'
  bne _dow_coin4
  lda noun_buf+3
  cmp #'E'
  bne _dow_coin4
  lda #58
  sta command_object
  rts

_dow_coin4:
    ; COIN
    lda noun_buf
    cmp #'C'
    bne _dow_keys4
    lda noun_buf+1
    cmp #'O'
    bne _dow_keys4
    lda noun_buf+2
    cmp #'I'
    bne _dow_keys4
    lda noun_buf+3
    cmp #'N'
    bne _dow_keys4
    lda #54
    sta command_object
    rts

_dow_keys4:
    ; KEYS
    lda noun_buf
    cmp #'K'
    bne _dow_food4
    lda noun_buf+1
    cmp #'E'
    bne _dow_food4
    lda noun_buf+2
    cmp #'Y'
    bne _dow_food4
    lda noun_buf+3
    cmp #'S'
    bne _dow_food4
    lda #1
    sta command_object
    rts

_dow_food4:
    ; FOOD
    lda noun_buf
    cmp #'F'
    bne _dow_door4
    lda noun_buf+1
    cmp #'O'
    bne _dow_door4
    lda noun_buf+2
    cmp #'O'
    bne _dow_door4
    lda noun_buf+3
    cmp #'D'
    bne _dow_door4
    lda #19
    sta command_object
    rts
_dow_door4:
    ; DOOR
    lda noun_buf
    cmp #'D'
    bne _dow_clam4
    lda noun_buf+1
    cmp #'O'
    bne _dow_clam4
    lda noun_buf+2
    cmp #'O'
    bne _dow_clam4
    lda noun_buf+3
    cmp #'R'
    bne _dow_clam4
    lda #9
    sta command_object
    rts
_dow_clam4:
    ; CLAM
    lda noun_buf
    cmp #'C'
    bne _dow_bear4
    lda noun_buf+1
    cmp #'L'
    bne _dow_bear4
    lda noun_buf+2
    cmp #'A'
    bne _dow_bear4
    lda noun_buf+3
    cmp #'M'
    bne _dow_bear4
    lda #14
    sta command_object
    rts
_dow_bear4:
    ; BEAR
    lda noun_buf
    cmp #'B'
    bne _dow_rug4
    lda noun_buf+1
    cmp #'E'
    bne _dow_rug4
    lda noun_buf+2
    cmp #'A'
    bne _dow_rug4
    lda noun_buf+3
    cmp #'R'
    bne _dow_rug4
    lda #35
    sta command_object
    rts
_dow_rug4:
    ; RUG
    lda noun_buf
    cmp #'R'
    bne _dow_eggs4
    lda noun_buf+1
    cmp #'U'
    bne _dow_eggs4
    lda noun_buf+2
    cmp #'G'
    bne _dow_eggs4
    lda #62
    sta command_object
    rts
_dow_eggs4:
    ; EGGS
    lda noun_buf
    cmp #'E'
    bne _dow_no4
    lda noun_buf+1
    cmp #'G'
    bne _dow_no4
    lda noun_buf+2
    cmp #'G'
    bne _dow_no4
    lda noun_buf+3
    cmp #'S'
    bne _dow_no4
    lda #56
    sta command_object
    rts
_dow_no4:
    rts

_dow_len5:
    cmp #5
    beq :+
    jmp _dow_len6
:

    ; GRATE
    lda noun_buf
    cmp #'G'
    bne _dow_coin5
    lda noun_buf+1
    cmp #'R'
    bne _dow_coin5
    lda noun_buf+2
    cmp #'A'
    bne _dow_coin5
    lda noun_buf+3
    cmp #'T'
    bne _dow_coin5
    lda noun_buf+4
    cmp #'E'
    bne _dow_coin5
    lda #3
    sta command_object
    rts

_dow_coin5:
    ; COINS
    lda noun_buf
    cmp #'C'
    bne _dow_food5
    lda noun_buf+1
    cmp #'O'
    bne _dow_food5
    lda noun_buf+2
    cmp #'I'
    bne _dow_food5
    lda noun_buf+3
    cmp #'N'
    bne _dow_food5
    lda noun_buf+4
    cmp #'S'
    bne _dow_food5
    lda #54
    sta command_object
    rts

_dow_food5:
    ; WATER
    lda noun_buf
    cmp #'W'
    bne _dow_keys5
    lda noun_buf+1
    cmp #'A'
    bne _dow_keys5
    lda noun_buf+2
    cmp #'T'
    bne _dow_keys5
    lda noun_buf+3
    cmp #'E'
    bne _dow_keys5
    lda noun_buf+4
    cmp #'R'
    bne _dow_keys5
    lda #21
    sta command_object
    rts

_dow_keys5:
    ; KEYS.
    ; Accept 5-letter misspelling "KEYES" as a fallback.
    lda noun_buf
    cmp #'K'
    bne _dow_snake5
    lda noun_buf+1
    cmp #'E'
    bne _dow_snake5
    lda noun_buf+2
    cmp #'Y'
    bne _dow_snake5
    lda noun_buf+3
    cmp #'E'
    bne _dow_snake5
    lda noun_buf+4
    cmp #'S'
    bne _dow_snake5
    lda #1
    sta command_object
    rts
_dow_snake5:
    ; SNAKE
    lda noun_buf
    cmp #'S'
    bne _dow_dwarf5
    lda noun_buf+1
    cmp #'N'
    bne _dow_dwarf5
    lda noun_buf+2
    cmp #'A'
    bne _dow_dwarf5
    lda noun_buf+3
    cmp #'K'
    bne _dow_dwarf5
    lda noun_buf+4
    cmp #'E'
    bne _dow_dwarf5
    lda #11
    sta command_object
    rts
_dow_dwarf5:
    ; DWARF
    lda noun_buf
    cmp #'D'
    bne _dow_chest5
    lda noun_buf+1
    cmp #'W'
    bne _dow_chest5
    lda noun_buf+2
    cmp #'A'
    bne _dow_chest5
    lda noun_buf+3
    cmp #'R'
    bne _dow_chest5
    lda noun_buf+4
    cmp #'F'
    bne _dow_chest5
    lda #17
    sta command_object
    rts
_dow_chest5:
    ; CHEST
    lda noun_buf
    cmp #'C'
    bne _dow_plant5
    lda noun_buf+1
    cmp #'H'
    bne _dow_plant5
    lda noun_buf+2
    cmp #'E'
    bne _dow_plant5
    lda noun_buf+3
    cmp #'S'
    bne _dow_plant5
    lda noun_buf+4
    cmp #'T'
    bne _dow_plant5
    lda #55
    sta command_object
    rts
_dow_plant5:
    ; PLANT
    lda noun_buf
    cmp #'P'
    bne _dow_steps5
    lda noun_buf+1
    cmp #'L'
    bne _dow_steps5
    lda noun_buf+2
    cmp #'A'
    bne _dow_steps5
    lda noun_buf+3
    cmp #'N'
    bne _dow_steps5
    lda noun_buf+4
    cmp #'T'
    bne _dow_steps5
    lda #24
    sta command_object
    rts
_dow_steps5:
    ; STEPS
    lda noun_buf
    cmp #'S'
    bne _dow_troll5
    lda noun_buf+1
    cmp #'T'
    bne _dow_troll5
    lda noun_buf+2
    cmp #'E'
    bne _dow_troll5
    lda noun_buf+3
    cmp #'P'
    bne _dow_troll5
    lda noun_buf+4
    cmp #'S'
    bne _dow_troll5
    lda #7
    sta command_object
    rts
_dow_troll5:
    ; TROLL
    lda noun_buf
    cmp #'T'
    bne _dow_chain5
    lda noun_buf+1
    cmp #'R'
    bne _dow_chain5
    lda noun_buf+2
    cmp #'O'
    bne _dow_chain5
    lda noun_buf+3
    cmp #'L'
    bne _dow_chain5
    lda noun_buf+4
    cmp #'L'
    bne _dow_chain5
    lda #33
    sta command_object
    rts
_dow_chain5:
    ; CHAIN
    lda noun_buf
    cmp #'C'
    bne _dow_pearl5
    lda noun_buf+1
    cmp #'H'
    bne _dow_pearl5
    lda noun_buf+2
    cmp #'A'
    bne _dow_pearl5
    lda noun_buf+3
    cmp #'I'
    bne _dow_pearl5
    lda noun_buf+4
    cmp #'N'
    bne _dow_pearl5
    lda #63
    sta command_object
    rts
_dow_pearl5:
    ; PEARL
    lda noun_buf
    cmp #'P'
    bne _dow_no5
    lda noun_buf+1
    cmp #'E'
    bne _dow_no5
    lda noun_buf+2
    cmp #'A'
    bne _dow_no5
    lda noun_buf+3
    cmp #'R'
    bne _dow_no5
    lda noun_buf+4
    cmp #'L'
    bne _dow_no5
    lda #61
    sta command_object
    rts
_dow_no5:
  rts

_dow_len6:
  cmp #6
  bne :+
  jmp _dow_len6_do
:
  jmp _dow_len7
_dow_len6_do:

  ; BOTTLE
  lda noun_buf
  cmp #'B'
  bne _dow_mirror6
  lda noun_buf+1
  cmp #'O'
  bne _dow_mirror6
  lda noun_buf+2
  cmp #'T'
  bne _dow_mirror6
  lda noun_buf+3
  cmp #'T'
  bne _dow_mirror6
  lda noun_buf+4
  cmp #'L'
  bne _dow_mirror6
  lda noun_buf+5
  cmp #'E'
  bne _dow_mirror6
  lda #20
  sta command_object
  rts
_dow_mirror6:
  ; MIRROR
  lda noun_buf
  cmp #'M'
  bne _dow_oyster6
  lda noun_buf+1
  cmp #'I'
  bne _dow_oyster6
  lda noun_buf+2
  cmp #'R'
  bne _dow_oyster6
  lda noun_buf+3
  cmp #'R'
  bne _dow_oyster6
  lda noun_buf+4
  cmp #'O'
  bne _dow_oyster6
  lda noun_buf+5
  cmp #'R'
  bne _dow_oyster6
  lda #23
  sta command_object
  rts
_dow_oyster6:
  ; OYSTER
  lda noun_buf
  cmp #'O'
  bne _dow_pirate6
  lda noun_buf+1
  cmp #'Y'
  bne _dow_pirate6
  lda noun_buf+2
  cmp #'S'
  bne _dow_pirate6
  lda noun_buf+3
  cmp #'T'
  bne _dow_pirate6
  lda noun_buf+4
  cmp #'E'
  bne _dow_pirate6
  lda noun_buf+5
  cmp #'R'
  bne _dow_pirate6
  lda #15
  sta command_object
  rts
_dow_pirate6:
  ; PIRATE
  lda noun_buf
  cmp #'P'
  bne _dow_dragon6
  lda noun_buf+1
  cmp #'I'
  bne _dow_dragon6
  lda noun_buf+2
  cmp #'R'
  bne _dow_dragon6
  lda noun_buf+3
  cmp #'A'
  bne _dow_dragon6
  lda noun_buf+4
  cmp #'T'
  bne _dow_dragon6
  lda noun_buf+5
  cmp #'E'
  bne _dow_dragon6
  lda #30
  sta command_object
  rts
_dow_dragon6:
  ; DRAGON
  lda noun_buf
  cmp #'D'
  bne _dow_chasm6
  lda noun_buf+1
  cmp #'R'
  bne _dow_chasm6
  lda noun_buf+2
  cmp #'A'
  bne _dow_chasm6
  lda noun_buf+3
  cmp #'G'
  bne _dow_chasm6
  lda noun_buf+4
  cmp #'O'
  bne _dow_chasm6
  lda noun_buf+5
  cmp #'N'
  bne _dow_chasm6
  lda #31
  sta command_object
  rts
_dow_chasm6:
  ; CHASM
  lda noun_buf
  cmp #'C'
  bne _dow_pillow6
  lda noun_buf+1
  cmp #'H'
  bne _dow_pillow6
  lda noun_buf+2
  cmp #'A'
  bne _dow_pillow6
  lda noun_buf+3
  cmp #'S'
  bne _dow_pillow6
  lda noun_buf+4
  cmp #'M'
  bne _dow_pillow6
  lda #32
  sta command_object
  rts
_dow_pillow6:
  ; PILLOW
  lda noun_buf
  cmp #'P'
  bne _dow_tablet6
  lda noun_buf+1
  cmp #'I'
  bne _dow_tablet6
  lda noun_buf+2
  cmp #'L'
  bne _dow_tablet6
  lda noun_buf+3
  cmp #'L'
  bne _dow_tablet6
  lda noun_buf+4
  cmp #'O'
  bne _dow_tablet6
  lda noun_buf+5
  cmp #'W'
  bne _dow_tablet6
  lda #10
  sta command_object
  rts
_dow_tablet6:
  ; TABLET
  lda noun_buf
  cmp #'T'
  bne _dow_silver6
  lda noun_buf+1
  cmp #'A'
  bne _dow_silver6
  lda noun_buf+2
  cmp #'B'
  bne _dow_silver6
  lda noun_buf+3
  cmp #'L'
  bne _dow_silver6
  lda noun_buf+4
  cmp #'E'
  bne _dow_silver6
  lda noun_buf+5
  cmp #'T'
  bne _dow_silver6
  lda #13
  sta command_object
  rts
_dow_silver6:
  ; SILVER
  lda noun_buf
  cmp #'S'
  bne _dow_nugget6
  lda noun_buf+1
  cmp #'I'
  bne _dow_nugget6
  lda noun_buf+2
  cmp #'L'
  bne _dow_nugget6
  lda noun_buf+3
  cmp #'V'
  bne _dow_nugget6
  lda noun_buf+4
  cmp #'E'
  bne _dow_nugget6
  lda noun_buf+5
  cmp #'R'
  bne _dow_nugget6
  lda #52
  sta command_object
  rts
_dow_nugget6:
  ; NUGGET
  lda noun_buf
  cmp #'N'
  bne _dow_carpet6
  lda noun_buf+1
  cmp #'U'
  bne _dow_carpet6
  lda noun_buf+2
  cmp #'G'
  bne _dow_carpet6
  lda noun_buf+3
  cmp #'G'
  bne _dow_carpet6
  lda noun_buf+4
  cmp #'E'
  bne _dow_carpet6
  lda noun_buf+5
  cmp #'T'
  bne _dow_carpet6
  lda #50
  sta command_object
  rts
_dow_carpet6:
  ; CARPET
  lda noun_buf
  cmp #'C'
  bne _dow_no6
  lda noun_buf+1
  cmp #'A'
  bne _dow_no6
  lda noun_buf+2
  cmp #'R'
  bne _dow_no6
  lda noun_buf+3
  cmp #'P'
  bne _dow_no6
  lda noun_buf+4
  cmp #'E'
  bne _dow_no6
  lda noun_buf+5
  cmp #'T'
  bne _dow_no6
  lda #40
  sta command_object
  rts
_dow_no6:
  rts

_dow_len7:
  cmp #7
  bne :+
  jmp _dow_len7_do
:
  jmp _dow_len8
_dow_len7_do:

  ; EMERALD
  lda noun_buf
  cmp #'E'
  bne _dow_fissure7
  lda noun_buf+1
  cmp #'M'
  bne _dow_fissure7
  lda noun_buf+2
  cmp #'E'
  bne _dow_fissure7
  lda noun_buf+3
  cmp #'R'
  bne _dow_fissure7
  lda noun_buf+4
  cmp #'A'
  bne _dow_fissure7
  lda noun_buf+5
  cmp #'L'
  bne _dow_fissure7
  lda noun_buf+6
  cmp #'D'
  bne _dow_fissure7
  lda #59
  sta command_object
  rts
_dow_fissure7:
  ; FISSURE
  lda noun_buf
  cmp #'F'
  bne _dow_trident7
  lda noun_buf+1
  cmp #'I'
  bne _dow_trident7
  lda noun_buf+2
  cmp #'S'
  bne _dow_trident7
  lda noun_buf+3
  cmp #'S'
  bne _dow_trident7
  lda noun_buf+4
  cmp #'U'
  bne _dow_trident7
  lda noun_buf+5
  cmp #'R'
  bne _dow_trident7
  lda noun_buf+6
  cmp #'E'
  bne _dow_trident7
  lda #12
  sta command_object
  rts
_dow_trident7:
  ; TRIDENT
  lda noun_buf
  cmp #'T'
  bne _dow_jewelry7
  lda noun_buf+1
  cmp #'R'
  bne _dow_jewelry7
  lda noun_buf+2
  cmp #'I'
  bne _dow_jewelry7
  lda noun_buf+3
  cmp #'D'
  bne _dow_jewelry7
  lda noun_buf+4
  cmp #'E'
  bne _dow_jewelry7
  lda noun_buf+5
  cmp #'N'
  bne _dow_jewelry7
  lda noun_buf+6
  cmp #'T'
  bne _dow_jewelry7
  lda #57
  sta command_object
  rts
_dow_jewelry7:
  ; JEWELRY
  lda noun_buf
  cmp #'J'
  bne _dow_battery7
  lda noun_buf+1
  cmp #'E'
  bne _dow_battery7
  lda noun_buf+2
  cmp #'W'
  bne _dow_battery7
  lda noun_buf+3
  cmp #'E'
  bne _dow_battery7
  lda noun_buf+4
  cmp #'L'
  bne _dow_battery7
  lda noun_buf+5
  cmp #'R'
  bne _dow_battery7
  lda noun_buf+6
  cmp #'Y'
  bne _dow_battery7
  lda #53
  sta command_object
  rts
_dow_battery7:
  ; BATTERY
  lda noun_buf
  cmp #'B'
  bne _dow_pyramid7
  lda noun_buf+1
  cmp #'A'
  bne _dow_pyramid7
  lda noun_buf+2
  cmp #'T'
  bne _dow_pyramid7
  lda noun_buf+3
  cmp #'T'
  bne _dow_pyramid7
  lda noun_buf+4
  cmp #'E'
  bne _dow_pyramid7
  lda noun_buf+5
  cmp #'R'
  bne _dow_pyramid7
  lda noun_buf+6
  cmp #'Y'
  bne _dow_pyramid7
  lda #39
  sta command_object
  rts
_dow_pyramid7:
  ; PYRAMID
  lda noun_buf
  cmp #'P'
  bne _dow_no7
  lda noun_buf+1
  cmp #'Y'
  bne _dow_no7
  lda noun_buf+2
  cmp #'R'
  bne _dow_no7
  lda noun_buf+3
  cmp #'A'
  bne _dow_no7
  lda noun_buf+4
  cmp #'M'
  bne _dow_no7
  lda noun_buf+5
  cmp #'I'
  bne _dow_no7
  lda noun_buf+6
  cmp #'D'
  bne _dow_no7
  lda #60
  sta command_object
  rts
_dow_no7:
  rts

_dow_len8:
  cmp #8
  bne :+
  jmp _dow_len8_do
:
  jmp _dow_len9
_dow_len8_do:

  ; DIAMONDS
  lda noun_buf
  cmp #'D'
  bne _dow_magazine8
  lda noun_buf+1
  cmp #'I'
  bne _dow_magazine8
  lda noun_buf+2
  cmp #'A'
  bne _dow_magazine8
  lda noun_buf+3
  cmp #'M'
  bne _dow_magazine8
  lda noun_buf+4
  cmp #'O'
  bne _dow_magazine8
  lda noun_buf+5
  cmp #'N'
  bne _dow_magazine8
  lda noun_buf+6
  cmp #'D'
  bne _dow_magazine8
  lda noun_buf+7
  cmp #'S'
  bne _dow_magazine8
  lda #51
  sta command_object
  rts
_dow_magazine8:
  ; MAGAZINE
  lda noun_buf
  cmp #'M'
  bne _dow_drawings8
  lda noun_buf+1
  cmp #'A'
  bne _dow_drawings8
  lda noun_buf+2
  cmp #'G'
  bne _dow_drawings8
  lda noun_buf+3
  cmp #'A'
  bne _dow_drawings8
  lda noun_buf+4
  cmp #'Z'
  bne _dow_drawings8
  lda noun_buf+5
  cmp #'I'
  bne _dow_drawings8
  lda noun_buf+6
  cmp #'N'
  bne _dow_drawings8
  lda noun_buf+7
  cmp #'E'
  bne _dow_drawings8
  lda #16
  sta command_object
  rts
_dow_drawings8:
  ; DRAWINGS
  lda noun_buf
  cmp #'D'
  bne _dow_drawings8_no
  lda noun_buf+1
  cmp #'R'
  bne _dow_drawings8_no
  lda noun_buf+2
  cmp #'A'
  bne _dow_drawings8_no
  lda noun_buf+3
  cmp #'W'
  bne _dow_drawings8_no
  lda noun_buf+4
  cmp #'I'
  bne _dow_drawings8_no
  lda noun_buf+5
  cmp #'N'
  bne _dow_drawings8_no
  lda noun_buf+6
  cmp #'G'
  bne _dow_drawings8_no
  lda noun_buf+7
  cmp #'S'
  bne _dow_drawings8_no
  lda #29
  sta command_object
  rts
_dow_drawings8_no:
  jmp _dow_no

_dow_len9:
  cmp #9
  bne _dow_no

  ; BATTERIES
  lda noun_buf
  cmp #'B'
  bne _dow_no
  lda noun_buf+1
  cmp #'A'
  bne _dow_no
  lda noun_buf+2
  cmp #'T'
  bne _dow_no
  lda noun_buf+3
  cmp #'T'
  bne _dow_no
  lda noun_buf+4
  cmp #'E'
  bne _dow_no
  lda noun_buf+5
  cmp #'R'
  bne _dow_no
  lda noun_buf+6
  cmp #'I'
  bne _dow_no
  lda noun_buf+7
  cmp #'E'
  bne _dow_no
  lda noun_buf+8
  cmp #'S'
  bne _dow_no
  lda #39
  sta command_object
  rts

_dow_no:
  rts

decode_motion_word:
  ; Default stays target_verb=1 if no match.
  lda cmd_len
  cmp #1
  bne _dm_chk2
  jmp _dm_len1
_dm_chk2:
  cmp #2
  bne _dm_chk3
  jmp _dm_len2
_dm_chk3:
  cmp #3
  bne _dm_chk4
  jmp _dm_len3
_dm_chk4:
  cmp #4
  bne _dm_chk5
  jmp _dm_len4
_dm_chk5:
  cmp #5
  bne _dm_chk6
  jmp _dm_len5
_dm_chk6:
  cmp #6
  bne _dm_chk7
  jmp _dm_len6
_dm_chk7:
  cmp #7
  bne _dm_chk8
  jmp _dm_len7
_dm_chk8:
  cmp #8
  bne _dm_chk9
  jmp _dm_len8
_dm_chk9:
  cmp #9
  bne _dm_chk10
  jmp _dm_len9
_dm_chk10:
  cmp #10
  bne _dm_dispatch_no
  jmp _dm_len10
_dm_dispatch_no:
  rts

_dm_len1:
  lda cmd_buf
  cmp #'N'
  bne :+
  lda #45
  sta target_verb
  rts
:
  cmp #'S'
  bne :+
  lda #46
  sta target_verb
  rts
:
  cmp #'E'
  bne :+
  lda #43
  sta target_verb
  rts
:
  cmp #'W'
  bne :+
  lda #44
  sta target_verb
  rts
:
  cmp #'U'
  bne :+
  lda #29
  sta target_verb
  rts
:
  cmp #'D'
  bne :+
  lda #30
  sta target_verb
  rts
:
  cmp #'I'
  bne :+
  lda #19
  sta target_verb
  rts
:
  cmp #'O'
  bne _dm_len1_no
  lda #11
  sta target_verb
  rts
_dm_len1_no:
  rts

_dm_len2:
  lda cmd_buf
  cmp #'N'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda #47
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'N'
  bne :+
  lda cmd_buf+1
  cmp #'W'
  bne :+
  lda #50
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda #48
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne :+
  lda cmd_buf+1
  cmp #'W'
  bne :+
  lda #49
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'N'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda #45
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda #46
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'E'
  bne :+
  lda cmd_buf+1
  cmp #'A'
  bne :+
  lda #43
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'W'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda #44
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'U'
  bne :+
  lda cmd_buf+1
  cmp #'P'
  bne :+
  lda #29
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'I'
  bne _dm_len2_no
  lda cmd_buf+1
  cmp #'N'
  bne _dm_len2_no
  lda #19
  sta target_verb
  rts
_dm_len2_no:
  rts

_dm_len3:
  lda cmd_buf
  cmp #'O'
  bne _dm_len3_pit
  lda cmd_buf+1
  cmp #'U'
  bne _dm_len3_pit
  lda cmd_buf+2
  cmp #'T'
  bne _dm_len3_pit
  lda #11
  sta target_verb
  rts
_dm_len3_pit:
  lda cmd_buf
  cmp #'P'
  bne _dm_len3_no
  lda cmd_buf+1
  cmp #'I'
  bne _dm_len3_no
  lda cmd_buf+2
  cmp #'T'
  bne _dm_len3_no
  lda #63
  sta target_verb
  rts
_dm_len3_no:
  rts

_dm_len4:
  lda cmd_buf
  cmp #'E'
  bne :+
  lda cmd_buf+1
  cmp #'A'
  bne :+
  lda cmd_buf+2
  cmp #'S'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda #43
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'W'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'S'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda #44
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'D'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'W'
  bne :+
  lda cmd_buf+3
  cmp #'N'
  bne :+
  lda #30
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'L'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'F'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda #44
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'W'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'L'
  bne :+
  lda cmd_buf+3
  cmp #'L'
  bne :+
  lda #12
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'H'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'M'
  bne :+
  lda cmd_buf+3
  cmp #'E'
  bne :+
  lda #12
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'G'
  bne :+
  lda cmd_buf+1
  cmp #'A'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'E'
  bne :+
  lda #63
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'R'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'A'
  bne :+
  lda cmd_buf+3
  cmp #'D'
  bne :+
  lda #2
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'B'
  bne :+
  lda cmd_buf+1
  cmp #'A'
  bne :+
  lda cmd_buf+2
  cmp #'C'
  bne :+
  lda cmd_buf+3
  cmp #'K'
  bne :+
  lda #8
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'E'
  bne :+
  lda cmd_buf+1
  cmp #'X'
  bne :+
  lda cmd_buf+2
  cmp #'I'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda #11
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'J'
  bne _dm_len4_no
  lda cmd_buf+1
  cmp #'U'
  bne _dm_len4_no
  lda cmd_buf+2
  cmp #'M'
  bne _dm_len4_no
  lda cmd_buf+3
  cmp #'P'
  bne _dm_len4_no
  lda #39
  sta target_verb
  rts
_dm_len4_no:
  rts

_dm_len5:
  lda cmd_buf
  cmp #'N'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'R'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda cmd_buf+4
  cmp #'H'
  bne :+
  lda #45
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'U'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda cmd_buf+4
  cmp #'H'
  bne :+
  lda #46
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'R'
  bne :+
  lda cmd_buf+1
  cmp #'I'
  bne :+
  lda cmd_buf+2
  cmp #'G'
  bne :+
  lda cmd_buf+3
  cmp #'H'
  bne :+
  lda cmd_buf+4
  cmp #'T'
  bne :+
  lda #37
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'E'
  bne :+
  lda cmd_buf+1
  cmp #'N'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'E'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda #3
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'X'
  bne :+
  lda cmd_buf+1
  cmp #'Y'
  bne :+
  lda cmd_buf+2
  cmp #'Z'
  bne :+
  lda cmd_buf+3
  cmp #'Z'
  bne :+
  lda cmd_buf+4
  cmp #'Y'
  bne :+
  lda #62
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'P'
  bne :+
  lda cmd_buf+1
  cmp #'L'
  bne :+
  lda cmd_buf+2
  cmp #'U'
  bne :+
  lda cmd_buf+3
  cmp #'G'
  bne :+
  lda cmd_buf+4
  cmp #'H'
  bne :+
  lda #65
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'H'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'U'
  bne :+
  lda cmd_buf+3
  cmp #'S'
  bne :+
  lda cmd_buf+4
  cmp #'E'
  bne :+
  lda #12
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'G'
  bne :+
  lda cmd_buf+1
  cmp #'R'
  bne :+
  lda cmd_buf+2
  cmp #'A'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda cmd_buf+4
  cmp #'E'
  bne :+
  lda #63
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne :+
  lda cmd_buf+1
  cmp #'H'
  bne :+
  lda cmd_buf+2
  cmp #'A'
  bne :+
  lda cmd_buf+3
  cmp #'C'
  bne :+
  lda cmd_buf+4
  cmp #'K'
  bne :+
  lda #12
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'C'
  bne _dm_len5_no
  lda cmd_buf+1
  cmp #'R'
  bne _dm_len5_no
  lda cmd_buf+2
  cmp #'A'
  bne _dm_len5_no
  lda cmd_buf+3
  cmp #'W'
  bne _dm_len5_no
  lda cmd_buf+4
  cmp #'L'
  bne _dm_len5_no
  lda #17
  sta target_verb
  rts
_dm_len5_no:
  rts

_dm_len6:
  lda cmd_buf
  cmp #'F'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'R'
  bne :+
  lda cmd_buf+3
  cmp #'E'
  bne :+
  lda cmd_buf+4
  cmp #'S'
  bne :+
  lda cmd_buf+5
  cmp #'T'
  bne :+
  lda #6
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'I'
  bne :+
  lda cmd_buf+1
  cmp #'N'
  bne :+
  lda cmd_buf+2
  cmp #'S'
  bne :+
  lda cmd_buf+3
  cmp #'I'
  bne :+
  lda cmd_buf+4
  cmp #'D'
  bne :+
  lda cmd_buf+5
  cmp #'E'
  bne :+
  lda #19
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'P'
  bne :+
  lda cmd_buf+1
  cmp #'L'
  bne :+
  lda cmd_buf+2
  cmp #'O'
  bne :+
  lda cmd_buf+3
  cmp #'V'
  bne :+
  lda cmd_buf+4
  cmp #'E'
  bne :+
  lda cmd_buf+5
  cmp #'R'
  bne :+
  lda #71
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'R'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'U'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'N'
  bne :+
  lda #8
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'V'
  bne :+
  lda cmd_buf+1
  cmp #'A'
  bne :+
  lda cmd_buf+2
  cmp #'L'
  bne :+
  lda cmd_buf+3
  cmp #'L'
  bne :+
  lda cmd_buf+4
  cmp #'E'
  bne :+
  lda cmd_buf+5
  cmp #'Y'
  bne :+
  lda #9
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'G'
  bne :+
  lda cmd_buf+1
  cmp #'U'
  bne :+
  lda cmd_buf+2
  cmp #'L'
  bne :+
  lda cmd_buf+3
  cmp #'L'
  bne :+
  lda cmd_buf+4
  cmp #'Y'
  bne :+
  lda #13
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'I'
  bne :+
  lda cmd_buf+1
  cmp #'N'
  bne :+
  lda cmd_buf+2
  cmp #'W'
  bne :+
  lda cmd_buf+3
  cmp #'A'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'D'
  bne :+
  lda #19
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'O'
  bne :+
  lda cmd_buf+1
  cmp #'N'
  bne :+
  lda cmd_buf+2
  cmp #'W'
  bne :+
  lda cmd_buf+3
  cmp #'A'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'D'
  bne :+
  lda #7
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'U'
  bne :+
  lda cmd_buf+1
  cmp #'P'
  bne :+
  lda cmd_buf+2
  cmp #'W'
  bne :+
  lda cmd_buf+3
  cmp #'A'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'D'
  bne :+
  lda #29
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'S'
  bne _dm_len6_no
  lda cmd_buf+1
  cmp #'T'
  bne _dm_len6_no
  lda cmd_buf+2
  cmp #'R'
  bne _dm_len6_no
  lda cmd_buf+3
  cmp #'E'
  bne _dm_len6_no
  lda cmd_buf+4
  cmp #'A'
  bne _dm_len6_no
  lda cmd_buf+5
  cmp #'M'
  bne _dm_len6_no
  lda #14
  sta target_verb
  rts
_dm_len6_no:
  rts

_dm_len7:
  lda cmd_buf
  cmp #'F'
  bne :+
  lda cmd_buf+1
  cmp #'O'
  bne :+
  lda cmd_buf+2
  cmp #'R'
  bne :+
  lda cmd_buf+3
  cmp #'W'
  bne :+
  lda cmd_buf+4
  cmp #'A'
  bne :+
  lda cmd_buf+5
  cmp #'R'
  bne :+
  lda cmd_buf+6
  cmp #'D'
  bne :+
  lda #7
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'R'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'R'
  bne :+
  lda cmd_buf+4
  cmp #'E'
  bne :+
  lda cmd_buf+5
  cmp #'A'
  bne :+
  lda cmd_buf+6
  cmp #'T'
  bne :+
  lda #8
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'R'
  bne :+
  lda cmd_buf+1
  cmp #'E'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'U'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'N'
  bne :+
  lda #8
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'O'
  bne _dm_len7_no
  lda cmd_buf+1
  cmp #'U'
  bne _dm_len7_no
  lda cmd_buf+2
  cmp #'T'
  bne _dm_len7_no
  lda cmd_buf+3
  cmp #'S'
  bne _dm_len7_no
  lda cmd_buf+4
  cmp #'I'
  bne _dm_len7_no
  lda cmd_buf+5
  cmp #'D'
  bne _dm_len7_no
  lda cmd_buf+6
  cmp #'E'
  bne _dm_len7_no
  lda #11
  sta target_verb
  rts
_dm_len7_no:
  rts

_dm_len8:
  lda cmd_buf
  cmp #'O'
  bne :+
  lda cmd_buf+1
  cmp #'U'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'D'
  bne :+
  lda cmd_buf+4
  cmp #'O'
  bne :+
  lda cmd_buf+5
  cmp #'O'
  bne :+
  lda cmd_buf+6
  cmp #'R'
  bne :+
  lda cmd_buf+7
  cmp #'S'
  bne :+
  lda #32
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'E'
  bne :+
  lda cmd_buf+1
  cmp #'N'
  bne :+
  lda cmd_buf+2
  cmp #'T'
  bne :+
  lda cmd_buf+3
  cmp #'R'
  bne :+
  lda cmd_buf+4
  cmp #'A'
  bne :+
  lda cmd_buf+5
  cmp #'N'
  bne :+
  lda cmd_buf+6
  cmp #'C'
  bne :+
  lda cmd_buf+7
  cmp #'E'
  bne :+
  lda #64
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'U'
  bne :+
  lda cmd_buf+1
  cmp #'P'
  bne :+
  lda cmd_buf+2
  cmp #'S'
  bne :+
  lda cmd_buf+3
  cmp #'T'
  bne :+
  lda cmd_buf+4
  cmp #'R'
  bne :+
  lda cmd_buf+5
  cmp #'E'
  bne :+
  lda cmd_buf+6
  cmp #'A'
  bne :+
  lda cmd_buf+7
  cmp #'M'
  bne :+
  lda #4
  sta target_verb
  rts
:
  lda cmd_buf
  cmp #'B'
  bne _dm_len8_no
  lda cmd_buf+1
  cmp #'U'
  bne _dm_len8_no
  lda cmd_buf+2
  cmp #'I'
  bne _dm_len8_no
  lda cmd_buf+3
  cmp #'L'
  bne _dm_len8_no
  lda cmd_buf+4
  cmp #'D'
  bne _dm_len8_no
  lda cmd_buf+5
  cmp #'I'
  bne _dm_len8_no
  lda cmd_buf+6
  cmp #'N'
  bne _dm_len8_no
  lda cmd_buf+7
  cmp #'G'
  bne _dm_len8_no
  lda #12
  sta target_verb
  rts
_dm_len8_no:
  rts

_dm_len9:
  ; NORTHEAST / NORTHWEST / SOUTHEAST / SOUTHWEST
  lda cmd_buf
  cmp #'N'
  bne _dm_len9_se
  lda cmd_buf+1
  cmp #'O'
  bne _dm_len9_se
  lda cmd_buf+2
  cmp #'R'
  bne _dm_len9_se
  lda cmd_buf+3
  cmp #'T'
  bne _dm_len9_se
  lda cmd_buf+4
  cmp #'H'
  bne _dm_len9_se
  lda cmd_buf+5
  cmp #'E'
  beq _dm_len9_ne
  cmp #'W'
  beq _dm_len9_nw
  jmp _dm_len9_se
_dm_len9_ne:
  lda cmd_buf+6
  cmp #'A'
  bne _dm_len9_se
  lda cmd_buf+7
  cmp #'S'
  bne _dm_len9_se
  lda cmd_buf+8
  cmp #'T'
  bne _dm_len9_se
  lda #47
  sta target_verb
  rts
_dm_len9_nw:
  lda cmd_buf+6
  cmp #'E'
  bne _dm_len9_se
  lda cmd_buf+7
  cmp #'S'
  bne _dm_len9_se
  lda cmd_buf+8
  cmp #'T'
  bne _dm_len9_se
  lda #50
  sta target_verb
  rts
_dm_len9_se:
  lda cmd_buf
  cmp #'S'
  bne _dm_len9_no
  lda cmd_buf+1
  cmp #'O'
  bne _dm_len9_no
  lda cmd_buf+2
  cmp #'U'
  bne _dm_len9_no
  lda cmd_buf+3
  cmp #'T'
  bne _dm_len9_no
  lda cmd_buf+4
  cmp #'H'
  bne _dm_len9_no
  lda cmd_buf+5
  cmp #'E'
  beq _dm_len9_southeast
  cmp #'W'
  beq _dm_len9_southwest
  bne _dm_len9_no
_dm_len9_southeast:
  lda cmd_buf+6
  cmp #'A'
  bne _dm_len9_no
  lda cmd_buf+7
  cmp #'S'
  bne _dm_len9_no
  lda cmd_buf+8
  cmp #'T'
  bne _dm_len9_no
  lda #48
  sta target_verb
  rts
_dm_len9_southwest:
  lda cmd_buf+6
  cmp #'E'
  bne _dm_len9_no
  lda cmd_buf+7
  cmp #'S'
  bne _dm_len9_no
  lda cmd_buf+8
  cmp #'T'
  bne _dm_len9_no
  lda #49
  sta target_verb
  rts
_dm_len9_no:
  rts

_dm_len10:
  lda cmd_buf
  cmp #'D'
  bne _dm_len10_dep
  lda cmd_buf+1
  cmp #'O'
  bne _dm_len10_dep
  lda cmd_buf+2
  cmp #'W'
  bne _dm_len10_dep
  lda cmd_buf+3
  cmp #'N'
  bne _dm_len10_dep
  lda cmd_buf+4
  cmp #'S'
  bne _dm_len10_dep
  lda cmd_buf+5
  cmp #'T'
  bne _dm_len10_dep
  lda cmd_buf+6
  cmp #'R'
  bne _dm_len10_dep
  lda cmd_buf+7
  cmp #'E'
  bne _dm_len10_dep
  lda cmd_buf+8
  cmp #'A'
  bne _dm_len10_dep
  lda cmd_buf+9
  cmp #'M'
  bne _dm_len10_dep
  lda #4
  sta target_verb
  rts
_dm_len10_dep:
  lda cmd_buf
  cmp #'D'
  bne _dm_len10_no
  lda cmd_buf+1
  cmp #'E'
  bne _dm_len10_no
  lda cmd_buf+2
  cmp #'P'
  bne _dm_len10_no
  lda cmd_buf+3
  cmp #'R'
  bne _dm_len10_no
  lda cmd_buf+4
  cmp #'E'
  bne _dm_len10_no
  lda cmd_buf+5
  cmp #'S'
  bne _dm_len10_no
  lda cmd_buf+6
  cmp #'S'
  bne _dm_len10_no
  lda cmd_buf+7
  cmp #'I'
  bne _dm_len10_no
  lda cmd_buf+8
  cmp #'O'
  bne _dm_len10_no
  lda cmd_buf+9
  cmp #'N'
  bne _dm_len10_no
  lda #63
  sta target_verb
  rts
_dm_len10_no:
  rts

execute_action_command:
  lda command_type
  cmp #3
  bne _eac_chk_take
  jmp _eac_inventory
_eac_chk_take:
  cmp #1
  bne _eac_chk_drop
  jmp _eac_take
_eac_chk_drop:
  cmp #2
  bne _eac_chk_look
  jmp _eac_drop
_eac_chk_look:
  cmp #4
  bne _eac_chk_quit
  jmp _eac_look
_eac_chk_quit:
  cmp #5
  bne _eac_chk_examine
  jmp _eac_quit
_eac_chk_examine:
  cmp #6
  bne _eac_chk_read
  jmp _eac_examine
_eac_chk_read:
  cmp #7
  bne _eac_chk_parse
  jmp _eac_read
_eac_chk_parse:
  cmp #8
  bne _eac_chk_open
  jmp _eac_parse_error
_eac_chk_open:
  cmp #9
  bne _eac_chk_close
  jmp _eac_openish
_eac_chk_close:
  cmp #10
  bne _eac_chk_help
  jmp _eac_closeish
_eac_chk_help:
  cmp #11
  bne _eac_chk_light
  jmp _eac_help
_eac_chk_light:
  cmp #12
  bne _eac_chk_dark
  jmp _eac_light
_eac_chk_dark:
  cmp #13
  bne _eac_chk_eat
  jmp _eac_dark
_eac_chk_eat:
  cmp #14
  bne _eac_chk_drink
  jmp _eac_eat
_eac_chk_drink:
  cmp #15
  bne _eac_chk_fill
  jmp _eac_drink
_eac_chk_fill:
  cmp #16
  bne _eac_chk_pour
  jmp _eac_fill
_eac_chk_pour:
  cmp #17
  bne _eac_chk_wave
  jmp _eac_pour
_eac_chk_wave:
  cmp #18
  bne _eac_chk_find
  jmp _eac_wave
_eac_chk_find:
  cmp #19
  bne _eac_chk_say
  jmp _eac_find
_eac_chk_say:
  cmp #20
  bne _eac_chk_kill
  jmp _eac_say
_eac_chk_kill:
  cmp #21
  bne _eac_chk_throw
  jmp _eac_kill
_eac_chk_throw:
  cmp #22
  bne _eac_chk_feed
  jmp _eac_throw
_eac_chk_feed:
  cmp #23
  bne _eac_chk_break
  jmp _eac_feed
_eac_chk_break:
  cmp #24
  bne _eac_chk_rub
  jmp _eac_break
_eac_chk_rub:
  cmp #25
  bne _eac_chk_wake
  jmp _eac_rub
_eac_chk_wake:
  cmp #26
  bne _eac_chk_blast
  jmp _eac_wake
_eac_chk_blast:
  cmp #27
  bne _eac_chk_score
  jmp _eac_blast
_eac_chk_score:
  cmp #28
  bne _eac_chk_calm
  jmp _eac_score
_eac_chk_calm:
  cmp #29
  bne _eac_chk_walk2
  jmp _eac_calm
_eac_chk_walk2:
  cmp #30
  bne _eac_chk_nothing
  jmp _eac_walk
_eac_chk_nothing:
  cmp #31
  bne _eac_chk_brief
  jmp _eac_nothing
_eac_chk_brief:
  cmp #32
  bne _eac_chk_suspend
  jmp _eac_brief
_eac_chk_suspend:
  cmp #33
  bne _eac_chk_hours
  jmp _eac_suspend
_eac_chk_hours:
  cmp #34
  bne _eac_chk_log
  jmp _eac_hours
_eac_chk_log:
  cmp #35
  bne _eac_chk_load
  jmp _eac_log
_eac_chk_load:
  cmp #36
  bne _eac_chk_foo
  jmp _eac_load
_eac_chk_foo:
  cmp #37
  bne _eac_done
  jmp _eac_foo
_eac_done:
  rts

_eac_inventory:
  lda #'I'
  jsr print_char
  lda #'N'
  jsr print_char
  lda #'V'
  jsr print_char
  lda #':'
  jsr print_char
  lda #' '
  jsr print_char

  lda #0
  sta input_seen
  ldx #1
_eac_inv_loop:
  cpx #100
  bcs _eac_inv_done
  lda toting_table,x
  beq _eac_inv_next
  lda #1
  sta input_seen
  txa
  pha
  jsr print_inventory_item
  pla
  tax
  lda #' '
  jsr print_char
_eac_inv_next:
  inx
  jmp _eac_inv_loop

_eac_inv_done:
  lda input_seen
  bne _eac_inv_ok
  lda #'E'
  jsr print_char
  lda #'M'
  jsr print_char
  lda #'P'
  jsr print_char
  lda #'T'
  jsr print_char
  lda #'Y'
  jsr print_char
  jsr newline
  rts

_eac_inv_ok:
  jsr newline
  rts

_eac_take:
  jsr noun_is_all
  bcc _eac_take_one
  jmp _eac_take_all
_eac_take_one:
  lda command_object
  bne _eac_take_have_obj
  jmp _eac_no
_eac_take_have_obj:
  jsr object_is_portable
  bcs :+
  ldx #<msg_cant_take
  ldy #>msg_cant_take
  jsr print_zstr_xy
  jsr newline
  rts
:
  tax
  cpx #100
  bcc _eac_take_in_range
  jmp _eac_no
_eac_take_in_range:
  lda present_table,x
  bne _eac_take_present
  jmp _eac_take_not_here
_eac_take_present:
  ; Rug (obj 62) can't be taken while the dragon (obj 31) is alive.
  txa
  cmp #62
  bne _eac_take_do
  ldx #31
  lda prop_table,x
  bne _eac_take_do         ; dragon dead (prop != 0) — allow
  lda #153                 ; "The dragon looks rather nasty."
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: rts
_eac_take_do:
  ldx command_object       ; restore X in case dragon check changed it
  lda #0
  sta present_table,x
  lda #1
  sta toting_table,x
  ; Tally: removing a treasure from the building re-opens it.
  lda target_room
  cmp #3
  bne _eac_take_no_tally
  txa
  cmp #50
  bcc _eac_take_no_tally
  cmp #65
  bcs _eac_take_no_tally
  lda tally
  cmp #15
  bcs _eac_take_no_tally     ; already at max
  inc tally
_eac_take_no_tally:
  lda #$FF
  sta place_table,x
  ldx #<msg_taken_prefix
  ldy #>msg_taken_prefix
  jsr print_zstr_xy
  ldx command_object
  jsr print_inventory_item
  jsr newline
  rts

_eac_take_all:
  lda #0
  sta input_seen
  ldx #1
_eac_take_all_loop:
  cpx #100
  bcs _eac_take_all_done
  txa
  pha
  sta command_object
  jsr object_is_portable
  pla
  tax
  bcc _eac_take_all_next
  lda present_table,x
  beq _eac_take_all_next
  lda #1
  sta input_seen
  lda #0
  sta present_table,x
  lda #1
  sta toting_table,x
  lda #$FF
  sta place_table,x
  txa
  pha
  ldx #<msg_taken_prefix
  ldy #>msg_taken_prefix
  jsr print_zstr_xy
  pla
  pha
  tax
  jsr print_inventory_item
  pla
  tax
  jsr newline
_eac_take_all_next:
  inx
  jmp _eac_take_all_loop
_eac_take_all_done:
  lda input_seen
  bne _eac_take_all_ok
  jmp _eac_no
_eac_take_all_ok:
  rts

_eac_take_not_here:
  ldx #<msg_no_here_prefix
  ldy #>msg_no_here_prefix
  jsr print_zstr_xy
  ldx command_object
  jsr print_inventory_item
  ldx #<msg_here_suffix
  ldy #>msg_here_suffix
  jsr print_zstr_xy
  jsr newline
  rts

_eac_drop:
  jsr noun_is_all
  bcc _eac_drop_one
  jmp _eac_drop_all
_eac_drop_one:
  lda command_object
  bne _eac_drop_have_obj
  jmp _eac_no
_eac_drop_have_obj:
  tax
  cpx #100
  bcc _eac_drop_in_range
  jmp _eac_no
_eac_drop_in_range:
  lda toting_table,x
  bne _eac_drop_holding
  lda place_table,x
  cmp target_room
  bne :+
  jmp _eac_drop_already_here
:
  jmp _eac_drop_not_carrying
_eac_drop_holding:
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
  ; Tally: depositing a treasure at the building (room 3) closes it out.
  lda target_room
  cmp #3
  bne _eac_drop_no_tally
  txa
  cmp #50
  bcc _eac_drop_no_tally
  cmp #65
  bcs _eac_drop_no_tally
  lda tally
  beq _eac_drop_no_tally     ; already 0, don't underflow
  dec tally
  bne _eac_drop_no_tally     ; still >0, not done yet
  lda closing
  bne _eac_drop_no_tally     ; already triggered
  jsr trigger_closing
_eac_drop_no_tally:
  ldx #<msg_dropped_prefix
  ldy #>msg_dropped_prefix
  jsr print_zstr_xy
  ldx command_object
  jsr print_inventory_item
  jsr newline
  rts

_eac_drop_all:
  lda #0
  sta input_seen
  ldx #1
_eac_drop_all_loop:
  cpx #100
  bcs _eac_drop_all_done
  lda toting_table,x
  beq _eac_drop_all_next
  lda #1
  sta input_seen
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
  txa
  pha
  ldx #<msg_dropped_prefix
  ldy #>msg_dropped_prefix
  jsr print_zstr_xy
  pla
  pha
  tax
  jsr print_inventory_item
  pla
  tax
  jsr newline
_eac_drop_all_next:
  inx
  jmp _eac_drop_all_loop
_eac_drop_all_done:
  lda input_seen
  bne _eac_drop_all_ok
  jmp _eac_no
_eac_drop_all_ok:
  rts

_eac_drop_already_here:
  ldx #<msg_already_here
  ldy #>msg_already_here
  jsr print_zstr_xy
  jsr newline
  rts

_eac_drop_not_carrying:
  ldx #<msg_not_carrying_prefix
  ldy #>msg_not_carrying_prefix
  jsr print_zstr_xy
  ldx command_object
  jsr print_inventory_item
  jsr newline
  rts

_eac_look:
  jsr sync_present_from_room
  lda #0
  sta desc_mode
  jsr newline
  ldx #2
  lda prop_table,x
  bne _eac_look_lit
  lda target_room
  cmp #9
  bcc _eac_look_lit
  lda #16
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: rts
_eac_look_lit:
  jsr print_room_long
  jsr print_room_objects
  jsr newline
  rts

_eac_quit:
  ldx #<msg_quit_confirm
  ldy #>msg_quit_confirm
  jsr print_zstr_xy
  jsr newline
  jsr prompt_yes_no
  php
  jsr newline
  plp
  bcc _eac_quit_cancel
  lda #1
  sta gave_up
  ldx #<msg_quit_ok
  ldy #>msg_quit_ok
  jsr print_zstr_xy
  jsr newline
  jsr print_quit_summary
  jmp halt_loop
_eac_quit_cancel:
  rts

_eac_examine:
  lda command_object
  bne _eac_exam_have_obj
  jmp _eac_no
_eac_exam_have_obj:
  tax
  cpx #100
  bcc :+
  jmp _eac_no
:
  lda toting_table,x
  bne _eac_exam_ok
  lda present_table,x
  bne _eac_exam_ok
  jmp _eac_no
_eac_exam_ok:
  jsr print_examine_text
  jsr newline
  rts

_eac_read:
  lda command_object
  bne _eac_read_have_obj
  jmp _eac_no
_eac_read_have_obj:
  tax
  cpx #100
  bcc :+
  jmp _eac_no
:
  lda toting_table,x
  bne _eac_read_ok
  lda present_table,x
  bne _eac_read_ok
  jmp _eac_no
_eac_read_ok:
  jsr print_read_text
  jsr newline
  rts

_eac_openish:
  ; OPEN/UNLOCK supports GRATE and DOOR parity behavior.
  lda command_object
  cmp #9
  bne _eac_open_check_grate
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #9
  lda prop_table,x
  cmp #1
  beq _eac_open_door_54
  lda #111
  sta msg_target
  jmp _eac_open_door_print
_eac_open_door_54:
  lda #54
  sta msg_target
_eac_open_door_print:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_open_check_grate:
  lda command_object
  cmp #14                  ; CLAM
  bne :+
  jmp _eac_open_clam
:
  cmp #15                  ; OYSTER
  bne :+
  jmp _eac_open_clam
:
  cmp #63                  ; CHAIN
  bne :+
  jmp _eac_open_chain
:
  lda command_object
  cmp #3
  beq _eac_open_room_check
  cmp #4
  beq _eac_open_msg32
  cmp #1
  beq _eac_open_msg55
  lda #33
  sta msg_target
  jmp _eac_open_msg_only
_eac_open_msg32:
  lda #32
  sta msg_target
  jmp _eac_open_msg_only
_eac_open_msg55:
  lda #55
  sta msg_target
_eac_open_msg_only:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_open_room_check:
  lda target_room
  cmp #8
  bne :+
  jmp _eac_open_do
:
  cmp #9
  beq :+
  jmp _eac_no
:
_eac_open_do:
  ldx #3
  lda prop_table,x
  cmp #1
  bne :+
  jmp _eac_open_already
:
  jsr command_is_unlock
  bcc _eac_open_set
  jsr player_has_keys
  bcs _eac_open_set
  ldx #<msg_need_keys
  ldy #>msg_need_keys
  jsr print_zstr_xy
  jsr newline
  rts
_eac_open_set:
  ldx #3
  lda #1
  sta prop_table,x
  lda #36
  sta msg_target
  jsr newline
  jsr print_special_message
  bcc _eac_open_done
  ; Fallback if message lookup fails
  ldx #<msg_opened
  ldy #>msg_opened
  jsr print_zstr_xy
_eac_open_done:
  jsr newline
  rts
_eac_open_already:
  ldx #<msg_already_unlocked
  ldy #>msg_already_unlocked
  jsr print_zstr_xy
  jsr newline
  rts

_eac_open_clam:
  ; Adventure parity for OPEN CLAM/OYSTER:
  ; - no trident: 122/123
  ; - carrying object: 120/121
  ; - otherwise: 124/125 and CLAM->OYSTER with PEARL at 105
  lda command_object
  cmp #15
  bne :+
  lda #1
  bne _eac_open_clam_have_oy
:
  lda #0
_eac_open_clam_have_oy:
  sta tmp_digit
  ldx #57                  ; TRIDENT
  lda toting_table,x
  bne _eac_open_clam_have_trident
  lda #122
  clc
  adc tmp_digit
  sta msg_target
  jmp _eac_open_clam_print
_eac_open_clam_have_trident:
  ldx command_object
  lda toting_table,x
  beq _eac_open_clam_open_it
  lda #120
  clc
  adc tmp_digit
  sta msg_target
  jmp _eac_open_clam_print
_eac_open_clam_open_it:
  lda #124
  clc
  adc tmp_digit
  sta msg_target
  ; dstroy(CLAM)
  ldx #14
  lda #0
  sta toting_table,x
  sta present_table,x
  sta place_table,x
  ; drop(OYSTER, loc)
  ldx #15
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
  ; drop(PEARL, 105)
  ldx #61
  lda #0
  sta toting_table,x
  lda #105
  sta place_table,x
  lda target_room
  cmp #105
  bne :+
  lda #1
  bne :++
:
  lda #0
:
  sta present_table,x
_eac_open_clam_print:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_open_chain:
  ; OPEN/UNLOCK CHAIN (verb != LOCK path in original).
  jsr player_has_keys_here
  bcs :+
  lda #31
  sta msg_target
  jmp _eac_open_chain_print
:
  ldx #35                  ; BEAR
  lda prop_table,x
  bne :+
  lda #41
  sta msg_target
  jmp _eac_open_chain_print
:
  ldx #63                  ; CHAIN
  lda prop_table,x
  bne :+
  lda #37
  sta msg_target
  jmp _eac_open_chain_print
:
  lda #0
  sta prop_table,x
  ldx #35
  lda prop_table,x
  cmp #3
  beq :+
  lda #2
  sta prop_table,x
:
  lda #171
  sta msg_target
_eac_open_chain_print:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_closeish:
  ; CLOSE/LOCK supports GRATE and DOOR parity behavior.
  lda command_object
  cmp #9
  bne _eac_close_check_grate
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #9
  lda prop_table,x
  cmp #1
  beq _eac_close_door_54
  lda #111
  sta msg_target
  jmp _eac_close_door_print
_eac_close_door_54:
  lda #54
  sta msg_target
_eac_close_door_print:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_close_check_grate:
  lda command_object
  cmp #14                  ; CLAM
  bne :+
  jmp _eac_close_clam
:
  cmp #15                  ; OYSTER
  bne :+
  jmp _eac_close_clam
:
  cmp #63                  ; CHAIN
  bne :+
  jmp _eac_close_chain
:
  lda command_object
  cmp #3
  beq _eac_close_room_check
  cmp #4
  beq _eac_close_msg32
  cmp #1
  beq _eac_close_msg55
  lda #33
  sta msg_target
  jmp _eac_close_msg_only
_eac_close_msg32:
  lda #32
  sta msg_target
  jmp _eac_close_msg_only
_eac_close_msg55:
  lda #55
  sta msg_target
_eac_close_msg_only:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_close_room_check:
  lda target_room
  cmp #8
  bne :+
  jmp _eac_close_do
:
  cmp #9
  beq :+
  lda #33
  sta msg_target
  jmp _eac_close_msg_only
:
_eac_close_do:
  ldx #3
  lda prop_table,x
  cmp #0
  bne :+
  jmp _eac_close_already
:
  jsr command_is_lock
  bcs :+
  jmp _eac_close_set
:
  jsr player_has_keys
  bcc :+
  jmp _eac_close_set
:
  ldx #<msg_need_keys
  ldy #>msg_need_keys
  jsr print_zstr_xy
  jsr newline
  rts
_eac_close_set:
  ldx #3
  lda #0
  sta prop_table,x
  lda #35
  sta msg_target
  jsr newline
  jsr print_special_message
  bcc _eac_close_done
  ; Fallback if message lookup fails
  ldx #<msg_closed
  ldy #>msg_closed
  jsr print_zstr_xy
_eac_close_done:
  jsr newline
  rts
_eac_close_already:
  ldx #<msg_already_locked
  ldy #>msg_already_locked
  jsr print_zstr_xy
  jsr newline
  rts

_eac_close_clam:
  ; LOCK/CLOSE CLAM or OYSTER -> message 61.
  lda #61
  sta msg_target
  jsr newline
  jsr print_special_message
  bcc :+
  ldx #<msg_clam_lock
  ldy #>msg_clam_lock
  jsr print_zstr_xy
:
  jsr newline
  rts

_eac_close_chain:
  ; CLOSE/LOCK CHAIN (verb == LOCK path in original).
  jsr player_has_keys_here
  bcs :+
  lda #31
  sta msg_target
  jmp _eac_close_chain_print
:
  ldx #63                  ; CHAIN
  lda prop_table,x
  beq :+
  lda #34
  sta msg_target
  jmp _eac_close_chain_print
:
  lda target_room
  cmp #130
  beq :+
  lda #173
  sta msg_target
  jmp _eac_close_chain_print
:
  ldx #63
  lda #2
  sta prop_table,x
  lda toting_table,x
  beq :+
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
:
  lda #172
  sta msg_target
_eac_close_chain_print:
  jsr newline
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_help:
  ldx #<msg_help_1
  ldy #>msg_help_1
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_help_2
  ldy #>msg_help_2
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_help_3
  ldy #>msg_help_3
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_help_4
  ldy #>msg_help_4
  jsr print_zstr_xy
  jsr newline
  rts

_eac_light:
  lda command_object
  cmp #2
  beq :+
  jmp _eac_no
:
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #2
  lda prop_table,x
  beq :+
  ldx #<msg_lamp_already_on
  ldy #>msg_lamp_already_on
  jsr print_zstr_xy
  jsr newline
  rts
:
  lda #1
  sta prop_table,x
  ldx #<msg_lamp_on
  ldy #>msg_lamp_on
  jsr print_zstr_xy
  jsr newline
  rts

_eac_dark:
  lda command_object
  cmp #2
  beq :+
  jmp _eac_no
:
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #2
  lda prop_table,x
  bne :+
  ldx #<msg_lamp_already_off
  ldy #>msg_lamp_already_off
  jsr print_zstr_xy
  jsr newline
  rts
:
  lda #0
  sta prop_table,x
  ldx #<msg_lamp_off
  ldy #>msg_lamp_off
  jsr print_zstr_xy
  jsr newline
  rts

_eac_eat:
  lda command_object
  cmp #19
  beq :+
  jmp _eac_no
:
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #19
  lda #0
  sta toting_table,x
  sta present_table,x
  sta place_table,x
  ldx #<msg_eat_ok
  ldy #>msg_eat_ok
  jsr print_zstr_xy
  jsr newline
  rts

_eac_drink:
  lda command_object
  cmp #21
  bne _eac_drink_not_water_obj
  lda #20
  sta command_object
_eac_drink_not_water_obj:
  lda command_object
  cmp #20
  beq _eac_drink_obj_ok
  lda #110
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_drink_obj_ok:
  jsr object_is_accessible
  bcs _eac_drink_have_bottle
  jsr room_has_water
  bcs _eac_drink_stream
  lda #110
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_drink_stream:
  ldx #<msg_drink_ok
  ldy #>msg_drink_ok
  jsr print_zstr_xy
  jsr newline
  rts
_eac_drink_have_bottle:
  ldx #20
  lda prop_table,x
  bne _eac_drink_have_water
  ldx #<msg_no_water
  ldy #>msg_no_water
  jsr print_zstr_xy
  jsr newline
  rts
_eac_drink_have_water:
  lda #0
  sta prop_table,x
  ldx #<msg_drink_ok
  ldy #>msg_drink_ok
  jsr print_zstr_xy
  jsr newline
  rts

_eac_fill:
  lda command_object
  cmp #2                   ; LAMP
  beq _eac_fill_lamp
  cmp #58                  ; VASE
  bne :+
  jmp _eac_fill_vase
: cmp #20
  beq _eac_fill_bottle
  lda #29
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_fill_bottle:
  ldx #20
  lda toting_table,x
  bne :+
  jmp _eac_no
:
  jsr room_has_water
  bcs :+
  ldx #<msg_no_water_here
  ldy #>msg_no_water_here
  jsr print_zstr_xy
  jsr newline
  rts
:
  ldx #20
  lda prop_table,x
  beq :+
  ldx #<msg_bottle_full
  ldy #>msg_bottle_full
  jsr print_zstr_xy
  jsr newline
  rts
:
  lda #1
  sta prop_table,x
  ldx #<msg_fill_ok
  ldy #>msg_fill_ok
  jsr print_zstr_xy
  jsr newline
  rts

_eac_fill_lamp:
  ; Refuel lamp with fresh batteries (obj 39).
  ; Batteries must be toted or present, and fresh (prop=0).
  ldx #39
  lda toting_table,x
  bne _efl_have_bat
  lda present_table,x
  bne _efl_have_bat
  ; No batteries available
  lda #29
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
: jsr newline
  rts
_efl_have_bat:
  ldx #39
  lda prop_table,x
  bne _efl_exhausted       ; prop=1 means used up
  ; Fresh batteries: add 2500 turns ($09C4) to lamp_life
  lda lamp_life_lo
  clc
  adc #$C4
  sta lamp_life_lo
  bcc :+
  inc lamp_life_hi
: lda lamp_life_hi
  clc
  adc #$09
  sta lamp_life_hi
  ldx #39
  lda #1
  sta prop_table,x         ; mark batteries as exhausted
  lda #0
  sta lamp_warned           ; reset dim warning
  lda #188
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: rts
_efl_exhausted:
  lda #29
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
: jsr newline
  rts

_eac_fill_vase:
  jsr room_has_water
  bcs :+
  lda #144
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
:
  ldx #58
  lda toting_table,x
  bne :+
  lda #29
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
:
  lda #145
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  ; vdrop(VASE)-like simplification after filling.
  ldx #58
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
  ldx #10                  ; PILLOW
  lda present_table,x
  bne :+
  lda toting_table,x
  bne :+
  ldx #58
  lda #2
  sta prop_table,x
  rts
:
  ldx #58
  lda #0
  sta prop_table,x
  rts

_eac_pour:
  lda command_object
  cmp #20
  beq _eac_pour_bottle
  cmp #21                  ; WATER
  beq _eac_pour_liquid_obj
  cmp #22                  ; OIL
  beq _eac_pour_liquid_obj
  lda #78
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_pour_bottle:
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ldx #20
  lda prop_table,x
  bne :+
  ldx #<msg_bottle_empty
  ldy #>msg_bottle_empty
  jsr print_zstr_xy
  jsr newline
  rts
:
  lda #0
  sta prop_table,x
  ldx #<msg_pour_ok
  ldy #>msg_pour_ok
  jsr print_zstr_xy
  jsr newline
  rts

_eac_pour_liquid_obj:
  ; Simplified liquid handling: require accessible full bottle.
  ldx #20
  lda toting_table,x
  bne :+
  lda present_table,x
  bne :+
  lda #78
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
:
  lda prop_table,x
  bne :+
  lda #78
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
:
  lda #0
  sta prop_table,x
  ldx #9                   ; DOOR
  lda present_table,x
  beq _eac_pour_liquid_default
  lda command_object
  cmp #22
  bne _eac_pour_liquid_water
  ldx #9
  lda #1
  sta prop_table,x
  lda #114
  sta msg_target
  jmp _eac_pour_liquid_print
_eac_pour_liquid_water:
  ldx #9
  lda #0
  sta prop_table,x
  lda #113
  sta msg_target
  jmp _eac_pour_liquid_print
_eac_pour_liquid_default:
  lda #77
  sta msg_target
_eac_pour_liquid_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_wave:
  lda command_object
  cmp #5
  beq :+
  jmp _eac_no
:
  jsr object_is_accessible
  bcs :+
  jmp _eac_no
:
  ; Adventure parity: waving the rod at the fissure toggles the bridge.
  ldx #12
  lda present_table,x
  bne :+
  jmp _eac_wave_nothing
:
  lda prop_table,x
  bne _eac_wave_close_bridge
  lda #1
  sta prop_table,x
  ldx #<msg_bridge_now_spans
  ldy #>msg_bridge_now_spans
  jsr print_zstr_xy
  jmp _eac_wave_done
_eac_wave_close_bridge:
  lda #0
  sta prop_table,x
  ldx #<msg_bridge_vanished
  ldy #>msg_bridge_vanished
  jsr print_zstr_xy
  jmp _eac_wave_done
_eac_wave_done:
  jsr newline
  rts
_eac_wave_nothing:
  ldx #<msg_nothing_happens
  ldy #>msg_nothing_happens
  jsr print_zstr_xy
  jsr newline
  rts

_eac_find:
  lda command_object
  bne :+
  jmp _eac_no
:
  tax
  cpx #100
  bcc :+
  jmp _eac_no
:
  lda toting_table,x
  beq _eac_find_check_here
  lda #24
  sta msg_target
  jmp _eac_find_print
_eac_find_check_here:
  ; Adventure parity: WATER is findable if local water exists
  ; or if the carried bottle currently contains water.
  ; Parser maps WATER to object id 21.
  lda command_object
  cmp #21
  bne _eac_find_check_present
  ldx #20
  lda toting_table,x
  beq _eac_find_check_room_water
  lda prop_table,x
  cmp #1
  beq _eac_find_here
_eac_find_check_room_water:
  jsr room_has_water
  bcc _eac_find_not_found
_eac_find_here:
  lda #94
  sta msg_target
  jmp _eac_find_print
_eac_find_check_present:
  lda present_table,x
  beq _eac_find_not_found
  lda #94
  sta msg_target
  jmp _eac_find_print
_eac_find_not_found:
  lda #59
  sta msg_target
_eac_find_print:
  jsr print_special_message
  bcc :+
  ldx #<msg_exam_default
  ldy #>msg_exam_default
  jsr print_zstr_xy
:
  jsr newline
  rts

_eac_say:
  ldx #<msg_okay
  ldy #>msg_okay
  jsr print_zstr_xy
  jsr newline
  lda noun_len
  bne :+
  rts
:
  ldx #0
_eac_say_loop:
  cpx noun_len
  bcs _eac_say_done
  lda noun_buf,x
  jsr print_char
  inx
  jmp _eac_say_loop
_eac_say_done:
  jsr newline
  rts

_eac_kill:
  lda command_object
  bne _eac_kill_have_obj
  lda #44
  sta msg_target
  jmp _eac_kill_print
_eac_kill_have_obj:
  cmp #8                   ; BIRD
  beq _eac_kill_bird
  cmp #14                  ; CLAM
  beq _eac_kill_clam
  cmp #15                  ; OYSTER
  beq _eac_kill_clam
  cmp #11                  ; SNAKE
  beq _eac_kill_snake
  cmp #17                  ; DWARF
  beq _eac_kill_dwarf
  cmp #33                  ; TROLL
  beq _eac_kill_troll
  cmp #31                  ; DRAGON
  beq _eac_kill_dragon
  cmp #35                  ; BEAR
  bne :+
  jmp _eac_kill_bear
: jmp _eac_no
_eac_kill_bird:
  jsr object_is_accessible
  bcs :+
  lda #44
  sta msg_target
  jmp _eac_kill_print
:
  ldx #8
  lda #0
  sta toting_table,x
  sta present_table,x
  sta place_table,x
  sta prop_table,x
  lda #45
  sta msg_target
  jmp _eac_kill_print
_eac_kill_snake:
  lda #46
  sta msg_target
  jmp _eac_kill_print
_eac_kill_clam:
  lda #150
  sta msg_target
  jmp _eac_kill_print
_eac_kill_dwarf:
  lda ndwarves
  beq _ekdw_none
  lda target_room
  cmp #15
  bcc _ekdw_none
  dec ndwarves             ; kill one dwarf
  lda #149                 ; "You killed a little Dwarf. The body vanished..."
  sta msg_target
  jmp _eac_kill_print
_ekdw_none:
  lda #12                  ; "I don't know how to apply that word here."
  sta msg_target
  jmp _eac_kill_print
_eac_kill_troll:
  lda #157
  sta msg_target
  jmp _eac_kill_print
_eac_kill_dragon:
  ldx #31
  lda prop_table,x
  bne _ekd_already_dead
  ; Dragon is alive — prompt bare-hands attempt
  lda #49                  ; "With what? Your bare hands?"
  sta msg_target
  jsr print_special_message
  bcs _ekd_done
  jsr newline
  jsr prompt_yes_no
  php
  jsr newline
  plp
  bcc _ekd_done            ; player said NO
  ; YES — slay the dragon
  ldx #31
  lda #1
  sta prop_table,x         ; dragon dead
  lda #0
  sta place_table,x        ; remove from world
  sta present_table,x
  ldx #<msg_dragon_slain
  ldy #>msg_dragon_slain
  jsr print_zstr_xy
  jsr newline
  rts
_ekd_already_dead:
  lda #167
  sta msg_target
_ekd_done:
  jmp _eac_kill_print
_eac_kill_bear:
  lda #165
  sta msg_target
_eac_kill_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_throw:
  lda command_object
  bne _eac_throw_have_obj
  jmp _eac_no
_eac_throw_have_obj:
  tax
  lda toting_table,x
  bne _eac_throw_check_axe
  lda #29
  sta msg_target
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts
_eac_throw_check_axe:
  ; Throwing food at a nearby bear follows FEED behavior.
  lda command_object
  cmp #19                  ; FOOD
  bne :+
  ldx #35                  ; BEAR
  lda present_table,x
  beq :+
  lda #35
  sta command_object
  jmp _eac_feed
:
  lda command_object
  cmp #28                  ; AXE special case
  beq _eac_throw_axe
  ; Non-axe: if troll is present and object is a treasure (50..79), bribe him
  ldx #33                  ; TROLL
  lda present_table,x
  beq _eac_throw_not_troll
  lda command_object
  cmp #50
  bcc _eac_throw_not_troll
  cmp #80
  bcs _eac_throw_not_troll
  ; Troll catches the treasure — remove it and mark troll as bribed
  ldx command_object
  lda #0
  sta toting_table,x
  sta present_table,x
  sta place_table,x
  ldx #33                  ; TROLL: prop=1 means bribed (player may now cross)
  lda #1
  sta prop_table,x
  lda #159
  sta msg_target
  jsr print_special_message
  bcc :+
  rts
: jsr newline
  rts
_eac_throw_not_troll:
  jmp _eac_drop
_eac_throw_axe:
  ldx #31                  ; DRAGON
  lda present_table,x
  beq _eac_throw_chk_troll
  lda prop_table,x
  bne _eac_throw_chk_troll
  lda #152
  sta msg_target
  jmp _eac_throw_axe_done
_eac_throw_chk_troll:
  ldx #33                  ; TROLL
  lda present_table,x
  beq _eac_throw_chk_bear
  lda #158
  sta msg_target
  jmp _eac_throw_axe_done
_eac_throw_chk_bear:
  ldx #35                  ; BEAR
  lda present_table,x
  beq _eac_throw_axe_default
  lda prop_table,x
  bne _eac_throw_axe_default
  lda #164
  sta msg_target
  jmp _eac_throw_axe_done
_eac_throw_axe_default:
  lda #49
  sta msg_target
_eac_throw_axe_done:
  ldx #28
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_feed:
  lda command_object
  cmp #8                   ; BIRD
  beq _eac_feed_bird
  cmp #17                  ; DWARF
  beq _eac_feed_dwarf
  cmp #11                  ; SNAKE
  beq _eac_feed_snake
  cmp #35                  ; BEAR
  beq _eac_feed_bear
  cmp #31                  ; DRAGON
  bne :+
  jmp _eac_feed_dragon
:
  cmp #33                  ; TROLL
  bne :+
  jmp _eac_feed_troll
:
  lda #14
  sta msg_target
  jmp _eac_feed_print
_eac_feed_bird:
  lda #100
  sta msg_target
  jmp _eac_feed_print
_eac_feed_dwarf:
  ldx #19                  ; FOOD
  lda present_table,x
  bne :+
  lda toting_table,x
  bne :+
  lda #110
  sta msg_target
  jmp _eac_feed_print
:
  lda #103
  sta msg_target
  jmp _eac_feed_print
_eac_feed_snake:
  ldx #8
  lda present_table,x
  bne _eac_feed_snake_yes
  lda toting_table,x
  bne _eac_feed_snake_yes
  lda #102
  sta msg_target
  jmp _eac_feed_print
_eac_feed_snake_yes:
  lda #0
  sta present_table,x      ; x=8 (BIRD) — bird used up charming snake
  sta toting_table,x
  sta place_table,x
  ldx #11                  ; SNAKE — remove from world and mark as gone
  lda #0
  sta present_table,x
  sta toting_table,x
  sta place_table,x
  lda #1
  sta prop_table,x          ; prop[snake]=1: snake charmed/gone (unblocks cond 311)
  lda #101
  sta msg_target
  jmp _eac_feed_print
_eac_feed_bear:
  ldx #19                  ; FOOD
  lda present_table,x
  bne _eac_feed_bear_eat
  lda toting_table,x
  bne _eac_feed_bear_eat
  ldx #35
  lda prop_table,x
  cmp #3
  bcc :+
  lda #110
  sta msg_target
  jmp _eac_feed_print
:
  lda #102
  sta msg_target
  jmp _eac_feed_print
_eac_feed_bear_eat:
  ldx #19
  lda #0
  sta present_table,x
  sta toting_table,x
  sta place_table,x
  ldx #35
  lda #1
  sta prop_table,x
  lda #168
  sta msg_target
  jmp _eac_feed_print
_eac_feed_dragon:
  ldx #31
  lda prop_table,x
  beq :+
  lda #110
  sta msg_target
  jmp _eac_feed_print
:
  lda #102
  sta msg_target
  jmp _eac_feed_print
_eac_feed_troll:
  lda #182
  sta msg_target
_eac_feed_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_break:
  lda command_object
  cmp #23                  ; MIRROR
  beq _eac_break_mirror
  cmp #58                  ; VASE
  beq _eac_break_vase
  jmp _eac_no
_eac_break_mirror:
  lda #148
  sta msg_target
  jmp _eac_break_print
_eac_break_vase:
  ldx #58
  lda prop_table,x
  bne _eac_break_done
  lda #2
  sta prop_table,x
  lda toting_table,x
  beq :+
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
:
  lda #198
  sta msg_target
  jmp _eac_break_print
_eac_break_done:
  jmp _eac_no
_eac_break_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_rub:
  lda command_object
  cmp #2
  bne _eac_rub_other
  lda #75
  sta msg_target
  jmp _eac_rub_print
_eac_rub_other:
  lda #76
  sta msg_target
_eac_rub_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_wake:
  lda command_object
  cmp #17                  ; DWARF
  bne _eac_wake_default
  lda #199
  sta msg_target
  jmp _eac_wake_print
_eac_wake_default:
  lda #110
  sta msg_target
_eac_wake_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

_eac_blast:
  lda closed
  beq _eac_blast_generic          ; not in repository state — print generic msg
  ; Determine bonus based on location and ROD2 presence
  lda #133                        ; default: 45 pts (best outcome)
  sta bonus
  lda target_room
  cmp #115
  bne _eac_blast_check_rod2
  lda #134                        ; at NE repository: 30 pts
  sta bonus
_eac_blast_check_rod2:
  ldx #6
  lda present_table,x             ; is ROD2 present here?
  beq _eac_blast_do
  lda #135                        ; ROD2 here: 25 pts (worst)
  sta bonus
_eac_blast_do:
  lda bonus
  cmp #134
  beq _eac_blast_print_134
  cmp #135
  beq _eac_blast_print_135
  ; bonus == 133
  ldx #<msg_blast_133
  ldy #>msg_blast_133
  jmp _eac_blast_print
_eac_blast_print_134:
  ldx #<msg_blast_134
  ldy #>msg_blast_134
  jmp _eac_blast_print
_eac_blast_print_135:
  ldx #<msg_blast_135
  ldy #>msg_blast_135
_eac_blast_print:
  jsr print_zstr_xy
  jsr newline
  jsr print_quit_summary
  jmp halt_loop
_eac_blast_generic:
  lda #67
  sta msg_target
  jmp _eac_verbmsg_print

_eac_score:
  lda noun_len
  beq :+
  lda #13
  sta msg_target
  jmp _eac_verbmsg_print
:
  jsr print_score
  rts

_eac_calm:
  lda #14
  sta msg_target
  jmp _eac_verbmsg_print

_eac_walk:
  lda #43
  sta msg_target
  jmp _eac_verbmsg_print

_eac_nothing:
  lda #54
  sta msg_target
  jmp _eac_verbmsg_print

_eac_brief:
  lda #1
  sta desc_mode            ; switch to brief (short) descriptions
  lda #156                 ; "Okay, from now on I'll only describe a place in full..."
  sta msg_target
  jmp _eac_verbmsg_print

_eac_suspend:
  lda noun_len
  beq :+
  lda #13
  sta msg_target
  jmp _eac_verbmsg_print
:
  jsr save_game_snapshot
  bcs _eac_suspend_fail
  ldx #<msg_game_saved
  ldy #>msg_game_saved
  jsr print_zstr_xy
  jsr newline
  rts
_eac_suspend_fail:
  ldx #<msg_save_failed
  ldy #>msg_save_failed
  jsr print_zstr_xy
  jsr newline
  rts

_eac_hours:
  lda #13
  sta msg_target
  jmp _eac_verbmsg_print

_eac_log:
  lda #13
  sta msg_target
  jmp _eac_verbmsg_print

_eac_load:
  lda noun_len
  beq :+
  lda #13
  sta msg_target
  jmp _eac_verbmsg_print
:
  jsr load_game_snapshot
  bcs _eac_load_fail
  ldx #<msg_game_restored
  ldy #>msg_game_restored
  jsr print_zstr_xy
  jsr newline
  jsr print_room_long
  jsr print_room_objects
  ldx target_room
  lda #1
  sta visited_table,x
  jsr newline
  rts
_eac_load_fail:
  ldx #<msg_no_saved_game
  ldy #>msg_no_saved_game
  jsr print_zstr_xy
  jsr newline
  rts

_eac_foo:
  lda #147
  sta msg_target
  jmp _eac_verbmsg_print

_eac_verbmsg_print:
  jsr print_special_message
  bcc :+
  jmp _eac_no
:
  jsr newline
  rts

trigger_closing:
  ; Called when all 15 treasures have been deposited at the building.
  ; Sets the closing flag (+25 pts), warns the player, and teleports
  ; them to the repository (room 115). Also sets closed for end-game BLAST.
  lda #1
  sta closing
  sta closed
  ldx #<msg_cave_closing
  ldy #>msg_cave_closing
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_teleport
  ldy #>msg_teleport
  jsr print_zstr_xy
  jsr newline
  lda #115
  sta target_room
  jsr sync_present_from_room
  lda #0
  sta desc_mode               ; force long description
  jsr print_room_long
  jsr print_room_objects
  ldx target_room
  lda #1
  sta visited_table,x
  rts

player_death:
  ; Increment death counter. If >= 3 lives used, end the game.
  ; Otherwise offer resurrection: respawn at room 1, drop all items.
  inc num_deaths
  lda num_deaths
  cmp #3
  bcs _pd_game_over
  ldx #<msg_death
  ldy #>msg_death
  jsr print_zstr_xy
  jsr newline
  ldx #<msg_resurrect
  ldy #>msg_resurrect
  jsr print_zstr_xy
  jsr newline
  jsr prompt_yes_no
  php
  jsr newline
  plp
  bcc _pd_decline
  ; Drop all carried items where the player is (death room).
  ldx #1
_pd_drop_loop:
  cpx #100
  bcs _pd_drop_done
  lda toting_table,x
  beq _pd_drop_next
  lda #0
  sta toting_table,x
  lda #1
  sta present_table,x
  lda target_room
  sta place_table,x
_pd_drop_next:
  inx
  jmp _pd_drop_loop
_pd_drop_done:
  ; Respawn at room 1.
  lda #1
  sta target_room
  jsr sync_present_from_room
  lda #0
  sta desc_mode
  jsr newline
  jsr print_room_long
  jsr print_room_objects
  jsr newline
  rts
_pd_decline:
_pd_game_over:
  jsr print_quit_summary
  jmp halt_loop

print_score:
  ; Scoring per original Adventure:
  ; - treasure discovery/return points
  ; - survival bonus, gave-up bonus, dflag bonus, closing bonus
  ; - magazine-at-witt bonus
  ; - fixed baseline +2
  jsr compute_score
  lda #'S'
  jsr print_char
  lda #'C'
  jsr print_char
  lda #'O'
  jsr print_char
  lda #'R'
  jsr print_char
  lda #'E'
  jsr print_char
  lda #':'
  jsr print_char
  lda #' '
  jsr print_char
  jsr print_u16_dec
  jsr newline
  rts

print_quit_summary:
  ldx #<msg_treasures_label
  ldy #>msg_treasures_label
  jsr print_zstr_xy
  jsr compute_treasures
  jsr print_u16_dec
  jsr newline

  ldx #<msg_survival_label
  ldy #>msg_survival_label
  jsr print_zstr_xy
  ldx num_deaths
  lda survival_table,x
  sta work_lo
  lda #0
  sta work_hi
  jsr print_u16_dec
  jsr newline

  ldx #<msg_score_label
  ldy #>msg_score_label
  jsr print_zstr_xy
  jsr compute_score
  jsr print_u16_dec
  jsr newline
  rts

compute_treasures:
  lda #0
  sta work_lo
  sta work_hi
  ldx #50
_ct_loop:
  cpx #80
  bcs _ct_done
  lda prop_table,x
  bmi _ct_next
  lda place_table,x
  cmp #3
  bne _ct_next
  inc work_lo
  bne _ct_next
  inc work_hi
_ct_next:
  inx
  jmp _ct_loop
_ct_done:
  rts

compute_score:
  lda #0
  sta work_lo
  sta work_hi
  ldx #50
_cs_loop:
  cpx #80                  ; MAXTRS=79 in original
  bcs _cs_done_treasures
  lda prop_table,x
  bmi _cs_skip_seen
  lda #2
  jsr score_add_a
_cs_skip_seen:
  lda prop_table,x
  bne _cs_next
  lda place_table,x
  cmp #3
  bne _cs_next
  cpx #55                  ; CHEST
  beq _cs_add_chest
  bcc _cs_add_regular
  lda #14                  ; k-2 where k=16
  bne _cs_add_bonus
_cs_add_chest:
  lda #12                  ; k-2 where k=14
  bne _cs_add_bonus
_cs_add_regular:
  lda #10                  ; k-2 where k=12
_cs_add_bonus:
  jsr score_add_a
_cs_next:
  inx
  jmp _cs_loop

_cs_done_treasures:
  ldx #16                  ; MAGAZINE
  lda place_table,x
  cmp #108
  bne _cs_no_mag_bonus
  lda #1
  jsr score_add_a
_cs_no_mag_bonus:
  lda #2
  jsr score_add_a           ; baseline +2
  ; Survival: (3 - num_deaths) * 10
  ldx num_deaths
  lda survival_table,x
  jsr score_add_a
  ; Gave-up bonus: +4 if game ended without QUIT
  lda gave_up
  bne _cs_no_gaveup_bonus
  lda #4
  jsr score_add_a
_cs_no_gaveup_bonus:
  ; Exploration bonus: +25 once deep cave reached
  lda dflag
  beq _cs_no_dflag_bonus
  lda #25
  jsr score_add_a
_cs_no_dflag_bonus:
  ; Cave-closing bonus: +25 once all treasures returned
  lda closing
  beq _cs_no_closing_bonus
  lda #25
  jsr score_add_a
_cs_no_closing_bonus:
  lda closed
  beq _cs_done
  ldx bonus
  cpx #133
  beq _cs_bonus_133
  cpx #134
  beq _cs_bonus_134
  cpx #135
  beq _cs_bonus_135
  ; bonus == 0: +10 (entered repository but did not blast)
  lda #10
  jsr score_add_a
  jmp _cs_done
_cs_bonus_133:
  lda #45
  jsr score_add_a
  jmp _cs_done
_cs_bonus_134:
  lda #30
  jsr score_add_a
  jmp _cs_done
_cs_bonus_135:
  lda #25
  jsr score_add_a
_cs_done:
  rts

survival_table:
  .byte 30, 20, 10, 0      ; indexed by num_deaths (0..3)

score_add_a:
  clc
  adc work_lo
  sta work_lo
  bcc :+
  inc work_hi
:
  rts

; ---------------------------------------------------------------------------
; Dark room check: if lamp is off and player is underground (room >= 9),
; there is a 25% chance per turn of falling into a pit (msg 23).
; ---------------------------------------------------------------------------
dark_room_check:
  ldx #2                   ; LAMP
  lda prop_table,x
  bne _drc_done            ; lamp on — safe
  lda target_room
  cmp #9
  bcc _drc_done            ; rooms 1-8 are surface/lit
  ; Dark cave room — 25% chance of fatal pit fall
  jsr rng_next_byte
  cmp #64                  ; < 64/255 ≈ 25%
  bcs _drc_done
  lda #23                  ; "You fell into a pit and broke every bone in your body!"
  sta msg_target
  jsr print_special_message
  bcs _drc_done
  jsr newline
  jsr player_death
_drc_done:
  rts

; ---------------------------------------------------------------------------
; Pirate AI: each turn in deep cave, ~20% chance pirate appears.
; If player carries any treasure, he steals all of them (msg 128).
; If no treasure, he's spotted carrying chest and flees (msg 186).
; pflag=1 after first theft — pirate won't steal again.
; ---------------------------------------------------------------------------
pirate_ai:
  lda pflag
  bne _pai_done            ; already stolen once
  lda dflag
  beq _pai_done            ; only active in deep cave
  jsr rng_next_byte        ; random 0-254
  cmp #50                  ; ~20% chance per turn
  bcs _pai_done            ; not this turn
  ; Scan for any carried treasure (obj 50..79)
  ldx #50
_pai_scan:
  lda toting_table,x
  bne _pai_steal
  inx
  cpx #80
  bcc _pai_scan
  ; No treasure — pirate spotted, flees with chest (msg 186)
  lda #186
  sta msg_target
  jsr print_special_message
  bcs _pai_done
  jsr newline
  jmp _pai_done
_pai_steal:
  ; Pirate pounces and steals all carried treasures → room 114 (msg 128)
  lda #128
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: ldx #50
_pai_take_loop:
  lda toting_table,x
  beq _pai_take_next
  lda #0
  sta toting_table,x
  lda #114
  sta place_table,x
_pai_take_next:
  inx
  cpx #80
  bcc _pai_take_loop
  ; Place chest (obj 55) at room 114 (pirate's lair)
  ldx #55
  lda #114
  sta place_table,x
  lda #0
  sta toting_table,x
  lda #1
  sta pflag                ; won't steal again
_pai_done:
  rts

; Attack-probability lookup: index = ndwarves (0-5), value = threshold (0-255)
; pct_roll < threshold means a dwarf attacks this turn.
_ptu_attack_thresh:
  .byte 0, 50, 100, 150, 200, 250

per_turn_updates:
  jsr dark_room_check      ; 25% pit-death chance in dark cave rooms
  jsr pirate_ai            ; check pirate encounter each turn
  ; ----------------------------------------------------------------
  ; Lamp fuel countdown.
  ; ----------------------------------------------------------------
  ldx #2                   ; LAMP object
  lda prop_table,x
  beq _ptu_lamp_skip       ; lamp is off — no fuel consumed
  ; Check if already dead
  lda lamp_life_lo
  ora lamp_life_hi
  beq _ptu_lamp_dead
  ; Decrement 16-bit lamp_life
  lda lamp_life_lo
  bne :+
  dec lamp_life_hi
: dec lamp_life_lo
  ; Warn once when life drops to 30 or below
  lda lamp_warned
  bne _ptu_dead_check
  lda lamp_life_hi
  bne _ptu_dead_check      ; hi != 0 means life > 255, plenty left
  lda lamp_life_lo
  cmp #31
  bcs _ptu_dead_check      ; life > 30, not yet
  lda #183                 ; "Your lamp is getting dim..."
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: lda #1
  sta lamp_warned
  jmp _ptu_dead_check
_ptu_lamp_dead:
  ldx #2
  lda #0
  sta prop_table,x         ; lamp goes out
  lda #184                 ; "Your lamp has run out of power."
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jmp _ptu_lamp_skip
_ptu_dead_check:
  lda lamp_life_lo
  ora lamp_life_hi
  bne _ptu_lamp_skip
  ldx #2
  lda prop_table,x
  beq _ptu_lamp_skip
  jmp _ptu_lamp_dead
_ptu_lamp_skip:
  ; ----------------------------------------------------------------
  ; Dwarf AI.
  ; ----------------------------------------------------------------
  lda ndwarves
  bne :+
  rts                      ; no active dwarves
: lda target_room
  cmp #15
  bcs :+
  rts                      ; not in deep cave, dwarves can't reach here
:
  jsr rng_next_byte        ; random 0-254
  sta pct_roll
  ; First-encounter scene: ~35% chance per turn (threshold 90/255)
  lda dfirst
  bne _ptu_dwarf_active
  lda pct_roll
  cmp #90
  bcs _ptu_done            ; not this turn
  lda #1
  sta dfirst
  lda #3                   ; "A little dwarf just walked around a corner..."
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: lda #5                   ; "One sharp, nasty knife is thrown at you!"
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: lda #6                   ; "None of them hit you!"
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jmp _ptu_done
_ptu_dwarf_active:
  ; Attack if pct_roll < threshold for current dwarf count
  ldx ndwarves
  lda _ptu_attack_thresh,x
  cmp pct_roll
  bcc _ptu_done            ; pct_roll >= threshold — no attack this turn
  ; A dwarf attacks!
  lda #5                   ; "One sharp, nasty knife is thrown at you!"
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: ; Hit check: pct_roll < 38 (~15% of 255) means hit
  lda pct_roll
  cmp #38
  bcs _ptu_miss
  lda #7                   ; "One of them gets you!"
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
: jsr player_death
  jmp _ptu_done
_ptu_miss:
  lda #6                   ; "None of them hit you!"
  sta msg_target
  jsr print_special_message
  bcs :+
  jsr newline
:
_ptu_done:
  rts

save_game_snapshot:
  jsr pack_snapshot_buffer
  jsr fat32_openroot
  ldx #<save_filename
  ldy #>save_filename
  jsr fat32_finddirent
  bcc _sgs_found

  ; Save file missing: create it, then re-open.
  jsr create_save_snapshot_file
  bcs _sgs_fail
  jsr fat32_openroot
  ldx #<save_filename
  ldy #>save_filename
  jsr fat32_finddirent
  bcs _sgs_fail

_sgs_found:
  jsr fat32_opendirent

  ; Save file must be exactly the expected snapshot size.
  lda fat32_bytesremaining+3
  bne _sgs_fail
  lda fat32_bytesremaining+2
  bne _sgs_fail
  lda fat32_bytesremaining+1
  cmp #snapshot_size_hi
  bne _sgs_fail
  lda fat32_bytesremaining
  cmp #snapshot_size_lo
  bne _sgs_fail

  ; Overwrite snapshot image from RAM.
  lda #<snapshot_buf
  sta fat32_address
  lda #>snapshot_buf
  sta fat32_address+1
  lda #snapshot_size_lo
  sta fat32_bytesremaining
  lda #snapshot_size_hi
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_file_write
  bcs _sgs_fail
  clc
  rts
_sgs_fail:
  sec
  rts

create_save_snapshot_file:
  ; Create ADVSAVE DAT with exact snapshot size.
  lda #snapshot_size_lo
  sta fat32_bytesremaining
  lda #snapshot_size_hi
  sta fat32_bytesremaining+1
  lda #0
  sta fat32_bytesremaining+2
  sta fat32_bytesremaining+3
  jsr fat32_allocatefile
  bcs _create_save_fail

  ; Allocation traverses FAT and changes sector context.
  ; Re-open root directory before writing a new dir entry.
  jsr fat32_openroot

  lda #<save_filename
  sta fat32_filenamepointer
  lda #>save_filename
  sta fat32_filenamepointer+1
  jsr fat32_writedirent
  bcs _create_save_fail

  clc
  rts

_create_save_fail:
  sec
  rts

load_game_snapshot:
  jsr fat32_openroot
  ldx #<save_filename
  ldy #>save_filename
  jsr fat32_finddirent
  bcs _lgs_fail
  jsr fat32_opendirent

  ; Require exact snapshot file size.
  lda fat32_bytesremaining+3
  bne _lgs_fail
  lda fat32_bytesremaining+2
  bne _lgs_fail
  lda fat32_bytesremaining+1
  cmp #snapshot_size_hi
  bne _lgs_fail
  lda fat32_bytesremaining
  cmp #snapshot_size_lo
  bne _lgs_fail

  lda #<snapshot_buf
  sta fat32_address
  lda #>snapshot_buf
  sta fat32_address+1
  jsr fat32_file_read
  jsr unpack_snapshot_buffer
  clc
  rts
_lgs_fail:
  sec
  rts

pack_snapshot_buffer:
  lda #<snapshot_block1_base
  sta old_lo
  lda #>snapshot_block1_base
  sta old_hi
  lda #<snapshot_buf
  sta work_lo
  lda #>snapshot_buf
  sta work_hi
  lda #snapshot_block1_len_lo
  sta field_lo
  lda #snapshot_block1_len_hi
  sta field_hi
  jsr copy_mem_block

  lda #<snapshot_block2_base
  sta old_lo
  lda #>snapshot_block2_base
  sta old_hi
  lda #<(snapshot_buf + $02C8)
  sta work_lo
  lda #>(snapshot_buf + $02C8)
  sta work_hi
  lda #snapshot_block2_len_lo
  sta field_lo
  lda #snapshot_block2_len_hi
  sta field_hi
  jmp copy_mem_block

unpack_snapshot_buffer:
  lda #<snapshot_buf
  sta old_lo
  lda #>snapshot_buf
  sta old_hi
  lda #<snapshot_block1_base
  sta work_lo
  lda #>snapshot_block1_base
  sta work_hi
  lda #snapshot_block1_len_lo
  sta field_lo
  lda #snapshot_block1_len_hi
  sta field_hi
  jsr copy_mem_block

  lda #<(snapshot_buf + $02C8)
  sta old_lo
  lda #>(snapshot_buf + $02C8)
  sta old_hi
  lda #<snapshot_block2_base
  sta work_lo
  lda #>snapshot_block2_base
  sta work_hi
  lda #snapshot_block2_len_lo
  sta field_lo
  lda #snapshot_block2_len_hi
  sta field_hi
  jmp copy_mem_block

copy_mem_block:
  lda field_lo
  ora field_hi
  beq _cmb_done
_cmb_loop:
  ldy #0
  lda old_lo
  sta zp_sd_address
  lda old_hi
  sta zp_sd_address+1
  lda (zp_sd_address),y
  sta tmp_digit
  lda work_lo
  sta fat32_address
  lda work_hi
  sta fat32_address+1
  lda tmp_digit
  sta (fat32_address),y
  inc old_lo
  bne :+
  inc old_hi
:
  inc work_lo
  bne :+
  inc work_hi
:
  lda field_lo
  bne :+
  dec field_hi
:
  dec field_lo
  lda field_lo
  ora field_hi
  bne _cmb_loop
_cmb_done:
  rts

command_is_unlock:
  lda cmd_len
  cmp #6
  bcs :+
  clc
  rts
:
  lda cmd_buf
  cmp #'U'
  bne _ciu_no
  lda cmd_buf+1
  cmp #'N'
  bne _ciu_no
  lda cmd_buf+2
  cmp #'L'
  bne _ciu_no
  lda cmd_buf+3
  cmp #'O'
  bne _ciu_no
  lda cmd_buf+4
  cmp #'C'
  bne _ciu_no
  lda cmd_buf+5
  cmp #'K'
  bne _ciu_no
  sec
  rts
_ciu_no:
  clc
  rts

command_is_lock:
  lda cmd_len
  cmp #4
  bcs :+
  clc
  rts
:
  lda cmd_buf
  cmp #'L'
  bne _cil_no
  lda cmd_buf+1
  cmp #'O'
  bne _cil_no
  lda cmd_buf+2
  cmp #'C'
  bne _cil_no
  lda cmd_buf+3
  cmp #'K'
  bne _cil_no
  sec
  rts
_cil_no:
  clc
  rts

player_has_keys:
  ldx #1
  lda toting_table,x
  beq _phk_no
  sec
  rts
_phk_no:
  clc
  rts

player_has_keys_here:
  ldx #1
  lda toting_table,x
  bne _phkh_yes
  lda present_table,x
  beq _phkh_no
_phkh_yes:
  sec
  rts
_phkh_no:
  clc
  rts

object_is_portable:
  ; Returns C set if command_object can be carried.
  ; Keep fixed scenery objects out of inventory.
  lda command_object
  cmp #3                  ; GRATE
  bne _oip_yes
  clc
  rts
_oip_yes:
  sec
  rts

object_is_accessible:
  ; Returns C set if command_object is in inventory or present in room.
  lda command_object
  tax
  cpx #100
  bcc :+
  clc
  rts
:
  lda toting_table,x
  bne _oia_yes
  lda present_table,x
  bne _oia_yes
  clc
  rts
_oia_yes:
  sec
  rts

room_has_water:
  ; Early-game water locations.
  lda target_room
  cmp #1
  beq _rhw_yes
  cmp #4
  beq _rhw_yes
  cmp #7
  beq _rhw_yes
  clc
  rts
_rhw_yes:
  sec
  rts

_eac_parse_error:
  lda parse_error
  cmp #2
  bne _eac_perr_check_missing
  ldx #<msg_unknown_obj
  ldy #>msg_unknown_obj
  jsr print_zstr_xy
  jsr newline
  rts
_eac_perr_check_missing:
  cmp #3
  bne _eac_perr_default
  ldx #<msg_missing_obj
  ldy #>msg_missing_obj
  jsr print_zstr_xy
  jsr newline
  rts
_eac_perr_default:
  ldx #<msg_unknown_word
  ldy #>msg_unknown_word
  jsr print_zstr_xy
  jsr newline
  rts

_eac_no:
  lda #'N'
  jsr print_char
  lda #'O'
  jsr print_char
  jsr newline
  rts

read_pct_roll:
  ; Internal RNG roll: pct_roll = random value in 0..99.
  jsr rng_next_byte
  cmp #100
  bcc :+
  sec
  sbc #100
  cmp #100
  bcc :+
  sec
  sbc #100
:
  sta pct_roll
  rts

rng_next_byte:
  ; 8-bit Galois LFSR with polynomial 0x1D, period 255 (non-zero states).
  lda rng_state
  bne :+
  lda #$A7
:
  lsr
  bcc :+
  eor #$1D
:
  sta rng_state
  rts

prompt_yes_no:
  ; Prompt and read one line. Returns C set for YES, clear for NO/other.
  lda #'>'
  jsr print_char
  lda #' '
  jsr print_char
  lda #0
  sta input_seen
  sta tmp_digit
_pyn_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _pyn_done
  cmp #$0A
  beq _pyn_done
  cmp #'a'
  bcc :+
  cmp #('z'+1)
  bcs :+
  and #$DF
:
  cmp #'A'
  bcc _pyn_loop
  cmp #('Z'+1)
  bcs _pyn_loop
  ldx input_seen
  bne _pyn_loop
  sta tmp_digit
  inc input_seen
  jmp _pyn_loop
_pyn_done:
  lda tmp_digit
  cmp #'Y'
  beq _pyn_yes
  clc
  rts
_pyn_yes:
  sec
  rts

read_carried_obj:
  ; Read carried object id entry (0..99), default 0.
  lda #0
  sta carried_obj
  lda #0
  sta input_seen

  lda #'C'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rc_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rc_eol
  cmp #$0A
  beq _rc_eol
  cmp #'0'
  bcc _rc_loop
  cmp #('9'+1)
  bcs _rc_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rc_accum
  lda #0
  sta carried_obj
_rc_accum:
  lda #1
  sta input_seen

  lda carried_obj
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta carried_obj
  jmp _rc_loop

_rc_eol:
  lda input_seen
  beq _rc_loop

_rc_done:
  jsr newline
  rts

read_present_obj:
  ; Read present object id entry (0..99), default 0.
  lda #0
  sta present_obj
  lda #0
  sta input_seen

  lda #'A'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_ra_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _ra_eol
  cmp #$0A
  beq _ra_eol
  cmp #'0'
  bcc _ra_loop
  cmp #('9'+1)
  bcs _ra_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _ra_accum
  lda #0
  sta present_obj
_ra_accum:
  lda #1
  sta input_seen

  lda present_obj
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta present_obj
  jmp _ra_loop

_ra_eol:
  lda input_seen
  beq _ra_loop

_ra_done:
  jsr newline
  rts

read_prop_obj_id:
  ; Read property override object id (0..99), default 0.
  lda #0
  sta prop_obj_id
  lda #0
  sta input_seen

  lda #'O'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_ro_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _ro_eol
  cmp #$0A
  beq _ro_eol
  cmp #'0'
  bcc _ro_loop
  cmp #('9'+1)
  bcs _ro_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _ro_accum
  lda #0
  sta prop_obj_id
_ro_accum:
  lda #1
  sta input_seen

  lda prop_obj_id
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta prop_obj_id
  jmp _ro_loop

_ro_eol:
  lda input_seen
  beq _ro_loop

_ro_done:
  jsr newline
  rts

read_prop_obj_state:
  ; Read property override state (0..99), default 0.
  lda #0
  sta prop_obj_state
  lda #0
  sta input_seen

  lda #'S'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rs_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rs_eol
  cmp #$0A
  beq _rs_eol
  cmp #'0'
  bcc _rs_loop
  cmp #('9'+1)
  bcs _rs_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rs_accum
  lda #0
  sta prop_obj_state
_rs_accum:
  lda #1
  sta input_seen

  lda prop_obj_state
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta prop_obj_state
  jmp _rs_loop

_rs_eol:
  lda input_seen
  beq _rs_loop

_rs_done:
  jsr newline
  rts

init_prop_table:
  ; Parity with original Adventure:
  ; prop[0..49] = 0, prop[50..99] = -1 ($FF)
  ldx #0
  lda #0
_ipt_loop:
  sta prop_table,x
  inx
  cpx #50
  bcc _ipt_loop
  lda #$FF
_ipt_loop_neg:
  sta prop_table,x
  inx
  cpx #100
  bcc _ipt_loop_neg
  rts

init_toting_table:
  ; Initialize toting table values to zero.
  ldx #0
  lda #0
_itt_loop:
  sta toting_table,x
  inx
  cpx #100
  bcc _itt_loop
  rts

init_present_table:
  ; Initialize present table values to zero.
  ldx #0
  lda #0
_iat_loop:
  sta present_table,x
  inx
  cpx #100
  bcc _iat_loop
  rts

init_visited_table:
  ; Initialize visited-room flags to zero.
  ldx #0
  lda #0
_ivt_loop:
  sta visited_table,x
  inx
  cpx #200
  bcc _ivt_loop
  rts

init_place_table:
  ; Initialize place table to default Adventure object locations.
  ldx #0
  lda #0
_ipl_zero:
  sta place_table,x
  inx
  cpx #100
  bcc _ipl_zero

  ; place[1..10]
  ldx #1
  lda #3
  sta place_table,x
  ldx #2
  lda #3
  sta place_table,x
  ldx #3
  lda #8
  sta place_table,x
  ldx #4
  lda #10
  sta place_table,x
  ldx #5
  lda #11
  sta place_table,x
  ldx #6
  lda #0
  sta place_table,x
  ldx #7
  lda #14
  sta place_table,x
  ldx #8
  lda #13
  sta place_table,x
  ldx #9
  lda #94
  sta place_table,x
  ldx #10
  lda #96
  sta place_table,x

  ; place[11..20]
  ldx #11
  lda #19
  sta place_table,x
  ldx #12
  lda #17
  sta place_table,x
  ldx #13
  lda #101
  sta place_table,x
  ldx #14
  lda #103
  sta place_table,x
  ldx #15
  lda #0
  sta place_table,x
  ldx #16
  lda #106
  sta place_table,x
  ldx #17
  lda #0
  sta place_table,x
  ldx #18
  lda #0
  sta place_table,x
  ldx #19
  lda #3
  sta place_table,x
  ldx #20
  lda #3
  sta place_table,x

  ; place[23..29]
  ldx #23
  lda #109
  sta place_table,x
  ldx #24
  lda #25
  sta place_table,x
  ldx #25
  lda #23
  sta place_table,x
  ldx #26
  lda #111
  sta place_table,x
  ldx #27
  lda #35
  sta place_table,x
  ldx #28
  lda #0
  sta place_table,x
  ldx #29
  lda #97
  sta place_table,x

  ; place[31..40]
  ldx #31
  lda #119
  sta place_table,x
  ldx #32
  lda #117
  sta place_table,x
  ldx #33
  lda #117
  sta place_table,x
  ldx #34
  lda #0
  sta place_table,x
  ldx #35
  lda #130
  sta place_table,x
  ldx #36
  lda #0
  sta place_table,x
  ldx #37
  lda #126
  sta place_table,x
  ldx #38
  lda #140
  sta place_table,x
  ldx #39
  lda #140                 ; BATTERIES start at machine room
  sta place_table,x
  ldx #40
  lda #96
  sta place_table,x

  ; place[50..54]
  ldx #50
  lda #18
  sta place_table,x
  ldx #51
  lda #27
  sta place_table,x
  ldx #52
  lda #28
  sta place_table,x
  ldx #53
  lda #29
  sta place_table,x
  ldx #54
  lda #30
  sta place_table,x

  ; place[56..64]
  ldx #56
  lda #92
  sta place_table,x
  ldx #57
  lda #95
  sta place_table,x
  ldx #58
  lda #97
  sta place_table,x
  ldx #59
  lda #100
  sta place_table,x
  ldx #60
  lda #101
  sta place_table,x
  ldx #61
  lda #0
  sta place_table,x
  ldx #62
  lda #119
  sta place_table,x
  ldx #63
  lda #127
  sta place_table,x
  ldx #64
  lda #130
  sta place_table,x
  rts

sync_present_from_room:
  ; Rebuild present_table from place_table and target_room, excluding carried items.
  jsr init_present_table
  ldx #1
_spr_loop:
  cpx #100
  bcs _spr_done
  lda toting_table,x
  bne _spr_next
  lda place_table,x
  cmp target_room
  bne _spr_next
  lda #1
  sta present_table,x
_spr_next:
  inx
  jmp _spr_loop
_spr_done:
  rts

print_room_objects:
  ; Print all objects currently present in room.
  lda #0
  sta input_seen
  ldx #1
_pro_loop:
  cpx #100
  bcs _pro_end
  lda present_table,x
  beq _pro_next
  lda #1
  sta input_seen
  txa
  pha
  jsr newline
  lda #'-'
  jsr print_char
  lda #' '
  jsr print_char
  pla
  tax
  txa
  pha
  jsr print_inventory_item
  pla
  tax
_pro_next:
  inx
  jmp _pro_loop
_pro_end:
  lda input_seen
  beq _pro_done
  jsr newline
_pro_done:
  rts

print_object_line:
  ; In X/Y = pointer to zero-terminated object name string.
  stx work_lo
  sty work_hi
  jsr newline
  lda #'-'
  jsr print_char
  lda #' '
  jsr print_char
_pol_loop:
  ; OUTCH/print routines can clobber monitor scratch ZP,
  ; so reload the indirect pointer on every character fetch.
  lda work_lo
  sta zp_sd_address
  lda work_hi
  sta zp_sd_address+1
  ldy #0
  lda (zp_sd_address),y
  beq _pol_done
  jsr print_char
  inc work_lo
  bne _pol_loop
  inc work_hi
  jmp _pol_loop
_pol_done:
  rts

print_zstr_xy:
  ; In X/Y = pointer to zero-terminated string.
  stx work_lo
  sty work_hi
_pzs_loop:
  lda work_lo
  sta zp_sd_address
  lda work_hi
  sta zp_sd_address+1
  ldy #0
  lda (zp_sd_address),y
  beq _pzs_done
  jsr print_char
  inc work_lo
  bne _pzs_loop
  inc work_hi
  jmp _pzs_loop
_pzs_done:
  rts

print_examine_text:
  lda command_object
  cmp #1
  bne _pet_lamp
  ldx #<msg_exam_keys
  ldy #>msg_exam_keys
  jmp _pet_print
_pet_lamp:
  cmp #2
  bne _pet_food
  ldx #<msg_exam_lamp
  ldy #>msg_exam_lamp
  jmp _pet_print
_pet_food:
  cmp #19
  bne _pet_bottle
  ldx #<msg_exam_food
  ldy #>msg_exam_food
  jmp _pet_print
_pet_bottle:
  cmp #20
  bne _pet_axe
  ldx #<msg_exam_bottle
  ldy #>msg_exam_bottle
  jmp _pet_print
_pet_axe:
  cmp #28
  bne _pet_cage
  ldx #<msg_exam_axe
  ldy #>msg_exam_axe
  jmp _pet_print
_pet_cage:
  cmp #4
  bne _pet_rod
  ldx #<msg_exam_cage
  ldy #>msg_exam_cage
  jmp _pet_print
_pet_rod:
  cmp #5
  bne _pet_default
  ldx #<msg_exam_rod
  ldy #>msg_exam_rod
  jmp _pet_print
_pet_default:
  ldx #<msg_exam_default
  ldy #>msg_exam_default
_pet_print:
  jsr print_zstr_xy
  rts

print_read_text:
  lda command_object
  cmp #16                  ; MAGAZINE
  bne _prt_tablet
  lda #190
  sta msg_target
  jmp _prt_special
_prt_tablet:
  cmp #13                  ; TABLET
  bne _prt_message
  lda #196
  sta msg_target
  jmp _prt_special
_prt_message:
  cmp #36                  ; MESSAGE
  bne _prt_default
  lda #191
  sta msg_target
  jmp _prt_special
_prt_special:
  jsr print_special_message
  bcc _prt_done
_prt_default:
  ldx #<msg_read_default
  ldy #>msg_read_default
_prt_print:
  jsr print_zstr_xy
_prt_done:
  rts

noun_is_filler_word:
  ; Returns C set if noun token should be ignored as filler.
  lda noun_len
  cmp #1
  bne _nfw_chk2
  lda noun_buf
  cmp #'A'
  beq _nfw_yesjmp
  jmp _nfw_no
_nfw_yesjmp:
  jmp _nfw_yes
_nfw_chk2:
  cmp #2
  bne _nfw_chk3
  ; AN
  lda noun_buf
  cmp #'A'
  bne _nfw_to2
  lda noun_buf+1
  cmp #'N'
  beq _nfw_yesjmp
_nfw_to2:
  ; TO
  lda noun_buf
  cmp #'T'
  bne _nfw_at2
  lda noun_buf+1
  cmp #'O'
  beq _nfw_yesjmp
_nfw_at2:
  ; AT
  lda noun_buf
  cmp #'A'
  bne _nfw_in2
  lda noun_buf+1
  cmp #'T'
  beq _nfw_yesjmp
_nfw_in2:
  ; IN
  lda noun_buf
  cmp #'I'
  bne _nfw_on2
  lda noun_buf+1
  cmp #'N'
  beq _nfw_yesjmp
_nfw_on2:
  ; ON
  lda noun_buf
  cmp #'O'
  bne _nfw_of2
  lda noun_buf+1
  cmp #'N'
  beq _nfw_yesjmp
_nfw_of2:
  ; OF
  lda noun_buf
  cmp #'O'
  bne _nfw_up2
  lda noun_buf+1
  cmp #'F'
  beq _nfw_yesjmp
_nfw_up2:
  ; UP
  lda noun_buf
  cmp #'U'
  bne _nfw_no
  lda noun_buf+1
  cmp #'P'
  beq _nfw_yesjmp
  jmp _nfw_no
_nfw_chk3:
  cmp #3
  bne _nfw_chk4
  ; THE
  lda noun_buf
  cmp #'T'
  bne _nfw_no
  lda noun_buf+1
  cmp #'H'
  bne _nfw_no
  lda noun_buf+2
  cmp #'E'
  bne _nfw_no
  jmp _nfw_yes
_nfw_chk4:
  cmp #4
  bne _nfw_no
  ; WITH
  lda noun_buf
  cmp #'W'
  bne _nfw_with4
  lda noun_buf+1
  cmp #'I'
  bne _nfw_with4
  lda noun_buf+2
  cmp #'T'
  bne _nfw_with4
  lda noun_buf+3
  cmp #'H'
  beq _nfw_yes
_nfw_with4:
  ; FROM
  lda noun_buf
  cmp #'F'
  bne _nfw_no
  lda noun_buf+1
  cmp #'R'
  bne _nfw_no
  lda noun_buf+2
  cmp #'O'
  bne _nfw_no
  lda noun_buf+3
  cmp #'M'
  beq _nfw_yes
  jmp _nfw_no
_nfw_yes:
  sec
  rts
_nfw_no:
  clc
  rts

noun_is_descriptor_word:
  ; Returns C set for a small set of common adjectives in location
  ; phrases, e.g. GO TO SMALL BRICK BUILDING.
  lda noun_len
  cmp #5
  bne _ndw_no
  ; SMALL
  lda noun_buf
  cmp #'S'
  bne _ndw_brick5
  lda noun_buf+1
  cmp #'M'
  bne _ndw_brick5
  lda noun_buf+2
  cmp #'A'
  bne _ndw_brick5
  lda noun_buf+3
  cmp #'L'
  bne _ndw_brick5
  lda noun_buf+4
  cmp #'L'
  beq _ndw_yes
_ndw_brick5:
  ; BRICK
  lda noun_buf
  cmp #'B'
  bne _ndw_steel5
  lda noun_buf+1
  cmp #'R'
  bne _ndw_steel5
  lda noun_buf+2
  cmp #'I'
  bne _ndw_steel5
  lda noun_buf+3
  cmp #'C'
  bne _ndw_steel5
  lda noun_buf+4
  cmp #'K'
  beq _ndw_yes
_ndw_steel5:
  ; STEEL
  lda noun_buf
  cmp #'S'
  bne _ndw_no
  lda noun_buf+1
  cmp #'T'
  bne _ndw_no
  lda noun_buf+2
  cmp #'E'
  bne _ndw_no
  lda noun_buf+3
  cmp #'E'
  bne _ndw_no
  lda noun_buf+4
  cmp #'L'
  bne _ndw_no
_ndw_yes:
  sec
  rts
_ndw_no:
  clc
  rts

noun_is_all:
  ; Returns C set if noun token is "ALL".
  lda noun_len
  cmp #3
  bne _nia_no
  lda noun_buf
  cmp #'A'
  bne _nia_no
  lda noun_buf+1
  cmp #'L'
  bne _nia_no
  lda noun_buf+2
  cmp #'L'
  bne _nia_no
  sec
  rts
_nia_no:
  clc
  rts

count_holding:
  ; Count carried items into entry_count.
  lda #0
  sta entry_count
  ldx #1
_ch_loop:
  cpx #100
  bcs _ch_done
  lda toting_table,x
  beq _ch_next
  inc entry_count
_ch_next:
  inx
  jmp _ch_loop
_ch_done:
  rts

print_inventory_item:
  stx tmp_digit
  jsr object_name_ptr_by_id
  bcc _pii_num
  jsr print_zstr_xy
  rts
_pii_num:
  ldx tmp_digit
  txa
  sta work_lo
  lda #0
  sta work_hi
  jsr print_u16_dec
  rts

object_name_ptr_by_id:
  ; In:  X = object id
  ; Out: C set and X/Y -> zstr pointer if known, else C clear.
  txa
  cmp #64
  bcs _onp_no
  asl
  tay
  lda obj_name_table,y
  sta work_lo
  iny
  lda obj_name_table,y
  sta work_hi
  lda work_lo
  ora work_hi
  beq _onp_no
  ldx work_lo
  ldy work_hi
  sec
  rts
_onp_no:
  clc
  rts

read_toting_entry_count:
  ; Read number of toting entries (0..99), default 0.
  lda #0
  sta entry_count
  lda #0
  sta input_seen

  lda #'T'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rt_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rt_eol
  cmp #$0A
  beq _rt_eol
  cmp #'0'
  bcc _rt_loop
  cmp #('9'+1)
  bcs _rt_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rt_accum
  lda #0
  sta entry_count
_rt_accum:
  lda #1
  sta input_seen

  lda entry_count
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta entry_count
  jmp _rt_loop

_rt_eol:
  lda input_seen
  beq _rt_done

_rt_done:
  jsr newline
  rts

read_present_entry_count:
  ; Read number of present-at-location entries (0..99), default 0.
  lda #0
  sta entry_count
  lda #0
  sta input_seen

  lda #'L'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rlc_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rlc_eol
  cmp #$0A
  beq _rlc_eol
  cmp #'0'
  bcc _rlc_loop
  cmp #('9'+1)
  bcs _rlc_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rlc_accum
  lda #0
  sta entry_count
_rlc_accum:
  lda #1
  sta input_seen

  lda entry_count
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta entry_count
  jmp _rlc_loop

_rlc_eol:
  lda input_seen
  beq _rlc_done

_rlc_done:
  jsr newline
  rts

read_prop_entry_count:
  ; Read number of prop override entries (0..99), default 0.
  lda #0
  sta entry_count
  lda #0
  sta input_seen

  lda #'N'
  jsr print_char
  lda #'?'
  jsr print_char
  lda #' '
  jsr print_char

_rn_loop:
  jsr get_input_filtered
  cmp #$0D
  beq _rn_eol
  cmp #$0A
  beq _rn_eol
  cmp #'0'
  bcc _rn_loop
  cmp #('9'+1)
  bcs _rn_loop

  and #$0F
  sta tmp_digit
  lda input_seen
  bne _rn_accum
  lda #0
  sta entry_count
_rn_accum:
  lda #1
  sta input_seen

  lda entry_count
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta entry_count
  jmp _rn_loop

_rn_eol:
  lda input_seen
  beq _rn_done

_rn_done:
  jsr newline
  rts

print_room_long:
  ; Print room description for target_room from ADVENT1.TXT.
  ; desc_mode=0 full text, desc_mode=1 first sentence.
  ; Out: C clear on success, set if file/room missing.
  jsr fat32_openroot
  ldx #<room_filename1
  ldy #>room_filename1
  jsr fat32_finddirent
  bcc _prl_found
  sec
  rts

_prl_found:
  jsr fat32_opendirent

  lda #0
  sta room_in_section
  lda #1
  sta room_line_start
  lda #$FF
  sta prl_guard_lo
  sta prl_guard_hi

_prl_readloop:
  ; Safety guard: avoid infinite loops if SD stream never reaches EOF.
  lda prl_guard_lo
  ora prl_guard_hi
  bne :+
  jmp _prl_eof
:
  sec
  lda prl_guard_lo
  sbc #1
  sta prl_guard_lo
  lda prl_guard_hi
  sbc #0
  sta prl_guard_hi

  jsr fat32_file_readbyte
  bcc :+
  jmp _prl_eof
:
  sta room_char

  ; Ignore NUL padding/noise from SD stream.
  cmp #$00
  beq _prl_readloop

  ; Ignore CR
  cmp #$0D
  beq _prl_readloop

  ; Handle LF
  cmp #$0A
  bne _prl_not_lf
  lda room_in_section
  beq _prl_set_line_start
  lda desc_mode
  beq _prl_emit_lf_long
  ; In short mode, keep wrapped lines as a single sentence.
  lda room_line_start
  bne _prl_set_line_start
  lda #' '
  jsr print_char
  jmp _prl_set_line_start
_prl_emit_lf_long:
  jsr newline
_prl_set_line_start:
  lda #1
  sta room_line_start
  jmp _prl_readloop

_prl_not_lf:
  lda room_in_section
  beq _prl_not_in_room

  ; If next room marker starts, we are done with this room.
  lda room_line_start
  beq _prl_emit_char
  lda room_char
  cmp #'#'
  beq _prl_done_success

_prl_emit_char:
  lda room_char
  jsr print_char
  lda desc_mode
  beq _prl_emit_char_done
  lda room_char
  cmp #'.'
  bne _prl_emit_char_done
  jsr newline
  jmp _prl_done_success
_prl_emit_char_done:
  lda #0
  sta room_line_start
  jmp _prl_readloop

_prl_not_in_room:
  lda room_char
  cmp #'#'
  bne _prl_clear_line_start

  ; Parse marker number after '#'
  jsr read_room_marker_line
  bcc :+
  jmp _prl_eof
:
  lda room_marker_num
  cmp target_room
  beq :+
  jmp _prl_readloop
:
  lda #1
  sta room_in_section
  lda #1
  sta room_line_start
  jmp _prl_readloop

_prl_clear_line_start:
  lda #0
  sta room_line_start
  jmp _prl_readloop

_prl_eof:
  lda room_in_section
  bne _prl_done_success
  sec
  rts

_prl_done_success:
  clc
  rts

print_special_message:
  ; Print message section msg_target from ADVENT4.TXT.
  ; Out: C clear on success, set if file/section missing.
  jsr fat32_openroot
  ldx #<msg_filename4
  ldy #>msg_filename4
  jsr fat32_finddirent
  bcc _psm_found
  sec
  rts

_psm_found:
  jsr fat32_opendirent

  lda #0
  sta room_in_section
  lda #1
  sta room_line_start
  lda #$FF
  sta prl_guard_lo
  sta prl_guard_hi

_psm_readloop:
  ; Safety guard: avoid infinite loops if SD stream never reaches EOF.
  lda prl_guard_lo
  ora prl_guard_hi
  bne :+
  jmp _psm_eof
:
  sec
  lda prl_guard_lo
  sbc #1
  sta prl_guard_lo
  lda prl_guard_hi
  sbc #0
  sta prl_guard_hi

  jsr fat32_file_readbyte
  bcs _psm_eof
  sta room_char

  ; Ignore NUL padding/noise from SD stream.
  cmp #$00
  beq _psm_readloop

  cmp #$0D
  beq _psm_readloop
  cmp #$0A
  bne _psm_not_lf
  lda room_in_section
  beq _psm_set_line_start
  jsr newline
_psm_set_line_start:
  lda #1
  sta room_line_start
  jmp _psm_readloop

_psm_not_lf:
  lda room_in_section
  beq _psm_not_in_msg
  lda room_line_start
  beq _psm_emit_char
  lda room_char
  cmp #'#'
  beq _psm_done_success
_psm_emit_char:
  lda room_char
  jsr print_char
  lda #0
  sta room_line_start
  jmp _psm_readloop

_psm_not_in_msg:
  lda room_char
  cmp #'#'
  bne _psm_clear_line_start
  jsr read_room_marker_line
  bcs _psm_eof
  lda room_marker_num
  cmp msg_target
  bne _psm_readloop
  lda #1
  sta room_in_section
  lda #1
  sta room_line_start
  jmp _psm_readloop

_psm_clear_line_start:
  lda #0
  sta room_line_start
  jmp _psm_readloop

_psm_eof:
  lda room_in_section
  bne _psm_done_success
  sec
  rts

_psm_done_success:
  clc
  rts

read_room_marker_line:
  ; Consume marker line after leading '#', parse decimal room number.
  lda #0
  sta room_marker_num

_rml_loop:
  ; Safety guard shared with print_room_long to prevent infinite loops
  ; if the SD stream degrades into padding/noise bytes.
  lda prl_guard_lo
  ora prl_guard_hi
  bne :+
  jmp _rml_eof
:
  sec
  lda prl_guard_lo
  sbc #1
  sta prl_guard_lo
  lda prl_guard_hi
  sbc #0
  sta prl_guard_hi

  jsr fat32_file_readbyte
  bcs _rml_eof
  sta room_char

  cmp #$00
  beq _rml_loop
  cmp #$0D
  beq _rml_loop
  cmp #$0A
  beq _rml_done

  cmp #'0'
  bcc _rml_loop
  cmp #('9'+1)
  bcs _rml_loop

  and #$0F
  sta tmp_digit
  lda room_marker_num
  asl
  sta mul_lo
  asl
  asl
  clc
  adc mul_lo
  adc tmp_digit
  sta room_marker_num
  jmp _rml_loop

_rml_done:
  clc
  rts

_rml_eof:
  sec
  rts

read_line:
  ; Reads one text line (LF-terminated) into line_buf.
  ; Returns carry set if EOF with no data.
  lda #0
  sta line_idx
_rl_loop:
  jsr fat32_file_readbyte
  bcc _rl_got
  lda line_idx
  beq _rl_eof_empty
  ldy line_idx
  lda #0
  sta line_buf,y
  clc
  rts
_rl_eof_empty:
  sec
  rts
_rl_got:
  sta tmp_digit
  cmp #$00
  beq _rl_loop
  cmp #$0D
  beq _rl_loop
  cmp #$0A
  beq _rl_done
  lda line_idx
  cmp #79
  bcs _rl_loop
  tay
  lda tmp_digit
  sta line_buf,y
  inc line_idx
  jmp _rl_loop
_rl_done:
  ldy line_idx
  lda #0
  sta line_buf,y
  clc
  rts

find_hash_in_line:
  ; Find '#' anywhere in line_buf. If found, returns Y at first byte after '#'
  ; with carry clear. If not found, carry set.
  ldy #0
_fh_loop:
  lda line_buf,y
  beq _fh_not_found
  cmp #'#'
  beq _fh_found
  iny
  cpy #79
  bcc _fh_loop
_fh_not_found:
  sec
  rts
_fh_found:
  iny
  clc
  rts

parse_u16_field:
  ; Parse unsigned decimal field from line_buf starting at Y.
  ; Stops at ',' or NUL. Consumes ',' if present.
  lda #0
  sta field_lo
  sta field_hi
_puf_loop:
  lda line_buf,y
  beq _puf_done
  cmp #','
  beq _puf_comma
  cmp #'0'
  bcc _puf_next
  cmp #('9'+1)
  bcs _puf_next

  and #$0F
  sta tmp_digit

  lda field_lo
  sta old_lo
  lda field_hi
  sta old_hi

  ; field = old * 2
  clc
  lda old_lo
  adc old_lo
  sta field_lo
  lda old_hi
  adc old_hi
  sta field_hi

  ; mul = old * 8
  lda old_lo
  asl
  sta mul_lo
  lda old_hi
  rol
  sta mul_hi
  asl mul_lo
  rol mul_hi
  asl mul_lo
  rol mul_hi

  ; field = old*2 + old*8 = old*10
  clc
  lda field_lo
  adc mul_lo
  sta field_lo
  lda field_hi
  adc mul_hi
  sta field_hi

  ; + digit
  clc
  lda field_lo
  adc tmp_digit
  sta field_lo
  lda field_hi
  adc #0
  sta field_hi

_puf_next:
  iny
  jmp _puf_loop
_puf_comma:
  iny
_puf_done:
  rts

cond_matches:
  ; Returns C set if cond passes, clear if it fails.
  ; Supported condition classes:
  ;   0..99: 0=always, 1..99=pct_roll < cond
  ;   1xx: robject==0 or carrying robject
  ;   2xx: carrying robject or robject present at location
  ;   3xx/4xx/5xx/7xx: prop[robject] != class-3
  lda cond_lo
  ora cond_hi
  bne _cm_not_zero
  jmp _cm_yes

_cm_not_zero:
  lda #0
  sta cond_group
  lda cond_lo
  sta work_lo
  lda cond_hi
  sta work_hi

_cm_div100:
  lda work_hi
  bne _cm_sub100
  lda work_lo
  cmp #100
  bcc _cm_div_done
_cm_sub100:
  sec
  lda work_lo
  sbc #100
  sta work_lo
  lda work_hi
  sbc #0
  sta work_hi
  inc cond_group
  jmp _cm_div100

_cm_div_done:
  lda work_hi
  bne _cm_no
  lda work_lo
  sta cond_object

  lda cond_group
  beq _cm_class0
  cmp #1
  beq _cm_class1
  cmp #2
  beq _cm_class2
  cmp #3
  beq _cm_class3457
  cmp #4
  beq _cm_class3457
  cmp #5
  beq _cm_class3457
  cmp #7
  beq _cm_class3457
  jmp _cm_no

_cm_class0:
  ; remainder 1..99 means pct check, 0 already handled at start
  lda pct_roll
  cmp cond_object
  bcc _cm_yes
  jmp _cm_no

_cm_class1:
  lda cond_object
  beq _cm_yes
  cmp #100
  bcs _cm_no
  tax
  lda toting_table,x
  bne _cm_yes
  jmp _cm_no

_cm_class2:
  lda cond_object
  cmp #100
  bcs _cm_no
  tax
  lda toting_table,x
  bne _cm_yes
  lda present_table,x
  bne _cm_yes
  jmp _cm_no

_cm_class3457:
  ; required state = cond_group - 3
  lda cond_group
  sec
  sbc #3
  sta tmp_digit

  ldx cond_object
  cpx #100
  bcs _cm_no
  lda prop_table,x

_cm_compare_prop:
  cmp tmp_digit
  bne _cm_yes

_cm_no:
  clc
  rts

_cm_yes:
  sec
  rts

print_u16_dec:
  ; Prints work_hi:work_lo as unsigned decimal (0..999 expected)
  lda #0
  sta hundreds
  sta tens

_p100:
  lda work_hi
  bne _p100_sub
  lda work_lo
  cmp #100
  bcc _p100_done
_p100_sub:
  sec
  lda work_lo
  sbc #100
  sta work_lo
  lda work_hi
  sbc #0
  sta work_hi
  inc hundreds
  jmp _p100
_p100_done:

_p10:
  lda work_hi
  bne _p10_sub
  lda work_lo
  cmp #10
  bcc _p10_done
_p10_sub:
  sec
  lda work_lo
  sbc #10
  sta work_lo
  lda work_hi
  sbc #0
  sta work_hi
  inc tens
  jmp _p10
_p10_done:

  lda hundreds
  beq _ptens
  clc
  adc #'0'
  jsr print_char

_ptens:
  lda hundreds
  bne _ptens_emit
  lda tens
  beq _pones
_ptens_emit:
  lda tens
  clc
  adc #'0'
  jsr print_char

_pones:
  lda work_lo
  clc
  adc #'0'
  jsr print_char
  rts

  .include "hwconfig.s"
  .include "libsd.s"
  .include "libfat32.s"
  .include "libio.s"

  .word reset
  .word $0000

filename:
  .asciiz "ADVCAVE TXT"
room_filename1:
  .asciiz "ADVENT1 TXT"
msg_filename4:
  .asciiz "ADVENT4 TXT"
save_filename:
  .asciiz "ADVSAVE DAT"

obj_name_keys:
  .asciiz "KEYS"
obj_name_lamp:
  .asciiz "LAMP"
obj_name_grate:
  .asciiz "GRATE"
obj_name_steps:
  .asciiz "STEPS"
obj_name_bird:
  .asciiz "BIRD"
obj_name_door:
  .asciiz "DOOR"
obj_name_pillow:
  .asciiz "PILLOW"
obj_name_snake:
  .asciiz "SNAKE"
obj_name_fissure:
  .asciiz "FISSURE"
obj_name_tablet:
  .asciiz "TABLET"
obj_name_clam:
  .asciiz "CLAM"
obj_name_oyster:
  .asciiz "OYSTER"
obj_name_magazine:
  .asciiz "MAGAZINE"
obj_name_food:
  .asciiz "FOOD"
obj_name_bottle:
  .asciiz "BOTTLE"
obj_name_water:
  .asciiz "WATER"
obj_name_oil:
  .asciiz "OIL"
obj_name_mirror:
  .asciiz "MIRROR"
obj_name_plant:
  .asciiz "PLANT"
obj_name_stalactite:
  .asciiz "STALACTITE"
obj_name_figure:
  .asciiz "FIGURE"
obj_name_axe:
  .asciiz "AXE"
obj_name_drawings:
  .asciiz "DRAWINGS"
obj_name_pirate:
  .asciiz "PIRATE"
obj_name_dragon:
  .asciiz "DRAGON"
obj_name_chasm:
  .asciiz "CHASM"
obj_name_troll:
  .asciiz "TROLL"
obj_name_bear:
  .asciiz "BEAR"
obj_name_message:
  .asciiz "MESSAGE"
obj_name_volcano:
  .asciiz "VOLCANO"
obj_name_machine:
  .asciiz "MACHINE"
obj_name_batteries:
  .asciiz "BATTERIES"
obj_name_carpet:
  .asciiz "CARPET"
obj_name_nugget:
  .asciiz "NUGGET"
obj_name_diamonds:
  .asciiz "DIAMONDS"
obj_name_silver:
  .asciiz "SILVER"
obj_name_jewelry:
  .asciiz "JEWELRY"
obj_name_coins:
  .asciiz "COINS"
obj_name_chest:
  .asciiz "CHEST"
obj_name_eggs:
  .asciiz "EGGS"
obj_name_trident:
  .asciiz "TRIDENT"
obj_name_vase:
  .asciiz "VASE"
obj_name_emerald:
  .asciiz "EMERALD"
obj_name_pyramid:
  .asciiz "PYRAMID"
obj_name_pearl:
  .asciiz "PEARL"
obj_name_rug:
  .asciiz "RUG"
obj_name_chain:
  .asciiz "CHAIN"
obj_name_cage:
  .asciiz "CAGE"
obj_name_rod:
  .asciiz "ROD"

; object id -> display name pointer table for ids 0..63
obj_name_table:
  .word 0                    ; 0
  .word obj_name_keys        ; 1
  .word obj_name_lamp        ; 2
  .word obj_name_grate       ; 3
  .word obj_name_cage        ; 4
  .word obj_name_rod         ; 5
  .word obj_name_rod         ; 6
  .word obj_name_steps       ; 7
  .word obj_name_bird        ; 8
  .word obj_name_door        ; 9
  .word obj_name_pillow      ; 10
  .word obj_name_snake       ; 11
  .word obj_name_fissure     ; 12
  .word obj_name_tablet      ; 13
  .word obj_name_clam        ; 14
  .word obj_name_oyster      ; 15
  .word obj_name_magazine    ; 16
  .word 0                    ; 17
  .word 0                    ; 18
  .word obj_name_food        ; 19
  .word obj_name_bottle      ; 20
  .word obj_name_water       ; 21
  .word obj_name_oil         ; 22
  .word obj_name_mirror      ; 23
  .word obj_name_plant       ; 24
  .word obj_name_plant       ; 25
  .word obj_name_stalactite  ; 26
  .word obj_name_figure      ; 27
  .word obj_name_axe         ; 28
  .word obj_name_drawings    ; 29
  .word obj_name_pirate      ; 30
  .word obj_name_dragon      ; 31
  .word obj_name_chasm       ; 32
  .word obj_name_troll       ; 33
  .word obj_name_troll       ; 34
  .word obj_name_bear        ; 35
  .word obj_name_message     ; 36
  .word obj_name_volcano     ; 37
  .word obj_name_machine     ; 38
  .word obj_name_batteries   ; 39
  .word obj_name_carpet      ; 40
  .word 0                    ; 41
  .word 0                    ; 42
  .word 0                    ; 43
  .word 0                    ; 44
  .word 0                    ; 45
  .word 0                    ; 46
  .word 0                    ; 47
  .word 0                    ; 48
  .word 0                    ; 49
  .word obj_name_nugget      ; 50
  .word obj_name_diamonds    ; 51
  .word obj_name_silver      ; 52
  .word obj_name_jewelry     ; 53
  .word obj_name_coins       ; 54
  .word obj_name_chest       ; 55
  .word obj_name_eggs        ; 56
  .word obj_name_trident     ; 57
  .word obj_name_vase        ; 58
  .word obj_name_emerald     ; 59
  .word obj_name_pyramid     ; 60
  .word obj_name_pearl       ; 61
  .word obj_name_rug         ; 62
  .word obj_name_chain       ; 63

msg_already_here:
  .asciiz "ALREADY HERE"
msg_not_carrying_prefix:
  .asciiz "NOT CARRYING "
msg_no_here_prefix:
  .asciiz "NO "
msg_here_suffix:
  .asciiz " HERE"
msg_taken_prefix:
  .asciiz "TAKEN: "
msg_dropped_prefix:
  .asciiz "DROPPED: "

msg_exam_keys:
  .asciiz "KEYS: A SMALL RING OF KEYS."
msg_exam_lamp:
  .asciiz "LAMP: A SHINY BRASS LAMP."
msg_exam_food:
  .asciiz "FOOD: SOME TASTY-LOOKING FOOD."
msg_exam_bottle:
  .asciiz "BOTTLE: A SMALL GLASS BOTTLE."
msg_exam_axe:
  .asciiz "AXE: A LITTLE DWARF'S AXE."
msg_exam_cage:
  .asciiz "CAGE: A SMALL WICKER CAGE."
msg_exam_rod:
  .asciiz "ROD: A BLACK ROD WITH A RUSTY STAR."
msg_exam_default:
  .asciiz "NOTHING SPECIAL."

msg_unknown_word:
  .asciiz "I DON'T KNOW THAT WORD."
msg_unknown_obj:
  .asciiz "I DON'T SEE THAT HERE."
msg_missing_obj:
  .asciiz "WHAT?"
msg_cant_take:
  .asciiz "YOU CAN'T TAKE THAT."
msg_opened:
  .asciiz "OPEN."
msg_closed:
  .asciiz "CLOSED."
msg_already_unlocked:
  .asciiz "THE GRATE IS ALREADY UNLOCKED."
msg_already_locked:
  .asciiz "THE GRATE IS ALREADY LOCKED."
msg_need_keys:
  .asciiz "YOU NEED KEYS."
msg_locked_grate:
  .asciiz "You can't go through a locked steel grate!"
msg_help_1:
  .asciiz "CMDS: LOOK, INV, TAKE/DROP <OBJ>, TAKE/DROP ALL."
msg_help_2:
  .asciiz "MOVE: N,S,E,W,U,D, IN, OUT, GO <DIR/PLACE>."
msg_help_3:
  .asciiz "USE: LIGHT/OFF/RUB LAMP, EAT/FEED/THROW, DRINK/FILL/POUR."
msg_help_4:
  .asciiz "ACT: OPEN/CLOSE/LOCK/UNLOCK, WAVE, KILL/ATTACK, SAY, QUIT."
msg_okay:
  .asciiz "OKAY."

msg_read_default:
  .asciiz "NOTHING TO READ."
msg_lamp_on:
  .asciiz "YOUR LAMP IS NOW ON."
msg_lamp_off:
  .asciiz "YOUR LAMP IS NOW OFF."
msg_lamp_already_on:
  .asciiz "YOUR LAMP IS ALREADY ON."
msg_lamp_already_off:
  .asciiz "YOUR LAMP IS ALREADY OFF."
msg_eat_ok:
  .asciiz "THANK YOU, IT WAS DELICIOUS."
msg_no_water:
  .asciiz "YOU HAVE NO WATER."
msg_no_water_here:
  .asciiz "THERE IS NO WATER HERE."
msg_bottle_full:
  .asciiz "YOUR BOTTLE IS ALREADY FULL."
msg_fill_ok:
  .asciiz "YOUR BOTTLE IS NOW FULL."
msg_drink_ok:
  .asciiz "THE WATER TASTES FRESH."
msg_bottle_empty:
  .asciiz "YOUR BOTTLE IS ALREADY EMPTY."
msg_pour_ok:
  .asciiz "YOUR BOTTLE IS NOW EMPTY."
msg_bridge_now_spans:
  .asciiz "A crystal bridge now spans the fissure."
msg_bridge_vanished:
  .asciiz "The crystal bridge has vanished!"
msg_nothing_happens:
  .asciiz "NOTHING HAPPENS."
msg_clam_lock:
  .asciiz "YOU HAVE NO WAY TO LOCK THAT."
msg_game_saved:
  .asciiz "GAME SAVED."
msg_game_restored:
  .asciiz "GAME RESTORED."
msg_no_saved_game:
  .asciiz "NO SAVED GAME."
msg_save_failed:
  .asciiz "SAVE FAILED."
msg_welcome_1:
  .asciiz "                              Welcome to"
msg_welcome_2:
  .asciiz ""
msg_welcome_3:
  .asciiz "                       Colossal Cave Adventure!"
msg_welcome_4:
  .asciiz ""
msg_welcome_5:
  .asciiz "               Original development by Willie Crowther."
msg_welcome_6:
  .asciiz "                  Major features added by Don Woods."
msg_welcome_7:
  .asciiz "                 Conversion to BDS C by J. R. Jaeger."
msg_ask_instructions:
  .asciiz "Would you like instructions?"
msg_intro_help_1:
  .asciiz "Somewhere nearby is Colossal Cave, where others have found fortunes in"
msg_intro_help_2:
  .asciiz "treasure and gold, though it is rumored that some who enter are never"
msg_intro_help_3:
  .asciiz "seen again.  Magic is said to work in the cave.  I will be your eyes"
msg_intro_help_4:
  .asciiz "and hands.  Direct me with commands of 1 or 2 words.  I should warn"
msg_intro_help_5:
  .asciiz "you that I look at only the first five letters of each word, so you'll"
msg_intro_help_6:
  .asciiz "have to enter 'Northeast' as 'ne' to distinguish it from 'North'."
msg_intro_help_7:
  .asciiz "(Should you get stuck, type 'help' for some general hints)."
msg_quit_confirm:
  .asciiz "Do you really want to quit now?"
msg_quit_ok:
  .asciiz "OK"
msg_treasures_label:
  .asciiz "Treasures:          "
msg_survival_label:
  .asciiz "Survival:           "
msg_score_label:
  .asciiz "Score:              "
msg_cave_closing:
  .asciiz "A distant explosion shakes the cave! All entrances have collapsed. The cave is now closed."
msg_death:
  .asciiz "You are dead."
msg_resurrect:
  .asciiz "Do you wish to be reincarnated?"
msg_teleport:
  .asciiz "The cave entrance collapses behind you. You find yourself in the repository."
msg_blast_133:
  .asciiz "There is a loud explosion, and you are suddenly engulfed in a blinding flash of light!"
msg_blast_134:
  .asciiz "There is a loud explosion, and a hole appears in the wall. You are engulfed in blinding light."
msg_blast_135:
  .asciiz "There is a loud explosion, and a nearby rod vibrates. The room shudders."
msg_dragon_slain:
  .asciiz "You slay the dragon with your bare hands! (Unbelievable, isn't it?)"

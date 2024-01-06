;;;
;;;  ZEDIAC MUSIC TRACKER
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-12-08: Created file
;;; 2023-12-09: Note interface implementated
;;; 2023-12-29: Update to use new SLIME begin/end link blocks
;;;

#include "../curses/build/curses.sym"
#include "../monitor/inc/keyboard.inc"

;;; HW DEVICE
SOUND_V0    .equ $a000
SOUND_V1    .equ $a004
SOUND_V2    .equ $a008
SOUND_V3    .equ $a00c
    
;;; SOME NOTES (about notes)
;;;
;;; Notes are stored in a single byte bitfield organized as:
;;; 7 654 3 210
;;; |  |  |  |
;;; |  |  |  +-> Note (1-7) representing C-B
;;; |  |  +----> 1 = sharp, 0 = no accidental
;;; |  +-------> Octave (0-7)
;;; +----------> [UNUSED]
;;;
    

;;; ------------------------------------
;;;  CONSTANTS
;;; ------------------------------------

NUM_VOICES      .def 4
BEATS_PER_VOICE .def 256
BEATS_TO_PRINT  .def 16
NUM_WAVEFORMS   .def 5          ; Number of available waveforms per voice

NOTE_ACCI_MASK  .def $08
NOTE_NOTE_MASK  .def $07
NOTE_OCTA_MASK  .def $70

DEFAULT_TEMPO   .def 120        ; BPM
MIN_TEMPO       .def 30         ; BPM
MAX_TEMPO       .def 200        ; BPM

;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------

beat_index      .equ $c0        ; 8  bits - Current beat
selected_voice  .equ $c1        ; 16 bits - Currently selected voice
                                ;    Doesn't need 16 bits, but the reduces
                                ;    the rep/sep'ing needed
loop_beat       .equ $c3        ; 8  bits - Where to jump to on a loop
tempo           .equ $c4        ; 8  bits - Tempo in BPM
is_playing      .equ $c5        ; 8  bits - VOLATILE BOOL !0 = running, 0 = paused
tempo_div       .equ $c6        ; 8  bits - VOLATILE - used to divide NMI interrupt freq

tmp0            .equ $d0        ; 8  bits - temporary storage (assume
                                ;    overriten by any sub)
    
;;; ------------------------------------
;;;  STATIC VARIABLES
;;; ------------------------------------

beats           .equ $4000      ; One page of 256 bytes per voice
                                ; This stores the note index of each note
                                ; 0 = note off
voice_wave_type .equ beats + {BEATS_PER_VOICE * NUM_VOICES}
                                ; Stores the waveform index for each voice
                                ; NUM_VOICES bytes in length
    

;;; ------------------------------------
;;;  APPLICATION LOAD BASE
;;; ------------------------------------
    
    ;; .org 0                      ; For assembler

    .begin $0000 "nmi-handler"
    ;; BEGIN LOADER BLOCK
    ;; .word __REGION_2_END - __REGION_2_BEGIN
    ;; .word 0
    ;; .byte 0
    ;; END LOADER BLOCK
    
;;; Put NMI handler here

__REGION_2_BEGIN:
    pha
    phx
    php
    .as 
    .xs
    sep #$30
    lda VIA0_T1CL               ; Clear interrupt
    inc tempo_div               ; Only write out notes every 256 NMIs
    beq nmi_notes
    plp
    plx
    pla
    rti

nmi_notes:
    lda <is_playing             ; If not playing, don't do anything
    beq nmi_cleanup
    
    ldx <beat_index
    lda beats, x                ; Get beat note
    tax
    .xl
    .al
    rep #$30
    ;;  TODO: some divider
    sta SOUND_V0

    ;; Now set all the waveforms
    ldx #0
nmi_waves:  
    lda voice_wave_type
    sta SOUND_V0+2
    inx
    inx
    inx
    inx
    bne nmi_waves
    inc beat_index
nmi_cleanup:    
    plp
    plx
    pla
    rti
__REGION_2_END:
    .end "nmi-handler"

    .begin $ "music-tracker"
    ;; BEGIN LOADER BLOCK
    .word __REGION_1_END - __REGION_1_BEGIN
    .word __REGION_1_BEGIN & $ffff
    .byte {__REGION_1_BEGIN >> 16} & $ff
    ;; END LOADER BLOCK
    
__REGION_1_BEGIN:
entry:
    .as
    .xl
    sep #$20
    rep #$10
    lda #2                      ; Set the DBR to a known value
    pha
    plb
    jmp tracker
    
;;; ------------------------------------
;;;  TEXT BANK
;;; ------------------------------------
_txt_eol:
    .byte "\n", 0               ; End of line
_txt_title_bar:
    .byte "             Z-TRACKER (C) Ray Clemens 2023            ",0
_txt_track:
    .byte " TRACK ",0
_txt_play:
    .byte "PLAYING",0
_txt_pause:
    .byte "PAUSED ",0
_txt_tempo:
    .byte "Tempo: ",0
_txt_nonote:
    .byte "            |",0
_txt_wavetable:
    .byte "="                   ; Square wave
    .byte "/"                   ; Right saw
    .byte "\\"                  ; Left saw
    .byte "^"                   ; Triangle
    .byte "#"                   ; Noise

    ;; TODO: MOVE THIS TO THE CURSES LIB!
_txt_inv_cur:
    .byte "^[[?25l",0           ; Invisible cursor
_txt_vis_cur:
    .byte "^[[?25h",0           ; Visible cursor
    
;;; ------------------------------------
;;;  FUNCTIONS
;;; ------------------------------------

;;; MAIN ENTRY POINT!
    .as
    .xl
tracker:
    ;; Curses setup
    jsl cur_clear               ; Clear screen
    jsl cur_echo_off            ; Disable character echo
    ldx #1
    jsl cur_set_to              ; Wait for a key

    ;;  Reset tempo and is_playing for "safety"
    stz is_playing
    lda #DEFAULT_TEMPO
    sta tempo
    
    lda #%11000000              ; Enable T1 interrupts
    sta VIA0_IER
    lda #%01000000              ; Free running T1 mode, disable PB7,
                                ; no input latching on A or B
    sta VIA0_ACR

    .al
    rep #$20
    lda #61416                  ; Jiffy timer = 120 BPM
    sta VIA0_T1LL               ; VIA0 T1 TCL/TCH

    .as
    sep #$20
    
    ;; By default, don't reset just in case something goes wrong
    ;; you might be able to recover your song
    jmp t_refresh
    
    ;; Tracker datastructure setup
t_init: 
    jsr tracker_reset_all

t_refresh:
    pea _txt_inv_cur            ; Shut off cursor
    jsl sys_puts
    ply
    jsr print_title             ; Do 'da tin

    ;; Main event loop
t_main_loop:
    jsr print_status_info
    jsr print_tracks

    ;;  Handle input events
    jsl cur_getch
    bcc t_main_loop             ; No key, just loop again
    cmp #'~'                    ; EXIT?
    beq t_exit
    cmp #'`'                    ; FULL RESET?
    beq t_init
    cmp #'r'                    ; Refresh display?
    beq t_refresh
    cmp #'['                    ; Back up the beat index?
    beq backup
    cmp #']'                    ; Next beat index?
    beq frontup
    cmp #'w'                    ; Cycle waveform?
    beq cycle_waveform
    cmp #'}'                    ; Move to next voice?
    beq nextvoice
    cmp #'{'                    ; Move to previous voice?
    beq prevvoice
    cmp #'<'                    ; Decrease tempo?
    beq dectempo
    cmp #'>'                    ; Increase tempo?
    beq inctempo
    cmp #' '                    ; Start playback?
    beq toggle_play
    
    jsr parse_note              ; Might be a note, process that
        
    jmp t_main_loop

;;; Full quit back to MONITOR
t_exit:
    pea _txt_vis_cur            ; Turn back on cursor
    jsl sys_puts
    ply
    lda #0                      ; User-generated
    sta VIA0_IER                ; Disable interrupts
    rtl

;;; Move to previous beat
backup:
    lda <beat_index
    ;; beq t_main_loop             ; Don't decrement a 0 index!
    dec
    sta <beat_index
    bra t_main_loop

;;; Move to next beat
frontup:
    lda <beat_index
    ;; cmp #$ff                    ; Don't overflow index!
    inc
    sta <beat_index
    bra t_main_loop

;;; Cycle waveform for currently selected voice
cycle_waveform:
    .xs
    sep #$10
    ldx <selected_voice         ; Get current voice number
    lda voice_wave_type, x
    inc
    cmp #NUM_WAVEFORMS          ; Does this need to wrap?
    bcc cw_nowrap
    lda #0                      ; Reset
cw_nowrap:
    sta voice_wave_type, x
    .xl
    rep #$10
    bra t_main_loop

;;; Move currently active voice to next
nextvoice:
    lda <selected_voice
    inc
    cmp #NUM_VOICES
    bcc nv_store                ; No wrap, just save next number
    lda #0
nv_store:
    sta <selected_voice
    bra t_main_loop

;;; Move currently active voice to previous
prevvoice:
    lda <selected_voice
    dec
    bpl pv_store                ; No wrap, just save next number
    lda #NUM_VOICES - 1
pv_store:
    sta <selected_voice
    jmp t_main_loop

    
;;; Increment tempo
inctempo:
    lda <tempo
    inc
    cmp #MAX_TEMPO+1
    beq $+3
    sta <tempo
    jsr set_tempo
    jmp t_main_loop

;;; Decrement tempo
dectempo:
    lda <tempo
    dec
    cmp #MIN_TEMPO-1
    beq $+3
    sta <tempo
    jsr set_tempo
    jmp t_main_loop

toggle_play:
    lda <is_playing
    beq tp_start
    stz <is_playing             ; Stop playback
    stz VIA0_IER                ; Disable timer
    jmp t_main_loop
tp_start:
    lda #1                      ; Start playback
    sta <is_playing
    jsr set_tempo
    jmp t_main_loop

t_mloop_goto:                   ; Extend branch range
    jmp t_main_loop

    
;;; Parse a single key and if it's a valid
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   A - the ASCII keycode of the note
;;; Uses:
;;;   A, X
;;; Return:
;;;   NONE
;;;
    .xl
    .as
parse_note:
    pha
    ;; Calculate beat address location
    lda #0
    xba
    lda <beat_index             ; Beat offset
    .al
    .xs
    rep #$20
    sep #$10
    ;; Multiply BEATS_PER_VOICE by the voice number
    ldx <selected_voice         ; Get current voice
pn_note_idxl:
    beq pn_note_offs
    clc
    adc #BEATS_PER_VOICE
    dex
    bra pn_note_idxl
pn_note_offs:   
    .xl
    rep #$10
    tax                         ; X now includes the offset into the beat array
                                ; for the current beat and voice
    .as
    sep #$20
    pla

    ;; Perform key parsing
    cmp #'#'                    ; Sharp?
    bne pn_not_note             ; Nope
    ;;  Toggle sharp bit
    lda beats, x
    eor #NOTE_ACCI_MASK         ; Toggle
    sta beats, x
    rts
    
pn_not_note:    
    cmp #'0'                    ; A < '0' ?
    bcc pn_done                 ; Not a valid note
    cmp #'7'+1                  ; Valid octave?
    bcs pn_note                 ; No
    ;; Valid octave, enter it
    asl                         ; Shift bits to octave position
    asl
    asl
    asl
    and #NOTE_OCTA_MASK         ; Get just the octave
    pha
    lda beats, x                ; Get beat
    and #~NOTE_OCTA_MASK        ; Clear octave
    sta tmp0                    ; Save for later
    pla
    ora tmp0
    sta beats, x                ; Save result
    rts

pn_note:
    ;; Check if this is a valid note
    and #$5f                    ; Ignore case
    cmp #'A'                    ; LT = invalid
    bcc pn_done                 ; invalid
    cmp #'G'+1                  ; LT = valid
    bcs pn_done
    ;; Valid note, put it in the beat
    pha
    lda beats, x                ; Get beat
    and #~NOTE_NOTE_MASK        ; Clear note
    sta tmp0                    ; Save for later
    pla
    and #NOTE_NOTE_MASK         ; Get just the note
    ora tmp0
    and #~NOTE_ACCI_MASK        ; Remove accidental
    sta beats, x                ; Save result
    
pn_done:
    rts


;;; Print the status bar across the top of the UI
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A, Y
;;; Return:
;;;   NONE
;;;
    .xl
    .as
print_title:
    jsl cur_home                ; Top left of screen
    jsl cur_standend            ; Clear attributes
    jsl cur_setattr_inv         ; Invert text
    pea _txt_title_bar          ; Get title text
    jsl sys_puts
    ply                         ; Correct stack
    jsl cur_clrattr_inv         ; Clear inversion
    rts


;;; Print the tempo and other status information
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A, X, Y
;;; Return:
;;;   NONE
;;;
    .xl
    .as
print_status_info:
    ldx #3
    ldy #2
    jsl cur_movexy

    lda <is_playing
    beq psi_not_play
    pea _txt_play
    jsl sys_puts
    ply                         ; Correct stack
    bra psi_2

psi_not_play:
    pea _txt_pause
    jsl sys_puts
    ply                         ; Correct stack

psi_2:  
    lda #' '
    jsl sys_putc
    
    pea _txt_tempo
    jsl sys_puts
    ply                         ; Correct stack

    lda <tempo
    clc                         ; No leading 0's
    jsl sys_putdec
    lda #' '
    jsl sys_putc
    rts

    
;;; Print the voice tracks from the current play head
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A, X, Y
;;; Return:
;;;   NONE
;;; 
    .xl
    .as
print_tracks:
    ;; Move to third line
    ldx #3
    ldy #3
    jsl cur_movexy
    
    ;; Print track headers
    ldx #0                      ; Track index
pt_head:
    lda #'|'                    ; Start of column
    jsl sys_putc
    cpx <selected_voice         ; If this is the selected voice, bold the text
    bne pt_h_nosel
    jsl cur_setattr_inv
pt_h_nosel:
    pea _txt_track
    jsl sys_puts
    ply
    txa                         ; Get track number
    jsl sys_puthex
    lda #' '
    jsl sys_putc
    .xs
    sep #$10
    ldy voice_wave_type, x      ; Get wave type
    lda _txt_wavetable, y       ; Get symbol for wave type
    jsl sys_putc
    .xl
    rep #$10
    lda #' '
    jsl sys_putc
    jsl cur_clrattr_inv         ; Disable bold
    inx
    cpx #NUM_VOICES             ; Check if all the voices are printed
    bne pt_head
    
    lda #'|'                    ; End of track column marker
    jsl sys_putc

    ;; Outer loop: iterate over beats
    lda <beat_index             ; Get current beat
    cmp #{BEATS_TO_PRINT / 2}   ; Is this beat before the middle of the display?
    bcs pt_start_nz             ; Yes, start at beat 0
    lda #{BEATS_TO_PRINT / 2}
pt_start_nz:
    ldx #0
    sec
    sbc #{BEATS_TO_PRINT / 2}   ; Offset to keep current beat vertically centered
    pha                         ; Save changed beat index
pt_beats:
    pea _txt_eol                ; Newline
    jsl sys_puts
    ply
    
    pla                         ; Get beat number (after shift)
    cmp <beat_index             ; If this is the current beat, higilight line
    bne pt_beatnum
    pha                         ; Continue to save beat number
    jsl cur_setattr_inv
    pla
pt_beatnum: 
    tay                         ; Save A
    jsl sys_puthex              ; Print beat number
    iny
    tya
    pha                         ; Save beat number for later outer iterations
    
    lda #'|'
    jsl sys_putc
    
    ;; Inner loop: Iterate over tracks
    lda #0                      ; Get the current beat (after modification)
    xba
    pla
    pha
    dec
    tay
pt_ti:
    lda beats, y                ; Get beat data
    pha
    and #NOTE_NOTE_MASK
    bne pt_ti_prnote            ; Non-zero = print note
    pla
    phy
    pea _txt_nonote             ; Zero = blank entry
    jsl sys_puts
    pla                         ; Restore stack
    pla
    ply
    bra pt_ti_next
pt_ti_prnote:
    lda #' '
    jsl sys_putc
    pla
    jsr prnote                  ; Print the note (3 chars wide)
    phy
    ldy #7                      ; Print a few spaces
    lda #' '
pt_ti_pn_l:
    jsl sys_putc
    dey
    bpl pt_ti_pn_l
    lda #'|'                    ; End of column
    jsl sys_putc
    ply
pt_ti_next: 
    .al                         ; Get next voice beat table
    rep #$20
    tya
    clc
    adc #BEATS_PER_VOICE
    tay
    .as
    sep #$20

    cpy #{BEATS_PER_VOICE * NUM_VOICES}
    beq $+4
    bcc pt_ti                   ; Not doen with entries, do next voice
    
    ;; Done with line, set up next outer loop
    jsl cur_clrattr_inv         ; Make sure that inversion is not carried to next line
    
    inx
    cpx #BEATS_TO_PRINT
    bmi pt_beats
    pla
    rts
    

;;; Clear all tracker data (INIT)
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A, X
;;; Return:
;;;   NONE
;;;
    .xl
    .as
tracker_reset_all:
    ;; Reset beat number
    stz <beat_index

    ;; Reset currently selected voice
    stz <selected_voice
    stz <selected_voice+1

    ;; Reset loop target beat
    stz loop_beat

    ;; Reset tempo
    lda #DEFAULT_TEMPO
    sta <tempo
    
    ;; Clear beat array
    ldx #BEATS_PER_VOICE * NUM_VOICES
_tr_loop_b:
    stz beats,x
    dex
    bpl _tr_loop_b

    ;; Voice waveform array
    ldx #NUM_VOICES
_tr_loop_v:
    stz voice_wave_type,x
    dex
    bpl _tr_loop_v
    rts


;;; Print a note as a string
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   A - The note to print
;;; Uses:
;;;   A
;;; Return:
;;;   NONE
;;;
    .xl
    .as
prnote:
    pha
    ;; Print note
    and #NOTE_NOTE_MASK         ; Just get the note
    ora #$40                    ; Map to ASCII character
    jsl sys_putc
    ;; Followed by sharps
    pla
    pha
    and #NOTE_ACCI_MASK         ; Accidental?
    beq prn_nacci               ; Nope!
    lda #'#'                    ; Yep
    bra prn_acci
prn_nacci:
    lda #'-'
prn_acci:
    jsl sys_putc
    ;; And finally, the octave
    pla
    lsr
    lsr
    lsr
    lsr
    ora #'0'                    ; Map to '0'-'7'
    jsl sys_putc
    rts


;;; Update the VIA timer with the current tempo
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
;;;
    .as
    .xl
set_tempo:
    lda #%11000000              ; Enable T1 interrupts
    sta VIA0_IER
    lda <tempo                  ; Get clock divider from table
    .al
    rep #$20
    and #$00ff
    asl
    tax
    lda tempos, x
    sta VIA0_T1CL
    sta VIA0_T1LL
    .as
    sep #$20
    rts

    
#include "tempos.inc"

__REGION_1_END:
    .end "music-tracker"
    

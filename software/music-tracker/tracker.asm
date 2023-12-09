;;;
;;;  ZEDIAC CURSES IMPLEMENTATION
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-12-08: Created file
;;;

#include "../curses/build/curses.sym"
#include "../monitor/inc/keyboard.inc"

;;; ------------------------------------
;;;  CONSTANTS
;;; ------------------------------------

NUM_VOICES      .def 4
BEATS_PER_VOICE .def 256
BEATS_TO_PRINT  .def 16
NUM_WAVEFORMS   .def 5          ; Number of available waveforms per voice
    
;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------

beat_index      .equ $c0        ; 8  bits - Current beat
selected_voice  .equ $c1        ; 16 bits - Currently selected voice
                                ;    Doesn't need 16 bits, but the reduces
                                ;    the rep/sep'ing needed
    
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
    
    .rom $1000
    .org $0000                 ; TODO: Change this back to a reasonable address (and the DBR in the init too!

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
_txt_nonote:
    .byte "            |",0
_txt_wavetable:
    .byte "="                   ; Square wave
    .byte "/"                   ; Right saw
    .byte "\\"                  ; Left saw
    .byte "^"                   ; Triangle
    .byte "#"                   ; Noise

    ;; DEV-LEVEL?
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
    ldx #0
    jsl cur_set_to              ; Wait forever for a key

    ;; Tracker datastructure setup
t_init: 
    jsr tracker_reset_all

t_refresh:
    pea _txt_inv_cur            ; Shut off cursor
    jsl sys_puts
    ply
    jsr print_status_bar        ; Do 'da tin

    ;; Main event loop
t_main_loop:
    jsr print_tracks

    ;;  Handle input events
    jsl cur_getch
    bcc t_main_loop             ; No key, just loop again
    cmp #KEY_ESC                ; EXIT?
    beq t_exit
    cmp #'~'                    ; FULL RESET?
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
        
    jmp t_main_loop

;;; Full quit back to MONITOR
t_exit:
    pea _txt_vis_cur            ; Turn back on cursor
    jsl sys_puts
    ply
    lda #0                      ; User-generated exit
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
    bra t_main_loop

    
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
print_status_bar:
    jsl cur_home                ; Top left of screen
    jsl cur_standend            ; Clear attributes
    jsl cur_setattr_inv         ; Invert text
    pea _txt_title_bar          ; Get title text
    jsl sys_puts
    ply                         ; Correct stack
    jsl cur_clrattr_inv         ; Clear inversion
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
    pha
pt_beats:
    pea _txt_eol                ; Newline
    jsl sys_puts
    ply
    
    pla                         ; Get beat number
    cmp <beat_index             ; If this is the current beat, higilight line
    bne pt_beatnum
    pha
    jsl cur_setattr_inv
    pla
pt_beatnum: 
    tay                         ; Save A
    jsl sys_puthex              ; Print beat number
    iny
    tya
    pha
    
    lda #'|'
    jsl sys_putc
    
    ;; Inner loop: Iterate over tracks
    txy
pt_ti:  
    lda beats, y                ; Get beat data
    bne pt_ti_prnote            ; Non-zero = print note
    phy
    pea _txt_nonote             ; Zero = blank entry
    jsl sys_puts
    pla                         ; Restore stack
    pla
    ply
    bra pt_ti_next
pt_ti_prnote:
    ;; TODO: figure out how to print a note!
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
    bmi pt_ti                   ; Not doen with entries, do next voice
    
    ;; Done with line, set up next outer loop
    jsl cur_clrattr_inv         ; Make sure that inversion is not carried to next line
    
    inx
    cpx #BEATS_TO_PRINT
    bmi pt_beats
    pla
    rts
    

;;; Clear all tracker data
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

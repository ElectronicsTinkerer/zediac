;;;
;;;  ZEDIAC CURSES IMPLEMENTATION
;;;
;;; (C) Zach Baldwin 2023
;;;
;;; Updates:
;;; 2023-12-07: Created file
;;; 2023-12-08: Added debug test
;;;

#include "../monitor/zediac-inc-1.8.0.inc"

;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------

do_echo         .equ $80            ; 8  bits - BOOL
getch_timeout   .equ $81            ; 16 bits - in ms
    
;;; ------------------------------------
;;;  LIBRARY LOAD BASE
;;; ------------------------------------
    
    .org $7000
    
    ;; BEGIN LOADER BLOCK
    .word __REGION_1_END - __REGION_1_BEGIN
    .word __REGION_1_BEGIN & $ffff
    .byte {__REGION_1_BEGIN >> 16} & $ff
    ;; END LOADER BLOCK

__REGION_1_BEGIN:
    ;; BEIGN DEBUG
    .as
    .xl
    sep #$20
    rep #$10
    lda #0                      ; Set the DBR to a known value
    pha
    plb
    
    ldx #14
    ldy #14
    jsl cur_movexy              ; Print a test string
    pea _dummy_text
    jsl sys_puts
    ply
    rtl

_dummy_text:
    .byte "HELLO, WORLD!",0
    ;; END DEBUG
    
;;; ------------------------------------
;;;  TEXT BANK
;;; ------------------------------------
;;; Attributes
_txt_bold:
    .byte "^[[1m",0             ; Set text to bold
_txt_dim:
    .byte "^[[2m",0             ; Set text to dim
_txt_uline:
    .byte "^[[4m",0             ; Set text to underlined
_txt_blink:
    .byte "^[[5m",0             ; Set text to blinking
_txt_inv:
    .byte "^[[7m",0             ; Set text to reverse
_txt_un_bold: /* same as un_dim! */
_txt_un_dim:
    .byte "^[[22m",0
_txt_un_uline:
    .byte "^[[24m",0
_txt_un_blink:
    .byte "^[[25m",0
_txt_un_inv:    
    .byte "^[[27m",0
;;; Screen/cursor manipulation
_txt_reset:
    .byte "^[[0m",0             ; Reset text mode to default
_txt_clr_scrn:
    .byte "^[[2J^[[H",0         ; Clear screen and home cursor to (0,0)
_txt_backspace:
    .byte "\b^[[K",0            ; Back up one char then delete at cursor
_txt_move:
    .byte "^[[",0               ; Set up cursor movement
_txt_home:
    .byte "^[[H",0              ; Set cursor to (0,0)
    
    
;;; ------------------------------------
;;;  LIBRARY FUNCTIONS
;;; ------------------------------------

;;; Turn off attributes and set normal mode
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
cur_standend:
    pea _txt_clr_scrn
    jsl sys_puts
    ply
    rtl

    
;;; Back up the cursor one position and delete the character
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
cur_backupcur:
    pea _txt_backspace
    jsl sys_puts
    ply
    rtl

    
;;; Set text to DIM
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
cur_setattr_dim:
    pea _txt_dim
    jsl sys_puts
    ply
    rtl


;;; Set text to BOLD
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
cur_setattr_bold:
    pea _txt_bold
    jsl sys_puts
    ply
    rtl


;;; Set text to ULINE
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
cur_setattr_uline:
    pea _txt_uline
    jsl sys_puts
    ply
    rtl


;;; Set text to BLINK
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
cur_setattr_blink:
    pea _txt_blink
    jsl sys_puts
    ply
    rtl


;;; Set text to INVERSE
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
cur_setattr_inv:
    pea _txt_inv
    jsl sys_puts
    ply
    rtl


;;; Unset text from DIM
;;; Unset text from BOLD
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
cur_clrattr_bold:
cur_clrattr_dim:
    pea _txt_un_dim
    jsl sys_puts
    ply
    rtl


;;; Unset text from UNDERLINE
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
cur_clrattr_uline:
    pea _txt_un_uline
    jsl sys_puts
    ply
    rtl


;;; Unset text from BLINK
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
cur_clrattr_blink:
    pea _txt_un_blink
    jsl sys_puts
    ply
    rtl


;;; Unset text from INVERSE
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
cur_clrattr_inv:
    pea _txt_un_inv
    jsl sys_puts
    ply
    rtl

    
;;; Clear the screen and home cursor to (0,0)
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
cur_clear:
    pea _txt_clr_scrn
    jsl sys_puts
    ply
    rtl


;;; Home cursor to (0,0)
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
cur_home:
    pea _txt_home
    jsl sys_puts
    ply
    rtl


;;; Set cursor position to (X,Y)
;;; Requirements:
;;;   .xl
;;;   .as
;;; Args:
;;;   X - X position to put cursor
;;;   Y - Y position to put cursor
;;; Uses:
;;;   A, Y
;;; Return:
;;;   NONE
;;; 
    .xl
    .as
cur_movexy:
    phy                         ; Save Y
    pea _txt_move
    jsl sys_puts
    ply                         ; Restore stack
    ply                         ; Get line number
    tya                         ; Don't care about upper 8 bits
    phx
    clc                         ; Don't print leading 0's
    jsl sys_putdec              ; Print line number
    lda #';'
    jsl sys_putc
    plx                         ; Get column number
    txa
    clc                         ; Don't print leading 0's
    jsl sys_putdec
    lda #'H'                    ; End the curses function
    jsl sys_putc
    rtl


;;; Attempt to read a key and if there is one available, return it
;;; Requirements:
;;;   .as
;;;   .xl - Only required if timeout > 255 ms
;;; Args:
;;;   NONE
;;; Uses:
;;;   A, X
;;; Return:
;;;   A contains the received character, if available
;;;   p.c = 1 if chararcter was available, 0 otherwise
;;;
    .as
cur_getch:
    ldx <getch_timeout
    bne _cur_gch_to             ; Technically speaking, this does not need
                                ; to branch based on the timeout but it should
                                ; (possibly) reduce some overhead from the
                                ; sys_getc_to function.
    jsl sys_getc                ; Wait for key
    bra _cur_gch_echochk

_cur_gch_to:
    jsl sys_getc_to             ; Timeout is in X, wait for character
    
_cur_gch_echochk:
    pha
    lda <do_echo
    beq _cur_gch_done           
    ;; php
    jsl sys_putc                ; If the character echo mode is enabled,
                                ; then echo the character
    ;; plp
_cur_gch_done:  
    pla
    rtl        
    

;;; Set getch timeout
;;; Requirements:
;;;   .xl
;;; Args:
;;;   X - Delay in ms to wait for a character before timing out
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
;;;
    .xl
cur_set_to:
    ;; Honestly should be a macro...
    stx <getch_timeout
    rtl


;;; Enable echo of input characters
;;; Requirements:
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
;;;    
    .as
cur_echo_on:
    sec                         ; Don't use a GP reg
    rol <do_echo
    rtl


;;; Disable echo of input characters
;;; Requirements:
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
;;;    
    .as
cur_echo_off:
    ;; Probably should also bve a macro ...
    stz <do_echo
    rtl

__REGION_1_END:


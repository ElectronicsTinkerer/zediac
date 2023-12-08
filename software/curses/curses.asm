;;;
;;;  ZEDIAC CURSES IMPLEMENTATION
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-12-07: Created file
;;;

#include "../monitor/zediac-inc-1.7.8.inc"

;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------

do_echo     .equ $00            ; BOOL

;;; ------------------------------------
;;;  LIBRARY LOAD BASE
;;; ------------------------------------
    
    .rom $1000
    .org $001000

;;; ------------------------------------
;;;  TEXT BANK
;;; ------------------------------------
_txt_reset:
    .byte "^[[0m",0             ; Reset text mode to default
_txt_dim:
    .byte "^[[2m",0             ; Set text to dim
_txt_clr_scrn:
    .byte "^[[2J^[[H",0         ; Clear screen and home cursor to (0,0)
_txt_backspace:
    .byte "\b^[[K",0            ; Back up one char then delete at cursor

;;; ------------------------------------
;;;  LIBRARY FUNCTIONS
;;; ------------------------------------

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
cur_clear:
    pea _txt_clr_scrn
    jml sys_puts
    ply
    rtl
    

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
cur_standend:
    pea _txt_clr_scrn
    jml sys_puts
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
cur_standend:
    pea _txt_backspace
    jml sys_puts
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
cur_standend:
    pea _txt_dim
    jml sys_puts
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
cur_standend:
    pea _txt_backspace
    jml sys_puts
    ply
    rtl

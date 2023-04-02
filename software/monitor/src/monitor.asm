;;;
;;;  ZEDIAC MONITOR
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-04-01: Created file
;;;
;;;

;;; ROM Size & base address
    .rom $8000
    .org $8000

;;; ------------------------------------
;;;  ENTRY POINT
;;; ------------------------------------
emu_vector_reset:   
    cld
    ;; clc                         ; Native mode
    ;; xce
    
;;; ------------------------------------
;;;  INTERRUPT HANDLERS
;;; ------------------------------------
vector_cop:
vector_brk:
vector_abort:
vector_nmi:
vector_irq:
emu_vector_cop:
emu_vector_abort:
emu_vector_nmi:
emu_vector_irq:
    rti

    
;;; ------------------------------------
;;;  CPU VECTORS
;;; ------------------------------------
    .org $ffe4
    ;; Native mode vectors
    .word vector_cop
    .word vector_brk
    .word vector_abort
    .word vector_nmi
    .word 0                     ; Reserved
    .word vector_irq
    ;; Emulation mode vectors
    .word emu_vector_cop
    .word 0                     ; Reserved
    .word emu_vector_abort
    .word emu_vector_nmi
    .word emu_vector_reset
    .word emu_vector_irq        ; Also BRK
    
   

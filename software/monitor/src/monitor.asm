;;;
;;;  ZEDIAC MONITOR
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-04-01: Created file
;;; 2023-04-28: Added header includes for HW and keyboard
;;; 2023-05-16: Add VIA and UART initialization
;;;

#include "../inc/hw.inc"
#include "../inc/keyboard.inc"    
;; #include "../inc/macros.inc"
    
;;; ROM Size & base address
    .rom ROM_SIZE
    .org ROM_BASE

;;; ------------------------------------
;;;  ENTRY POINT
;;; ------------------------------------
emu_vector_reset:   
    sei                         ; Disable IRQs
    clc                         ; Switch to native mode
    xce
    
    .xl
    rep #$10
    ldx #$01ff                  ; Init stack
    txs
    .as
    .xs
    sep #$30                    ; 8-bit mode

    ;; a_short()

    ;; Initialize VIA to inputs
    stz VIA0_DDRB
    stz VIA0_DDRA

    ;; UART0 INIT
    lda #$80                    ; Switch to divisor register access 
    sta UART0_LCR
    lda #96                     ; Set BAUD to 19200
    sta UART0_DLL               ; Low byte
    lda #0
    sta UART0_DLM               ; High byte
    lda #$03                    ; 8-bits, no parity, 1 stop bit, switch back to data registers
    sta UART0_LCR
    lda #$00                    ; Disable Receive buffer and Transmitter Holding Reg Empty interrupts
    sta UART0_IER
    lda #$87                    ; Set 8-byte interrupt trigger and R/T FIFO (Also reset Tx and Rx FIFOs)
    sta UART0_FCR
    lda #$13                    ; Set DTR and RTS, enable loopback (for selftest)
    sta UART0_MCR
    lda #'A'                    ; Test value
    sta UART0_THR            
    jsr _delay                  ; Wait for byte to be recieved
    lda UART0_RHR
    cmp #'A'                    ; Did we get the same character back?
    beq _uart0_init_good        ; Yes
    jmp _err                    ; No, indicate error condition
_uart0_init_good: 
    lda #$03                    ; Enable Receive buffer and Transmitter Holding Reg Empty interrupts
    sta UART0_IER
    sta UART0_MCR
    ;; END: UART0 INIT

;;; TODO: Correct UART baud rate, setup FIFOs, HW handshaking, etc...

    
;;; ------------------------------------
;;;  UTILITY FUNCTIONS
;;; ------------------------------------
_err:
    jmp _err                    

;;; ------------------------------------
;;;  SYSCALL FUNCTIONS
;;; ------------------------------------
;;; THESE SHOULD NOT BE CALLED DIRECTLY!
;;; USE THE SYSCALL TABLE THROUGH THE COP
;;; INSTRUCTION.
    
;;; Delay for some amount of time
;;; Args:
;;;   X - x1000 cycles to wait (DESTRUCTIVE)
;;; Uses:
;;;   Y
;;; Return:
;;;   NONE
_delay:
    .as
    .xl
    rep #$20
    sep #$10
    txy
    ;;  TODO! 
    rti

_puthex:
_putdec:
_puthex_word:
_putdec_word:
_puts:
    rti

;;; ------------------------------------
;;;  SYSCALL TABLE
;;; ------------------------------------
;;; SYSCALLS can be accessed by loading
;;; X with the syscall index then executing
;;; a COP instruction. The syscalls do not
;;; care about M/X widths.
_syscall_table: 
    .word _delay
    .word _puthex
    .word _putdec
    .word _puthex_word
    .word _putdec_word
    .word _puts
    
;;; ------------------------------------
;;;  INTERRUPT HANDLERS
;;; ------------------------------------
vector_cop:
    jmp (_syscall_table, x)

vector_brk:
vector_abort:
vector_nmi:
vector_irq:
emu_vector_cop:
emu_vector_abt:
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

    .org $fff4
    ;; Emulation mode vectors
    .word emu_vector_cop
    .word 0                     ; Reserved
    .word emu_vector_abt
    .word emu_vector_nmi
    .word emu_vector_reset
    .word emu_vector_irq        ; Also BRK
    
   

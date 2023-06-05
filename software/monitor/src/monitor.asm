;;;
;;;  ZEDIAC MONITOR
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-04-01: Created file
;;; 2023-04-28: Added header includes for HW and keyboard
;;; 2023-05-16: Add VIA and UART initialization
;;; 2023-06-03: Implement basic IO and print utilities; start monitor prompt
;;;

#include "../inc/syscalls.inc"
    
#include "../inc/hw.inc"
#include "../inc/keyboard.inc"    
;; #include "../inc/macros.inc"
    
;;; ROM Size & base address
    .rom ROM_SIZE
    .org ROM_BASE

direct_page     .equ $7f00
arg_stack       .equ $0         
buf             .equ $010000    ; Input line buffer

;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------
syscall_tmp0    .equ $0
syscall_tmp1    .equ $1
syscall_tmp2    .equ $2
syscall_tmp3    .equ $3
syscall_tmp4    .equ $4
syscall_tmp5    .equ $5
syscall_tmp6    .equ $6
syscall_tmp7    .equ $7

enum mon {
    tmp0 = $10,
    tmp1,
    tmp2,
    tmp3,
    tmp4,
    tmp5,
    tmp6,
    tmp7
}

xsav            .equ mon.tmp0
ysav            .equ mon.tmp2
arg_sp          .equ mon.tmp4
line_start      .equ mon.tmp6
    
info_y          .equ mon.tmp0

    
;;; ------------------------------------
;;;  TEXT BANK
;;; ------------------------------------
_txt_prompt:
    .byte "[ ^[[32mZEDIAC^[[0m ] > ",0
_txt_eol_reset:
    .byte "^[[0m\n",0        ; Reset style to term default
_txt_clr_scrn:
    .byte "^[[2J^[[H",0      ; Clear screen and home cursor to (0,0)
_txt_backspace:
    .byte "^[[K",0           ; Delete at cursor
_txt_unk_cmd:
    .byte "Command not found.\n",0
_txt_help:
    .byte "Available commands:\n"
    .byte " help ... Display avaiable commands\n"
    .byte " clear .. Clear the terminal\n"
    .byte 0

    
;;; ------------------------------------
;;;  COMMAND BANK
;;; ------------------------------------
;;; Command table (each entry is 8 bytes):
;;; 2 bytes: jump address for command
;;; 6 bytes: (zero-terminated or 6 chars) string of command

CMD_ENTRY_SIZE  .equ 8           ; In bytes (must be a power of 2)
CMD_STR_SIZE    .equ 6           ; In bytes

    ;; Each entry must be aligned by the size of an entry, hence the
    ;; funky .org expression before each entry
    .org {$ & {CMD_ENTRY_SIZE-1} != 0} * CMD_ENTRY_SIZE + {$ & ~{CMD_ENTRY_SIZE-1}}
_cmd_table:
    .word _help
    .byt "help",0
    .org {$ & {CMD_ENTRY_SIZE-1} != 0} * CMD_ENTRY_SIZE + {$ & ~{CMD_ENTRY_SIZE-1}}
    .word _clear
    .byt "clear",0
    .org {$ & {CMD_ENTRY_SIZE-1} != 0} * CMD_ENTRY_SIZE + {$ & ~{CMD_ENTRY_SIZE-1}}
    .word _info
    .byt "info",0
_cmd_table_end:                 ; Keep me! Used to determine number of entries in table


;;; ------------------------------------
;;;  ENTRY POINT
;;; ------------------------------------
emu_vector_reset:   
    sei                         ; Disable IRQs
    clc                         ; Switch to native mode
    xce
    
    .xl
    .al
    rep #$30
    ldx #direct_page-1          ; Init stack
    txs
    lda #direct_page            ; Set direct page to just above stack
    tcd
    .as
    .xs
    sep #$30                    ; 8-bit mode

    ;; Initialize VIA to inputs
    stz VIA0_DDRB
    stz VIA0_DDRA

    ;; UART0 INIT
    lda #$80                    ; Switch to divisor register access 
    sta UART0_LCR
    lda #8                      ; Set BAUD to 115200 (clock in is 8 * 1.8432MHz)
    sta UART0_DLL               ; Low byte
    lda #0
    sta UART0_DLM               ; High byte
    lda #$03                    ; 8-bits, no parity, 1 stop bit, switch back to data registers
    sta UART0_LCR
    lda #$00                    ; Disable Receive buffer and Transmitter Holding Reg Empty interrupts
    sta UART0_IER
    lda #$01                    ; Enable Tx/Rx FIFOs
    sta UART0_FCR
    lda #$c7                    ; Set 60-byte interrupt trigger and reset Tx and Rx FIFOs
    sta UART0_FCR
    lda #$13                    ; Set DTR and RTS, enable loopback (for selftest)
    sta UART0_MCR
    lda #'A'                    ; Test value
    sta UART0_THR
    ldy #2                      ; 2000 cycles
    ldx #SYS_DELAY
    cop 0                       ; Wait for byte to be recieved
    lda UART0_RHR
    cmp #'A'                    ; Did we get the same character back?
    beq _uart0_init_good        ; Yes
    jmp _err                    ; No, indicate error condition
_uart0_init_good: 
    lda #$03                    ; Enable Receive buffer and Transmitter Holding Reg Empty interrupts
    sta UART0_MCR
    ldx UART0_LCR               ; Save for later
    lda #$bf                    ; Enable access to EFR
    sta UART0_LCR
    lda #$c0                    ; Enable automatic CTS/RTS control
    sta UART0_EFR
    stx UART0_LCR               ; Restore line settings
    ;; END: UART0 INIT

    ;; Clear screen
    pea _txt_clr_scrn           ; Display prompt
    ldx #SYS_PUTS
    cop 0
    plx                         ; Restore stack

    ;; MONITOR PROMPT
monitor:    
    .xl
    .as
    rep #$10
    sep #$20
_mon_prompt:
    pea _txt_prompt             ; Display prompt
    ldx #SYS_PUTS
    cop 0
    plx                         ; Restore stack

    ldx #0                      ; Set up line index
    
_mon_next:  
    jsr _getc                   ; Get a charcter (blocking)
    jsr _putc
    cmp #03                     ; ^C?
    beq _mon_prompt             ; Yes, reset line
    cmp #KEY_LF                 ; Enter?
    beq _mon_exec               ; Yes, run the command
    cmp #KEY_BS                 ; Backspace?
    beq _mon_backspace          ; Yes
    ;; Otherwise, try to add char to buffer
    
    sta buf,x                   ; Buffer starts at address 0 in bank 1
    inx
    bpl _mon_next               ; If not in out of 32K mem, just keep going
    ldx #$7fff                  ; Maximum X value
    jmp _mon_next

_mon_backspace:
    phx
    ;; pea _txt_backspace          ; Delete char
    ;; ldx #SYS_PUTS
    ;; cop 0
    ;; plx                         ; Restore stack
    plx
    dex                         ; Back up index
    bpl _mon_next               ; If at beginning of line, we're done
    lda #' '                    ; otherwise, restore deleted char
    jsr _putc
    lda #$08                    ; Bell
    jsr _putc
    ldx #0                      ; and reset the line index
    jmp _mon_next

    ;; Run a command in the buffer
_mon_exec:
    cpx #0                      ; Ignore lines with no content
    beq _mon_prompt
    stx xsav                    ; Save X for later comparison

    ;; Remove any whitespace from the front of the command
    ldx #-1
_me_rm_wspc:
    inx
    cpx xsav                    ; End of line = no command present
    bcs _mon_prompt
    lda buf,x                   ; Get char from line
    cmp #' '
    beq _me_rm_wspc             ; Is a space
    cmp #'\t'
    beq _me_rm_wspc             ; Is a tab
    stx line_start              ; Save the starting position of the command
    
    ;; Parse line. Jumps to a command after tokenizing the remainder of the line.
    ;; Pushes last arg first. The last data pushed is a single byte count of the number
    ;; of arguments available on the stack

    ldy #0                      ; Command table index (O(n) search)
_me_loop_init:
    ldx line_start              ; X is line index
_me_loop:
    lda _cmd_table+2,y          ; Get char from command entry
    beq _me_eoc_chk
    cmp buf,x                   ; Get char from line
    bne _me_eoc_chk
    iny
    inx
    cpy #CMD_STR_SIZE
    bne _me_loop
    dex
    ;; If max string size, check for equality
_me_eoc_chk:
    cmp #0
    beq _me_eoc_gc
    cpy #CMD_STR_SIZE
    bne _me_eoc_chk_nxt
    ;; FT
_me_eoc_gc:
    cpx xsav                    ; If at end of entered line, setup stack
    beq _me_args
    lda buf,x                   ; Get char from line
    cmp #' '                    ; Check to see if it is whitespace
    beq _me_args
    cmp #'\t'
    bne _me_eoc_chk_nxt         ; Not whitespace, try another command
    ;; FT
_me_args:
    ;; A command was matched.
    ;; Now go through the remainder of the line and set up the args on the stack.
    sty ysav
    .al                         ; Set up arg pre-stack
    rep #$20
    lda #arg_stack
    sta arg_sp
    .as
    sep #$20
    cpx xsav                    ; End of line = done parsing args
    bcs _me_run_cmd

_me_args_delim:
    lda #0
    sta buf,x                   ; Tokenize inputs
_me_ad_next:
    inx
    cpx xsav                    ; End of line = done parsing args
    bcs _me_cmd_setup
    lda buf,x                   ; Get char from line
    cmp #' '
    beq _me_args_delim          ; Is a space
    cmp #'\t'
    beq _me_args_delim          ; Is a tab
    ;; Not a space: put it's pointer onto the stack
    .al
    rep #$20
    txa
    sta (arg_sp)
    inc arg_sp                  ; Post-inc the SP
    inc arg_sp
    .as
    sep #$20
    bra _me_ad_next
    
_me_cmd_setup:
    ;; Set up the stack with args pointers
    .al
    rep #$20
    ldx arg_sp
_me_cs_loop:
    txy                         ; Test if x is zero
    beq _me_run_cmd             ; Arg pseudo stack is empty => we're done here
    dex                         ; Pre-dec the SP
    dex
    lda |0,x                    ; Get the arg pointer (force abs addressing)
    pha                         ; and put it onto the call stack
    bra _me_cs_loop
    
_me_run_cmd:
    .as
    sep #$20
    lda arg_sp
    lsr                         ; Divide stack pointer by two
    pha                         ; Store the number of arguments to the command as the last entry on the stack
    .al
    rep #$20
    lda ysav                    ; Restore command table index
    and #{$ffff-{CMD_ENTRY_SIZE-1}} ; Mask off lowest bits (indicating index into the current entry)      
    tax                         ; X now has the base address of the entry (which is a pointer)
    .as
    sep #$20
    jmp (_cmd_table,x)          ; Execute command

_me_eoc_chk_nxt:
    .al
    rep #$20
    ;; Increment index to next command entry
    tya
    and #{$ffff-{CMD_ENTRY_SIZE-1}} ; Mask off lowest bits (which indicate the index into the current entry)
    clc
    adc #CMD_ENTRY_SIZE
    tay
    .as
    sep #$20
    cpy #_cmd_table_end - _cmd_table ; Get the size of the table in bytes
    bcs _me_cmd_invalid
    jmp _me_loop_init

_me_cmd_invalid:    
    ;; Not a valid command, output an error
    pea _txt_unk_cmd            ; Text string
    ldx #SYS_PUTS
    cop 0
    plx                         ; Restore stack
    jmp _mon_prompt
    
    
;;; ------------------------------------
;;;  COMMANDS
;;; ------------------------------------

_help:
    pea _txt_help
    ldx #SYS_PUTS
    cop 0
    plx
    jmp monitor

    
_clear:
    pea _txt_clr_scrn
    ldx #SYS_PUTS
    cop 0
    plx
    jmp monitor

;;; Prints argument info (for debugging)
_info:
    pea _txt_info_arg_cnt
    ldx #SYS_PUTS
    cop 0
    plx
    pla                         ; Get argc off stack
    sta info_y                  ; and save for later
    ldx #SYS_PD
    cop 0
    lda '\n'
    jsr _putc

    lda info_y
    asl                         ; Multiply count by 2
    tay
_info_argv_loop:
    beq _info_done
    phy
    pea _txt_info_arg           ; Print "arg"
    ldx #SYS_PUTS
    cop 0
    plx
    ply
    lda (0,s),y                 ; Get the pointer to the arg string
    pha
    ldx #SYS_PUTS
    cop 0
    plx
    lda '\n'
    dey
    dey
    jsr _putc
    
    jmp _info_argv_loop
    
_info_done: 
    jmp monitor

_txt_info_arg_cnt:
    .byte "argc: ",0
_txt_info_arg:
    .byte "arg: ",0
    
;;; ------------------------------------
;;;  UTILITY FUNCTIONS
;;; ------------------------------------
_err:
    jmp _err

    
;;; Print a character to UART0
;;; Requirements:
;;;   .as
;;; Args:
;;;   A - The character to print
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
_putc:                          ; "echo" from older kernels
    pha
_putc_loop: 
    lda UART0_LSR               ; Check for Tx empty
    and #$20
    beq _putc_loop              ; Not enough empty spaces in Tx FIFO
    pla
    sta UART0_THR
    rts

;;; Get a single character from UART0, blocking
;;; Requirements:
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A
;;; Return:
;;;   A contains the received character
_getc:  
    lda UART0_LSR               ; Check for Rx data (4)
    and #$01                    ; (2)
    beq _getc                   ; No Rx data, keep checking
    lda UART0_RHR               ; Yes, Get byte from Rx FIFO (4)
    rts

    
;;; Delay for some amount of time
;;; Requirements:
;;;   NONE
;;; Args:
;;;   Y - Y * ~1000 cycles to wait (DESTRUCTIVE)
;;;       NOTE: a value of 0 is maximum delay, 1 is minimum
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
_delay:
    php
    .xl
    sep #$10
_d_init:   
    ldx #248
_d_loop:
    dex
    bne _delay_loop
    dey
    bne _delay_init
    plp
    rts


    
;;; ------------------------------------
;;;  SYSCALL FUNCTIONS
;;; ------------------------------------
;;; THESE SHOULD NOT BE CALLED DIRECTLY!
;;; USE THE SYSCALL TABLE THROUGH THE COP
;;; INSTRUCTION.

    
;;; Delay for some amount of time
;;; Requirements:
;;;   NONE
;;; Args:
;;;   Y - Y * ~1000 cycles to wait (DESTRUCTIVE)
;;;       NOTE: a value of 0 is maximum delay, 1 is minimum
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
_sys_delay:
    .xl
    rep #$10
_sys_delay_init:   
    ldx #199
_sys_delay_loop:
    dex
    bne _sys_delay_loop
    dey
    bne _sys_delay_init
    rti

    
;;; Print a word (in A) as HEX to UART0
;;; Requirements:
;;;   NONE
;;; Args:
;;;   A - The byte to print (DESTRUCTIVE)
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
_sys_puthex_word:
    .as
    sep #$20
    xba
    ldx #SYS_PH
    cop 0
    xba
    jmp _sys_puthex

    
;;; Print a byte (in A) as HEX to UART0
;;; Requirements:
;;;   NONE
;;; Args:
;;;   A - The byte to print (DESTRUCTIVE)
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
_sys_puthex:
    .as
    sep #$20
    pha
    lsr
    lsr
    lsr
    lsr
    jsr _prhex
    pla
    jsr _prhex
    rti

_prhex:                     ; Prints a single hex digit in LSN of A
    and #$0f                    ; Get LSD
    ora #'0'                    ; Add "0"
    cmp #'9'+1                  ; Is it a decimal digit?
    bcc _putc                   ; Yes! output it.
    adc #$06                    ; No, convert it to ascii char for A-F
    jsr _putc
    rts
    
    
;;; Subroutine to print a byte in A in dec form (destructive)
;;; Requirements:
;;;   NONE
;;; Args:
;;;   A - The byte or word to print (DESTRUCTIVE)
;;;       The width of A (M flag) determines if a word or byte
;;;       is printed.
;;; Uses:
;;;   X
;;; Return:
;;;   NONE
;;; See: http://www.6502.org/source/integers/hex2dec-more.htm
;;; converted for 65816
_sys_putdec:
_sys_putdec_word:
    php                         ; Save A's width
    .al
    rep #$20
    stz syscall_tmp4
    plp                         ; Restore A's width
    sta syscall_tmp4            ; Save binary value to be converted
    .al
    .xs
    rep #$20
    sep #$18                    ; Also set D flag
    stz syscall_tmp0            ; Clear temp var
    stz syscall_tmp1            ; 16-bit A, so should clear bcd_tmp+2
    ldx #16                     ; Number of bits to do

_sys_putdec_cnvbit:
    asl syscall_tmp4            ; Shift out bit
    lda syscall_tmp0            ; Add into result
    adc syscall_tmp0
    sta syscall_tmp0
    .as
    sep #$20
    lda syscall_tmp2            ; Upper byte
    adc syscall_tmp2
    sta syscall_tmp2
    .al
    rep #$20
    dex                         ; Next bit
    bne _putdec_cnvbit
    
    lda syscall_tmp1            ; Upper 2 bytes of decimal number
    ldx #SYS_PHW
    cop 0                       ; Print word
    .as
    sep #$20
    lda syscall_tmp0            ; this loads 8-bit value
    ldx #SYS_PHW
    cop 0                       ; Print word
    rti

    
;;; Get a single character from UART0, blocking
;;; Requirements:
;;;   NONE
;;; Args:
;;;   NONE
;;; Uses:
;;;   A
;;; Return:
;;;   A contains the received character
_sys_getc:
    .as
    sep #$20
_sys_getc_loop: 
    lda UART0_LSR               ; Check for Rx data (4)
    and #$01                    ; (2)
    beq _sys_getc_loop          ; No Rx data, keep checking
    lda UART0_RHR               ; Yes, Get byte from Rx FIFO (4)
    rti

    
;;; Get a single character from UART0, nonblocking
;;; Requirements:
;;;   NONE
;;; Args:
;;;   NONE
;;; Uses:
;;;   A
;;; Return:
;;;   A contains the received character, if available
;;;   c = 1 if chararcter was available, 0 otherwise
_sys_getc_nb:
    .as
    sep #$20
    lda UART0_LSR               ; Check for Rx data (4)
    and #$01                    ; (2)
    beq _sys_getc_nb_none       ; No Rx data, signal as such
    lda 2,S                     ; Get status reg vrom before interrupt
    ora #$01                    ; Set carry high
    sta 2,S                     ; Status Reg after RTI
    lda UART0_RHR               ; Yes, Get byte from Rx FIFO (4)
    rti 
_sys_getc_nb_none:  
    and #$fe                    ; Clear Carry
    sta 2,S                     ; Status Reg after RTI
    rti


;;; Send a single character to UART0, blocking
;;; Requirements:
;;;   NONE
;;; Args:
;;;   A - the character to send
;;; Uses:
;;;   A
;;; Return:
;;;   NONE    
_sys_putc:
    .as
    sep #$20
    pha
_sys_putc_loop: 
    lda UART0_LSR               ; Check for Tx empty
    and #$20
    beq _sys_putc_loop          ; Not enough empty spaces in Tx FIFO
    pla
    sta UART0_THR
    rti

    
;;; Echo a null-terminated string to the UART0
;;; Requirements:
;;;   NONE
;;; Args:
;;;   (S) - string to print
;;; Uses:
;;;   A, Y
;;; Return:
;;;   NONE
_sys_puts:
    .as
    .xl
    sep #$20
    rep #$10
    ldy #0
_sys_puts_loop:
	lda (5,s),y                 ; Get the first byte of the string at the strptr position
	beq _sys_puts_donstr	    ;  | If the value pulled is $00, we are done
    cmp #'\n'                   ; If newline, print a CR first
    bne _sys_puts_echo          ; Otherwise just print the character like normal
    lda #'\r'
    jsr _putc
    lda #'\n'
_sys_puts_echo:
	jsr _putc	                ;  \ Spit the char out if not $00
	iny     	                ; Increment string character pointer
	bne _sys_puts_loop          ; Loop if we have not hit the limit of the y index
        ;; Fall through if y-index is exhausted
_sys_puts_donstr:
    rti
    

;;; ------------------------------------
;;;  SYSCALL TABLE
;;; ------------------------------------
;;; SYSCALLS can be accessed by loading
;;; X with the syscall index then executing
;;; a COP instruction. The syscalls do not
;;; care about M/X widths.
_syscall_table: 
    .word _sys_delay
    .word _sys_puthex
    .word _sys_putdec
    .word _sys_puthex_word
    .word _sys_putdec_word
    .word _sys_puts
    .word _sys_getc
    .word _sys_putc

    
;;; ------------------------------------
;;;  INTERRUPT HANDLERS
;;; ------------------------------------
vector_cop:
emu_vector_cop:
    jmp (_syscall_table, x)

vector_brk:
vector_abort:
vector_nmi:
vector_irq:
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
    
   

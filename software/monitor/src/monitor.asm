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
;;; 2023-06-09: Update symbols to make use of new .def SLIME support
;;;

#include "../inc/syscalls.inc"
    
#include "../inc/hw.inc"
#include "../inc/keyboard.inc"    
;; #include "../inc/macros.inc"
    
;;; ROM Size & base address
    .rom ROM_SIZE
    .org ROM_BASE

smc_base0       .equ $7c00
smc_base1       .equ $7c80
arg_stack       .equ $7d00         
; stack_base    .equ $7e00      ; Really $7eff but for keeping things "page orientated" ...
direct_page     .equ $7f00
buf             .equ $010000    ; Input line buffer
xrecv_buf       .equ $020000    ; XMODEM receive / send buffer base

    
GETC_LOAD       .def 50_000     ; Clock cycles
    
;;; ------------------------------------
;;;  DIRECT PAGE VARIABLES
;;; ------------------------------------
xmon_ascii_line .def 0          ; ASCII temp storage for hex monitor

enum define syscall {
    tmp0 = $10,
    tmp1,
    tmp2,
    tmp3,
    tmp4,
    tmp5,
    tmp6,
    tmp7,
    tmp8,
    tmp9,
    tmp10,
    tmp11,
    tmp12,
    tmp13,
    tmp14,
    tmp15
}
    
enum define mon {
    tmp0 = $20,
    tmp1,
    tmp2,
    tmp3,
    tmp4,
    tmp5,
    tmp6,
    tmp7,
    tmp8,
    tmp9,
    tmp10,
    tmp11,
    tmp12,
    tmp13,
    tmp14,
    tmp15,
    tmp16,
    tmp17,
    tmp18,
    tmp19,
    tmp20,
    tmp21,
    tmp22,
    tmp23,
    tmp24,
    tmp25,
    tmp26,
    tmp27,
    tmp28,
    tmp29,
    tmp30,
    tmp31
}


s_px_l          .def syscall.tmp0
s_px_m          .def syscall.tmp1
s_px_h          .def syscall.tmp2
    
    
xsav            .def mon.tmp0   ; 16 bits
ysav            .def mon.tmp2   ; 16 bits
arg_sp          .def mon.tmp4   ; 16 bits
line_start      .def mon.tmp6   ; 16 bits
cmd_tbl_ptr     .def mon.tmp8   ; 16 bits
wspc_skip       .def mon.tmp10  ; 8  bits
xmon_mode       .def mon.tmp11  ; 8  bits
xmon_l          .def mon.tmp12  ; 8  bits
xmon_m          .def mon.tmp13  ; 8  bits
xmon_h          .def mon.tmp14  ; 8  bits
xmon_stl        .def mon.tmp15  ; 8  bits
xmon_stm        .def mon.tmp16  ; 8  bits
xmon_sth        .def mon.tmp17  ; 8  bits
xmon_xaml       .def mon.tmp18  ; 8  bits
xmon_xamm       .def mon.tmp19  ; 8  bits
xmon_xamh       .def mon.tmp20  ; 8  bits

gs_addr_l       .def mon.tmp0
gs_addr_m       .def mon.tmp1
gs_addr_h       .def mon.tmp2
gs_argc         .def mon.tmp3
gs_mode         .def mon.tmp4
    
args_y          .def mon.tmp0
args_end        .def mon.tmp2

xr_blk          .def mon.tmp0   ; 8  bits
xr_blkptr       .def mon.tmp1   ; 24 bits!
xr_chksum       .def mon.tmp4   ; 8  bits
xr_isnext       .def mon.tmp5   ; 8  bits
xr_retry_num    .def mon.tmp6   ; 8  bits

copy_s_l        .def mon.tmp0   ; 8  bits
copy_s_m        .def mon.tmp1   ; 8  bits
copy_s_h        .def mon.tmp2   ; 8  bits
copy_d_l        .def mon.tmp3   ; 8  bits
copy_d_m        .def mon.tmp4   ; 8  bits
copy_d_h        .def mon.tmp5   ; 8  bits
copy_c_l        .def mon.tmp6   ; 8  bits
copy_c_m        .def mon.tmp7   ; 8  bits
copy_c_h        .def mon.tmp8   ; 8  bits (written to but unused)
copy_xy         .def mon.tmp9   ; 16 bits
copy_mode       .def mon.tmp11  ; 8  bits


;;; ------------------------------------
;;;  TEXT BANK
;;; ------------------------------------
_txt_prompt:
    .byte "\n[ ^[[32mZEDIAC^[[0m ] > ",0
_txt_eol:
    .byte "\n", 0               ; End of line
_txt_eol_reset:
    .byte "^[[0m\n",0           ; Reset style to term default
_txt_reset:
    .byte "^[[0m",0             ; Reset text mode to default
_txt_dim:
    .byte "^[[2m",0             ; Set text to dim
_txt_clr_scrn:
    .byte "^[[2J^[[H",0         ; Clear screen and home cursor to (0,0)
_txt_backspace:
    .byte "\b^[[K",0            ; Back up one char then delete at cursor
_txt_unk_cmd:
    .byte ": Command not found.\n",0
_txt_help:
    .byte "Available commands:\n"
    .byte " > args [arg1] [...]        Print stack info for passed arguments\n"
    .byte " > clear                    Clear the terminal\n"
    .byte " > copy ssssss dddddd cccc  Copy c bytes from s to d\n"
    .byte " > ecopy ssssss dddddd cccc Copy c bytes from s to d (EEPROM)\n"
    .byte " > go xxxxxx                JML to an address\n"
    .byte " > gosub xxxxxx             JSL to an address (RTL to MONITOR)\n"
    .byte " > help                     Display available commands\n"
    .byte " > memmap                   Display the system's memory map\n"
    .byte " > xrecv                    XMODEM receive to address $020000\n"
    .byte " > a.b                      Hexdump from a to b\n"
    .byte " > a:b [c] [...]            Store b at address a\n"
    .byte 0
_txt_mem_map:
    .byte "Memory map - mirrored across all banks\n"
    .byte "\n"
    .byte " $0000-$7fff RAM (mirrored every 16 banks)\n"
    .byte " $8000-$800f VIA\n"
    .byte " $8800-$880f UART\n"
    .byte " $9000-$9fff *unused*\n"
    .byte " $a000-$a00f AUXCSB0\n"
    .byte " $a800-$a80f AUXCSB1\n"
    .byte " $b000-$b00f AUXCSB2\n"
    .byte " $b800-$b80f AUXCSB3\n"
    .byte " $c000-$ffff ROM (A23 determines low/high 16k)\n"
    .byte "\n"
    .byte "Interrupts:\n"
    .byte " COP/BRK jump to reset vector\n"
    .byte " ABT     rti\n"
    .byte " NMI     $0000\n"
    .byte " IRQ     $0080\n"
    .byte 0
_txt_startup:
    .byte "^[[2J^[[H" // Clear screen
    .byte "Welcome to the ...\n"
    .byte "\n"
    .byte "##########################\n"
    .byte "#  .___ ___ ___ ___ __   #\n"
    .byte "#   __/_[__ |  \ | [__]  #\n"
    .byte "#  ./__.[___|__/_|_|  |  #\n"
    .byte "#    COMPUTER  SYSTEM    #\n"
    .byte "##########################\n"
    .byte "\n"
    .byte "(C) Ray Clemens 2023\n"
    .byte "Monitor : v1.3a (2023-07-06)\n"
    .byte "RAM : 512k\n"
    .byte "ROM : 32k\n"
    .byte "CPU : 65816\n"
    .byte 0

    
;;; ------------------------------------
;;;  COMMAND BANK
;;; ------------------------------------
;;; Command table
;;; 2 bytes: jump address for command
;;; 1 byte:  length of command
;;; n bytes: the command string

_cmd_table:
    .word _help
    .byte _cmd_help_end - $
    .byte "help"
_cmd_help_end:
    .word _clear
    .byte _cmd_clear_end - $
    .byte "clear"
_cmd_clear_end:
    .word _args
    .byte _cmd_args_end - $
    .byte "args"
_cmd_args_end:
    .word _xrecv
    .byte _cmd_xrecv_end - $
    .byte "xrecv"
_cmd_xrecv_end:
    .word _go
    .byte _cmd_go_end - $
    .byte "go"
_cmd_go_end:
    .word _gosub
    .byte _cmd_gosub_end - $
    .byte "gosub"
_cmd_gosub_end:
    .word _copy
    .byte _cmd_copy_end - $
    .byte "copy"
_cmd_copy_end:
    .word _eeprom
    .byte _cmd_eeprom_end - $
    .byte "ecopy"
_cmd_eeprom_end:
    .word _memmap
    .byte _cmd_memmap_end - $
    .byte "memmap"              ; no zero terminator! -> 6 chars long
_cmd_memmap_end:
    
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
    jsl sys_delay               ; Wait for byte to be recieved
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

    ;; Startup banner
    .xl
    rep #$10                    ; PUTS requires long X
    pea _txt_startup
    jsl sys_puts
    plx                         ; Restore stack

;;; ============= MONITOR PROMPT =============
;;; The MONITOR allows passing arguments to commands.
;;; It does this by pushing the following items onto the stack:
;;; 
;;; top ---> 8 bits  | argc (8-bit)
;;;          16 bits | address of argv[0] on the stack
;;;          16 bits | argv[0]
;;;          16 bits | argv[1]
;;;          ...
;;; 
;;; Why a 16-bit address as the penultimate stack entry? -> This
;;; allows the (d,s),y addressing mode to grab an arbitrary entry
;;; off the stack without having to do a bunch of popping or SMC.
;;; 
;;; Additionally, the address of each argv entry is within the
;;; same bank as the monitor's input buffer since the parser
;;; simply replaces whitespace with a null terminator for each
;;; argument.
;;;
;;; The length of the input buffer which is filled is passed via
;;; the Y register (.xl)
;;; 
;;; There is a max of 100 command arguments. Any arguments past this
;;; number are simply tokenized into a single string.
;;;
;;; Once all commands in the above command table have been compared
;;; and no match has been found, then the input line is parsed by
;;; the "xmon" (hexmonitor), which (as always with my projects) is a
;;; modified version of the Woz Monitor from the Apple 1.
;;; (I can't understate how useful that simple 254 byte program is
;;;  as both a standalone tool and base to build off of)
;;;
;;; MEMORY: The monitor uses bank $01 as its input buffer.
;;; 
monitor:
    sei                         ; Disable IRQs
    .xl
    .al
    rep #$30
    lda #direct_page            ; Set direct page to just above stack
    tcd
    .as
    sep #$20
    ldx #direct_page-1          ; Init stack (REQUIRED - many commands assume they can trash the stack!)
    txs
    lda #0                      ; Set data bank to 0
    pha
    plb
_mon_prompt:
    pea _txt_prompt             ; Display prompt
    jsl sys_puts
    plx                         ; Restore stack

    ldx #0                      ; Set up line index
    
_mon_next:  
    jsl sys_getc                   ; Get a charcter (blocking)
    cmp #KEY_CTRL_C             ; ^C?
    beq monitor                 ; Yes, reset line
    cmp #KEY_LF                 ; Enter? (LF)
    beq _mon_exec               ; Yes, run the command
    cmp #KEY_CR                 ; Enter? (CR)
    beq _mon_exec               ; Yes, run the command
    cmp #KEY_BS                 ; Backspace?
    beq _mon_backspace          ; Yes
    cmp #KEY_DEL                ; DEL (modern backspace key)?
    beq _mon_backspace          ; Yes
    ;; Otherwise, try to add char to buffer
    
    jsl sys_putc
    sta buf,x                   ; Buffer starts at address 0 in bank 1
    inx
    cpx #$7fff                  ; If not in out of 32K mem, just keep going
    bcc _mon_next               ; Also, do buffer size -1 so that it can be null-terminated
    
    dex                         ; Maximum X value
    jmp _mon_next

_mon_backspace:
    phx
    pea _txt_backspace          ; Delete char
    jsl sys_puts
    plx                         ; Restore stack
    plx
    dex                         ; Back up index
    bpl _mon_next               ; If at beginning of line, we're done
    lda #' '                    ; otherwise, restore deleted char
    jsl sys_putc
    lda #KEY_BELL               ; Bell
    jsl sys_putc
    inx                         ; and reset the line index
    jmp _mon_next

    ;; Run a command in the buffer
_mon_exec:
    cpx #0                      ; Ignore lines with no content
    beq _mon_prompt
    stx xsav                    ; Save X for later comparison
    
    lda #0                      ; Null terminate input (used for argument passing)
    sta buf,x

    ;; Print a newline
    pea _txt_eol
    jsl sys_puts
    plx                         ; Restore stack
    
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
    ;; Command table index (O(n) search)
    ldy #_cmd_table+2           ; Set up our data pointer to point at the
    sty cmd_tbl_ptr             ; size of the entry, not the start of the entry
_me_loop_init:
    ldx line_start              ; X is line index
    ldy #0
_me_loop:
    iny
    lda (cmd_tbl_ptr),y         ; Get char from command entry
    cmp buf,x                   ; Get char from line
    bne _me_eoc_gc
    inx
    tya
    cmp <(cmd_tbl_ptr)          ; End of command string?
    bne _me_loop                ; Nope, keep checking chars
    ;; If max string size, check for equality
_me_eoc_gc:
    cpx xsav                    ; If at end of entered command, setup stack
    beq _me_args
    lda buf,x                   ; Get char from line
    cmp #' '+1                  ; Check to see if it is whitespace
    bcc _me_args                ; Is a space or control char
    bcs _me_eoc_chk_nxt         ; Not whitespace, try another command
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
    stz wspc_skip               ; Whitespace encountered
    lda #0
    sta buf,x                   ; Tokenize inputs
_me_ad_next:
    inx
    cpx xsav                    ; End of line = done parsing args
    bcs _me_cmd_setup
    lda buf,x                   ; Get char from line
    cmp #' '+1
    bcc _me_args_delim          ; Is a space or control char
    ;; Not a space
    lda wspc_skip               ; Is this the first non-whitespace character?
    bne _me_ad_next             ; Nope. Ignore the char
    lda #1                      ; Yes, mark it
    sta wspc_skip
    ;; First char in token, put it's pointer onto the stack
    .al
    rep #$20
    txa
    sta (arg_sp)
    lda arg_sp                  ; Post-inc the SP
    clc
    adc #$0002
    sta arg_sp
    cmp #arg_stack + {100 * 2}  ; 100 ARGS MAX! (Allocated stack size limitation)
    .as
    sep #$20
    bcc _me_ad_next             ; Not hit arg limit, keep going
    
_me_cmd_setup:
    ;; Set up the stack with args pointers
    .al
    rep #$20
    lda arg_sp
    sec
    sbc #arg_stack
    tax
_me_cs_loop:
    txy                         ; Test if empty stack
    beq _me_run_cmd             ; Arg pseudo stack is empty => we're done here
    dex                         ; Pre-dec the SP
    dex
    lda |arg_stack,x            ; Get the arg pointer (force abs addressing)
    pha                         ; and put it onto the call stack
    bra _me_cs_loop
    
_me_run_cmd:
    tsx                         ; Put a pointer to the stack itself onto the stack
    inx
    phx
    .as
    sep #$20
    lda arg_sp
    lsr                         ; Divide stack pointer by two
    pha                         ; Store the number of arguments to the command as the last entry on the stack
    .al
    rep #$20
    lda cmd_tbl_ptr
    sec
    sbc #_cmd_table + 2         ; Get index to base of table
    tax                         ; X now has the base address of the entry (which is a pointer)
    ldy xsav                    ; Y now contains the line length
    jmp (_cmd_table,x)          ; Execute command

    .as
_me_eoc_chk_nxt:
    lda #0
    xba
    lda (cmd_tbl_ptr)
    .al
    rep #$21                    ; Also clear carry
    ;; Increment index to next command entry
    adc cmd_tbl_ptr
    clc
    adc #2                      ; Skip the jump address
    sta cmd_tbl_ptr
    cmp #_cmd_table_end         ; Get the end of the table
    .as
    sep #$20
    bcs _me_cmd_invalid
    jmp _me_loop_init           ; If less than the ending address, keep searching

_me_cmd_invalid:
    ;; Maybe it's a low-level monitor command
;;; HEX Monitor
_xmon:   
    .as
    .xl
    sep #$20
    rep #$10
    
    ; Line terminated, now parse it
    ldx #-1                     ; Reset text index
    lda #0                      ; Default to XAM mode
    jmp _xmon_setmode

_xmon_exit:
    jmp monitor
    
_xmon_setblkxam:
    lda #$40                    ; Block XAM
_xmon_setmode:
    asl
    sta xmon_mode
_xmon_blskip:
    inx                         ; Advance text index
    cpx xsav                    ; Done with line?
    bcs _xmon_exit
_xmon_nextitem:
    lda >buf,x                  ; Get character
    cmp #'.'                    ; Delimiter is anything < '.'
    bcc _xmon_blskip            ; Skip it
    beq _xmon_setblkxam         ; If '.' then set to Block XAM mode
    cmp #':'                    ; If ':' set STOR mode
    beq _xmon_setmode
    cmp #'?'                    ; Help?
    beq _xmon_help
    ;; and #$5f                 ; Make uppercase
    stz xmon_l                  ; Clear hex value
    stz xmon_m
    stz xmon_h
    stx ysav                    ; Save x for later (used for index comparison)
    
    ;; Attempt to parse a hex value
_xmon_nexthex:
    lda >buf,x                  ; Get character
    eor #$30                    ; Maps digits to 0-9
    cmp #9+1                    ; Is this a decimal digit?
    bcc _xmon_dig               ; Yes
    and #$5f                    ; Ignore case
    adc #$a8                    ; Map to $FA-$FF
    cmp #$fa                    ; Is this a hex character?
    bcc _xmon_nothex            ; No

_xmon_dig:
    asl                         ; Move hex digit to MSB of A
    asl                     
    asl
    asl
    
    ldy #$04                    ; Shift count (# of bits in a hex char)
_xmon_hexshift:
    asl                         ; Shift digit left, moving bit into Carry
    rol xmon_l                  ; Rotate into hex storage location
    rol xmon_m
    rol xmon_h
    dey                         ; Done 4 shifts?
    bne _xmon_hexshift          ; No, do another
    inx                         ; Advance text index
    bpl _xmon_nexthex           ; Next character
    jmp _xmon_exit              ; End of text buffer, quit

_xmon_bad_cmd:
    ;; Not a valid command, output an error
    phb
    phx
    lda #{buf >> 16} & $ff      ; Switch to input buffer data bank
    pha
    plb
_xmon_bc_term:                  ; Zero-terminate first token
    inx 
    lda |{buf & $ffff},x        ; Get char from line
    cmp #' '+1                  ; Check to see if it is whitespace
    bcs _xmon_bc_term           ; No, keep looping
    stz |{buf & $ffff},x
    
    jsl sys_puts
    ply
    plb
    pea _txt_unk_cmd            ; Text string
    jsl sys_puts
    plx                         ; Restore stack
    jmp monitor
    
_xmon_nothex:
    cpx ysav                    ; Was at least 1 hex char given?
    beq _xmon_bad_cmd           ; No, clear line and start over

    bit xmon_mode               ; Check what mode we are in
    bvc _xmon_notstor           ; B6: 0 = STOR, 1 = XAM

    ;; Fallthrough for STOR mode, store
    lda xmon_l                  ; Get LSB of hex data
    sta [xmon_stl]              ; Store data Long
    inc xmon_stl                ; Inc the store pointer
    bne _xmon_nextitem
    inc xmon_stm
    bne _xmon_nextitem
    inc xmon_sth
_xmon_tonextitem:
    jmp _xmon_nextitem

_xmon_help:
    jmp _help
    
_xmon_notstor:
    stx ysav
    bpl _xmon_xam               ; B7 = 0 for XAM, 1 for Block XAM
    lda xmon_l                  ; Ensure that block XAM ends with address xxxf
    ora #$0f
    sta xmon_l
    lda xmon_xaml               ; If the ending address is the same as the
    cmp xmon_l                  ; current XAM address, don't do anything
    lda xmon_xamm
    sbc xmon_m
    lda xmon_xamh
    sbc xmon_h
    beq _xmon_tnij
_xmon_xn:   
    jmp _xmon_nexprint_line
_xmon_tnij:
    jmp _xmon_tonextitem_jmp

    ;; Now in XAM mode
_xmon_xam:
    ;; Print the line index numbers
    ldx #7                      ; Print a few spaces
    lda #' '
_xmon_lin_indent:
    jsl sys_putc
    dex
    bne _xmon_lin_indent
    
    pea _txt_dim                ; Dim the addresses
    jsl sys_puts
    plx
    
    ldy #0
_xmon_lin_loop:
    lda #' '
    jsl sys_putc
    cpy #8                      ; Add spacer between 8th and 9th columns
    bne $+6
    jsl sys_putc
    tya
    jsl sys_puthex              ; Print the number
    iny
    cpy #16
    bcc _xmon_lin_loop

    pea _txt_reset              ; Restore normal text
    jsl sys_puts
    plx
    
    ;; Set up the memory data pointers
    ldx #$03                    ; Copy 3 bytes
_xmon_setadr:
    lda <xmon_l-1,x             ; Copy hex data into
    sta <xmon_stl-1,x           ; the STORE POINTER
    sta <xmon_xaml-1,x          ; and to the XAM POINTER
    dex
    bne _xmon_setadr            ; Copy all 3 bytes
    stz xmon_ascii_line+16      ; 16 = # of ASCII chars per line

    and #$f0                    ; Make rows always start with a multiple of 16
    sta <xmon_xaml,x
    lda xmon_l                  ; Ensure that (any) XAM ends with address xxxf
    ora #$0f
    sta xmon_l

    ldx #15                     ; Size of ascii buffer-1
    lda #' '
_xmon_zero_ascii:
    sta xmon_ascii_line,x       ; Clear ascii line
    dex
    bpl _xmon_zero_ascii

    pea _txt_eol                ; New line
    jsl sys_puts
    plx
    bra _xmon_nexprint_line

    ;; Print address and data from this address
_xmon_nxtprint:
    bne _xmon_prdata            ; != 0 => no address to print
    
    ;; Print ASCII text next to HEX
    lda #' '                    ; Delimiter
    jsl sys_putc
    jsl sys_putc
    pea xmon_ascii_line + direct_page   ; Print ASCII values
    jsl sys_puts
    plx

    pea _txt_eol                ; New line
    jsl sys_puts
    plx

    lda xmon_xaml               ; Check if there is more to print
    cmp xmon_l
    lda xmon_xamm
    sbc xmon_m
    lda xmon_xamh
    sbc xmon_h
    bcs _xmon_tonextitem_jmp    ; If we're past the end address,
                                ; return to parse the rest of the line 

_xmon_nexprint_line:
    jsl sys_getc_nb             ; Check for input characters
    bcc _xmon_nxtprintadr       ; None, keep going
    cmp #KEY_CTRL_C             ; Ctrl+C?
    bne _xmon_nxtprintadr       ; No.
    jmp _xmon_exit              ; Yes, break
_xmon_nxtprintadr:
    pea _txt_dim                ; Dim the addresses
    jsl sys_puts
    plx
    lda xmon_xamh               ; Output high byte of address
    jsl sys_puthex
    lda xmon_xamm               ; Middle byte
    jsl sys_puthex
    lda xmon_xaml               ; Low byte
    jsl sys_puthex
    lda #':'
    jsl sys_putc
    pea _txt_reset              ; Restore normal text
    jsl sys_puts
    plx

_xmon_prdata:
    lda #' '                    ; Delimiter
    jsl sys_putc
    lda [xmon_xaml]             ; Get data from mem
    pha                     
    jsl sys_puthex              ; Print hex value  
    lda #$00                    ; Clear B accumulator
    xba
    lda xmon_xaml               ; Get the current line byte index
    and #$0f
    tay                         ; For indexing into the ASCII string holding reg
    cmp #$07                    ; If this is the 8th byte printed, put a space in the middle
    bne _xmon_prdata_isprint    ; to make it easier to read large blocks
    lda #' '
    jsl sys_putc
_xmon_prdata_isprint:   
    pla
    cmp #' '                    ; Unprintable character? (A < ' ')
    bcc _xmon_prdata_ascii_d    ; Yes, print '.'
    cmp #$7f                    ; Unprintable character?
    bcc _xmon_prdata_ascii      ; Yes (A >= DEL(0x7f))
_xmon_prdata_ascii_d:
    lda #'.'
_xmon_prdata_ascii:
    tyx
    sta xmon_ascii_line,x       ; Store into string

_xmon_xamnext:
    inc xmon_xaml               ; Next address, update pointer
    bne _xmon_mod16chk
    inc xmon_xamm
    bne _xmon_mod16chk
    inc xmon_xamh

_xmon_mod16chk:
    lda xmon_xaml               ; If address MOD 16 == 0, start new line
    and #$0f
    jmp _xmon_nxtprint

_xmon_tonextitem_jmp:           ; Range extension
    stz xmon_mode               ; Switch to XAM mode
    ldx ysav                    ; Restore parse index
    jmp _xmon_nextitem

    
;;; ------------------------------------
;;;  COMMANDS
;;; ------------------------------------

    .al
    .xl
_help:
    .as
    sep #$20
    pea _txt_help
    jsl sys_puts
    jmp monitor

    
    .al
    .xl
_clear:
    .as
    sep #$20
    pea _txt_clr_scrn
    jsl sys_puts
    jmp monitor


;;; Print the system's memory map
    .al
    .xl
_memmap:
    .as
    sep #$20
    pea _txt_mem_map
    jsl sys_puts
    jmp monitor

    
;;; JML to a user-specified address
;;; JSL to a user-specified address
    .al
    .xl
_go:
    .as
    sep #$20
    lda #$80
    sta gs_mode
    bne _gs
_gosub:
    .as
    sep #$20
    stz gs_mode
_gs:    
    pla                         ; Pull argc off the stack
    sta gs_argc                 ; Save argc for later
    beq _gs_expd_arg            ; If argc == 0, error
    
    lda #{buf >> 16} & $ff      ; Get buffer's DBR
    pha                         ; Set program data bank to read off the input stack
    plb
    plx                         ; Remove the self-stack pointer
    ;; At this point, the top of the stack is the pointer to the first arg
    plx
    pha                         ; Add space for the return value
    phx
    jsl sys_parsehex            ; Convert the arg into an address
    plx                         ; Remove pointer from stack
    bcc _gs_nothex              ; Not a valid address, error
    tay
    sty gs_addr_l
    pla                         ; Get high byte of return value
    sta gs_addr_h
    ;; Now restore the stack with all the argc and whatnot...
    tsx                         ; Put a pointer to the stack itself onto the stack
    inx
    phx
    lda gs_argc                 ; argc - 1
    dea
    pha
    ;; Check the mode that we're in
    bit gs_mode
    bmi _gs_jml
    ;; Simulate a jsl [gs_addr_l]
    phk
    pea _gs_jml+2
_gs_jml:    
    jmp [gs_addr_l + direct_page]
    jmp monitor                 ; Used for the jsl case

_gs_expd_arg:
_gs_nothex:
    pea _txt_gs_arg
    jsl sys_puts
    jmp monitor

_txt_gs_arg:
    .byte "Expected hex value for address.\n",0
    
    
;;; Prints argument info (for debugging)
    .al
    .xl
_args:
    .as
    .xl
    sep #$20
    rep #$10
    pea _txt_args_arg_cnt
    jsl sys_puts
    plx
    pla                         ; Get argc off stack
    pha
    jsl sys_putdec
    
    pla
    asl                         ; Multiply count by 2
    sta args_end                ; and save for later
    stz args_end + 1
    ldy #0
    sty args_y
    
_args_argv_loop:
    lda #0                      ; Set data bank to 0
    pha
    plb

    ldy args_y
    cpy args_end                ; Check if we are out of args to read in
    beq _args_done

    pea _txt_args_arg           ; Print "arg: "
    jsl sys_puts
    plx

    .al
    rep #$20
    ldy args_y
    lda (1,s),y                 ; Get the pointer to the arg string
    pha                         ; ** The plx after the sys_puts below pulls this back off the stack
    iny                         ; and dec the "stack pointer"
    iny
    sty args_y

    .as
    sep #$20
    lda #{buf >> 16} & $ff      ; Get buffer's DBR
    pha                         ; Set program data bank to read off the input stack
    plb
    ;;  pha up a few lines pushed the string pointer onto the stack
    jsl sys_puts
    plx
    
    jmp _args_argv_loop
    
_args_done: 
    pea _txt_eol
    jsl sys_puts                ; Don't need to restore the stack before returning to the monitor
    jmp monitor

_txt_args_arg_cnt:
    .byte "argc: ",0
_txt_args_arg:
    .byte "\narg: ",0

    
;;; XMODEM transfer RECEIVE command
;;; Transferred data is stored in bank 2
XRECV_NAK_TIMEOUT   .def 3      ; In seconds
XRECV_BLK_TIMEOUT   .def 1      ; In seconds
XR_NT_LOAD          .def 50_000 ; In cycles
XR_NT_COUNT         .def {XRECV_NAK_TIMEOUT * SYS_CLK_HZ} / XR_NT_LOAD   
XR_BT_COUNT         .def {XRECV_BLK_TIMEOUT * SYS_CLK_HZ} / XR_NT_LOAD   

_xrecv:
    .as
    .xl
    sep #$20
    rep #$10
    pea _txt_xrecv_wait
    jsl sys_puts
    plx

_xr_wt_nak: 
    lda #XRECV_NAK_TIMEOUT      ; Set up NAK timeout counter
    sta xr_retry_num

_xr_wt_flush:   
    ;;  "PURGE" (flush) the input buffer
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcs _xr_wt_flush            ; Got char, keep reading to empty Rx buf
    ;; Now send the NAK and wait for SOH
    lda #KEY_NAK                ; Send NAK
    jsl sys_putc
_xr_wait_timeout:
    dec xr_retry_num
    bmi _xr_wt_nak              ; If we're out of timeouts, resend a NAK
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsr sys_getc_to             ; Timeout getc
    bcc _xr_wait_timeout
    cmp #KEY_CTRL_C             ; Cancel
    beq _xr_done
    cmp #KEY_SOH
    bne _xr_wt_nak              ; If character is invalid, resend NAK
    ;; Received start, init block number
    stz xr_blk                  ; Set up block number (actually starts at 1
                                ; but by having this as 0, the block incrementer
                                ; logic will inc the block the first time)
    ldy #xrecv_buf >> 8         ; Initialize data receive pointer
    sty xr_blkptr+1
    lda #xrecv_buf & $ff
    sta xr_blkptr
    jmp _xr_soh

_xr_nak:
    ;;  "PURGE" (flush) the input buffer
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcs _xr_nak                 ; Got char, keep reading to empty Rx buf
    lda #KEY_NAK                ; Send NAK now that everything has been read in
    jsl sys_putc
_xr_wait_for_soh:
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcc _xr_wait_for_soh        ; No char, timeout occured
    cmp #KEY_SOH
    beq _xr_soh                 ; Handle new block
    cmp #KEY_CTRL_C             ; Cancel
    beq _xr_done
    cmp #KEY_CTRL_R             ; Restart
    beq _xrecv
    cmp #KEY_EOT
    bne _xr_nak
    lda #KEY_ACK                ; ACK EOT
    jsl sys_putc
_xr_done:   
    jmp monitor                 ; DONE

_xr_can:       
    lda #KEY_CAN                ; Send CAN
    jsl sys_putc
    jmp monitor                 ; FAILED

_xr_soh:
    ;;  SOH was just received, get the block number
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcc _xr_nak                 ; No char, timeout occured
    cmp xr_blk
    bcc _xr_can                 ; Invalid block number
    stz xr_isnext               ; Say that this block is NOT a new number
    beq _xr_inv_blkno           ; If current no == received, don't inc the block number
    dea
    cmp xr_blk                  ; Is this the next block?
    bne _xr_can                 ; Invalid block number, CAN
    ;; Yes, this is the next block
    inc xr_isnext               ; Is a new block number = 1
    inc xr_blk
    bmi _xr_can                 ; If too long, cancel
_xr_inv_blkno:  
    ;; Get inverted block number
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcc _xr_nak                 ; No char, timeout occured
    eor #$ff
    cmp xr_blk
    bne _xr_can                 ; Invalid block number

    ;;  Now, we're getting data
    stz xr_chksum
    ldy #0
    cld
_xr_data_loop:
    ldx #XR_BT_COUNT            ; Initialize count timeout
    phy
    jsl sys_getc_to             ; Timeout getc
    ply
    bcc _xr_nak                 ; No char, timeout occured
    sta [xr_blkptr],y
    clc
    adc xr_chksum
    sta xr_chksum
    iny
    cpy #$80
    bcc _xr_data_loop           ; If we have not received 128 bytes, keep going
    
    ;; Now check checksum
    ldx #XR_BT_COUNT            ; Initialize count timeout
    jsl sys_getc_to             ; Timeout getc
    bcc _xr_nak_jmp             ; No char, timeout occured
    cmp xr_chksum
    bne _xr_nak_jmp             ; Checksum failed, NAK
    ;; Success! Update the 128-byte block pointer
    lda #KEY_ACK
    jsl sys_putc

    lda xr_isnext               ; Is this a repeat block?
    beq _xr_wfs_jmp             ; Yes, don't change block num
    .al                         ; Set block pointer to next region
    rep #$20
    lda #$80                    ; Block size
    clc
    adc xr_blkptr
    sta xr_blkptr
    .as
    sep #$20
    bcc _xr_wfs_jmp
    inc xr_blkptr+2
_xr_wfs_jmp:
    jmp _xr_wait_for_soh        ; Wait for next block or EOH
    
_xr_nak_jmp:                    ; Range extension
    jmp _xr_nak

_txt_xrecv_wait:
    .byte "Waiting\n",0


;;; Perform a block copy of data from RAM to EEPROM
;;; Perform a block copy of data
    .al
    .xl
_eeprom:
    .as
    sep #$20
    lda #$80
    sta copy_mode
    bne _cp
_copy:
    .as
    sep #$20
    stz copy_mode
_cp:
    .as
    .xl
    sep #$20
    rep #$10
    pla                         ; Pull argc off the stack
    cmp #3
    bne _copy_expd_arg          ; If argc != 3, error
    
    lda #{buf >> 16} & $ff      ; Get buffer's DBR
    pha                         ; Set program data bank to read off the input stack
    plb
    plx                         ; Remove the self-stack pointer
    ;; At this point, the top of the stack is the pointer to the first arg
    ldx #0
_copy_args: 
    stx copy_xy
    plx                         ; Get arg string pointer
    pha                         ; Add space for the return value
    phx
    jsl sys_parsehex            ; Convert the arg into an address
    plx                         ; Remove pointer from stack
    bcc _copy_nothex            ; Not a valid address, error
    ldx copy_xy
    tay
    sty copy_s_l,x
    pla                         ; Get high byte of return value
    sta copy_s_l+2,x
    inx
    inx
    inx
    cpx #$9                     ; Parsed 3 args yet? (3 * (3 bytes each) == 9)
    bcc _copy_args

    ;; Do some sanity checks on the user's input data
    ldx copy_c_l                ; Byte count
    beq _copy_size              ; Can't copy 0 bytes
    dex
    bmi _copy_size              ; Too large
    
    bit copy_mode               ; If eeprom write, do some special stuff
    bmi _cp_eep
    
    jsl sys_memcpy_init         ; Set up SMC

    ;; More sanity checks
    .al
    rep #$20
    ldy copy_d_l                ; Destination address
    cpy #ROM_BASE
    bcs _copy_noeep             ; Can't copy to EEPROM

    ;; Now actually use that data
    ldx copy_s_l                ; Source address
    pei (copy_c_l)              ; Byte count
    .as
    sep #$20
    lda copy_d_h                ; Get destination bank
    xba
    lda copy_s_h                ; Source bank
    jsl sys_memcpy
    jmp monitor

_copy_expd_arg:                 ; Expected an argument
    pea _txt_copy_help
    jsl sys_puts
    jmp monitor

_copy_nothex:                   ; Expected a hex val
    pea 0                       ; These are not the same size, but then again, 
    plb                         ; since the monitor resets the stack, it doesn't
                                ; matter (and it makes things faster/smaller too)
    pea _txt_copy_nx
    jsl sys_puts
    jmp monitor
    
_copy_noeep:                    ; Can't copy TO eeprom
    pea 0
    plb
    pea _txt_copy_ne
    jsl sys_puts
    jmp monitor
    
_copy_size:                     ; Copy size out of range
    pea 0
    plb
    pea _txt_copy_size
    jsl sys_puts
    jmp monitor

_cp_eep:
    ;; Do some sanity checks on the user's input data
    ldy copy_d_l                ; Destination address
    cpy #ROM_BASE
    bcc _copy_eep               ; Must copy to EEPROM
    ldx copy_s_l                ; Source address
    cpx #ROM_BASE
    bcs _copy_feep              ; Can't copy from EEPROM

    ;; Move the EEPROM write routine into memory
    .al
    .xl
    rep #$30
    ldx #_smc_cp_eep            ; From addr
    ldy #_smc_cp                ; To addr
    lda #_smc_cp_eep_eos - _smc_cp_eep - 1
    mvn 0,0
    jsr _smc_cp
    jmp monitor

_copy_eep:                      ; Requires destination to be EEPROM
    pea 0
    plb
    pea _txt_copy_eep
    jsl sys_puts
    jmp monitor

_copy_feep:                     ; Can't copy from EEPROM to EEPROM
    pea 0
    plb
    pea _txt_copy_feep
    jsl sys_puts
    jmp monitor

_txt_copy_help:
    .byte "Usage: copy ssssss dddddd cccc\n",0
_txt_copy_nx:
    .byte "Arguments must be valid hex addresses.\n",0
_txt_copy_ne:
    .byte "Cannot `copy` to EEPROM. (Addr range $C000-$FFFF)\n",0
_txt_copy_eep:
    .byte "Destination must be EEPROM. (Addr range $C000-$FFFF)\n",0
_txt_copy_feep:
    .byte "Cannot `ecopy` from EEPROM to EEPROM. (Addr range $C000-$FFFF)\n",0
_txt_copy_size:
    .byte "Valid e/copy count: [1..$8000]\n",0

    
_smc_cp         .equ smc_base1

_smc_cp_eep:                    ; This is copied to RAM
    .as
    sep #$20
    lda copy_d_l                ; Get low byte of dest address
    and #$c0                    ; EEPROM page size = 64 bytes
    xba
    lda copy_d_l
    xba
    sta copy_d_l                ; Pointer now has lowest 6 bits reset
    xba
    .al
    rep #$20
    and #$3f                    ; EEPROM page size = 64 bytes
    tay
    sty copy_xy                 ; Need to save the page number for later
    lda #$40
    sec
    sbc copy_xy                 ; Get reminaing byte count for this page
    tax
    dey
    .as
    sep #$20
    bra _smc_cp_loop

_smc_cp_nxtpg:  
    ldx #$40                    ; EEPROM page size = 64 bytes
_smc_cp_loop:
    iny
    lda [copy_s_l]
    sta [copy_d_l],y
    .al
    rep #$20
    dec copy_c_l
    beq _smc_cp_write           ; All bytes have been copied
    inc copy_s_l                ; Inc the src pointer
    .as
    sep #$20
    bne $+4                     ; If need to carry to bank, do so
    inc <copy_s_h
    dex
    bne _smc_cp_loop
    ;; End of page
_smc_cp_write:
    .as
    sep #$20
    ;; Here's how we check for the EEPROM write status.
    ;; Conveniently, if a read is performed during the internal write op,
    ;; the complement of the last bit 7 is presented. We can then xor
    ;; that value with last value written. If the result is negative, then
    ;; bit 7 of the written value and the read value are complements of
    ;; eachother. In this case, we check again. Once the result of the xor
    ;; has bit 7 clear (positive number), then we know the EEPROM is done.
    sta |_smc_cp_eor + 1 - _smc_cp_eep + _smc_cp
_smc_cp_wait:                   ; Wait for EEPROM to complete write op
    lda [copy_d_l],y
_smc_cp_eor:    
    eor #0                      ; Operand to be replaced
    bmi _smc_cp_wait            ; Check bit 7 of the read data - if different, loop again
    ldx copy_c_l                ; Get remaining byte count
    dex
    bpl _smc_cp_nxtpg           ; Do next page
    rts
_smc_cp_eep_eos:                ; END OF SUB - keep for SMC init


;;; ------------------------------------
;;;  UTILITY FUNCTIONS
;;; ------------------------------------
_err:                           ; OOH, so useful!
    rep #$30                    ; "Flash" the MX pin
    sep #$30
    jmp _err

        
;;; ------------------------------------
;;;  SYSCALL FUNCTIONS
;;; ------------------------------------

    
;;; Get a single character from UART0, blocking
;;; Requirements:
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A
;;; Return:
;;;   A contains the received character
    .as
sys_getc:  
    lda >UART0_LSR              ; Check for Rx data (4)
    and #$01                    ; (2)
    beq sys_getc                ; No Rx data, keep checking
    lda >UART0_RHR              ; Yes, Get byte from Rx FIFO (4)
    rtl
    
    
;;; Get a single character from UART0, nonblocking
;;; Requirements:
;;;   .as
;;; Args:
;;;   NONE
;;; Uses:
;;;   A
;;; Return:
;;;   A contains the received character, if available
;;;   p.c = 1 if chararcter was available, 0 otherwise
    .as
sys_getc_nb:
    lda >UART0_LSR              ; Check for Rx data (4)
    and #$01                    ; (2)
    beq _getc_nb_cc             ; No Rx data, signal as such
    sec                         ; Got character -> set carry
    lda >UART0_RHR              ; Yes, Get byte from Rx FIFO (4)
    rtl
_getc_nb_cc:  
    clc                         ; No character -> clear carry
    rtl


;;; Get char with timeout
;;; Requirements:
;;;   .as
;;; Args:
;;;   X - number of GETC_LOAD cycles to wait (within some tolerance)
;;; Uses:
;;;   A, X, Y
;;; Return:
;;;   A contains the received character, if available
;;;   p.c = 1 if chararcter was available, 0 otherwise
;;;
    .as
sys_getc_to:    
_gct_wait:
    jsl sys_getc_nb             ; Nonblocking get char
    bcc _gct_delay              ; No character, continue timeout countdown
_gct_done:  
    rtl                         ; Got char, return
_gct_delay:
    dex
    beq _gct_done
    phx
    ldy #GETC_LOAD
    jsl sys_delay
    plx
    jmp _gct_wait

    
;;; Delay for some amount of time
;;; Requirements:
;;;   NONE
;;; Args:
;;;   Y - Y * ~1000 cycles to wait (DESTRUCTIVE)
;;;       NOTE: a value of 0 is maximum delay, 1 is minimum
;;; Uses:
;;;   X, Y
;;; Return:
;;;   NONE
sys_delay:
    php
    .xl
    rep #$10
_d_init:   
    ldx #248
_d_loop:
    dex
    bne _d_loop
    dey
    bne _d_init
    plp
    rtl


;;; Memory Copy Initialization
;;; Desc:
;;;   Since the memcpy subroutine makes use of self-modifying code,
;;;   the first time that memcpy is called, it must be loaded into
;;;   RAM. (This must also happen if other SMC is called after the
;;;   init process).
;;; Requirements:
;;;   NONE
;;; Args:
;;;   NONE
;;; Uses:
;;;   C, X, Y, DBR
;;; Return:
;;;   NONE
sys_memcpy_init:
    php
    phb
    .al
    .xl
    rep #$30
    ldx #_smc_memcpy            ; From addr
    ldy #sys_memcpy             ; To addr
    lda #_smc_mc_eos - _smc_memcpy - 1
    mvn 0,0
    plb
    plp
    rtl

    
;;; Block copy memory (MVN)
;;; Requirements:
;;;   .as
;;;   .xl (not strictly necessary, but why would you call with .xs?)
;;;   _memcpy_init must have been called before _memcpy is called.
;;;       If not called prior, this function's behavior is UNDEFINED!
;;;   The source and destination regions must not overlap unless:
;;;       (1) you know what you are doing, or
;;;       (2) you like UNDEFINED behavior (it's actually defined but
;;;           very likely not what you want)
;;; Args:
;;;   A - Source bank (8)
;;;   B - Destination bank (8)
;;;   X - Source bank address (16)
;;;   Y - Destination bank address (16)
;;;   (S) - Number of bytes to copy (16)
;;; Uses:
;;;   A, B, X, Y
;;; Return:
;;;   NONE
sys_memcpy     .equ smc_base0
    .as
    .xl
_smc_memcpy:
    sta |_smc_mc_mvn + 2 - _smc_memcpy + sys_memcpy ; Source bank (self-modifying code!)
    xba
    sta |_smc_mc_mvn + 1 - _smc_memcpy + sys_memcpy ; Destination bank
    .al
    rep #$20
    lda 4,s                     ; Get byte count
    dea                         ; MVN uses C+1 as the byte transfer count
    phb                         ; Save data bank
_smc_mc_mvn:
    mvn 0,0                     ; Banks modified sta's above
    .as
    sep #$20
    plb
    rtl
_smc_mc_eos:                    ; END OF SUB - keep for _memcpy_init


;;; Print a word (in C) as HEX to UART0
;;; Requirements:
;;;   NONE
;;; Args:
;;;   C - The word to print (DESTRUCTIVE)
;;; Uses:
;;;   X
;;; Return:
;;;   .as - C will be short!
sys_puthex_word:
    .as
    sep #$20
    xba
    jsl sys_puthex
    xba
    ;; *********** FALLTHROUGH ***********

    
;;; Print a byte (in A) as HEX to UART0
;;; Requirements:
;;;   .as
;;; Args:
;;;   A - The byte to print (DESTRUCTIVE)
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
    .as
sys_puthex:
    pha
    lsr
    lsr
    lsr
    lsr
    jsl _prhex
    pla

_prhex:                         ; Prints a single hex digit in LSN of A
    and #$0f                    ; Get LSD
    ora #'0'                    ; Add "0"
    cmp #'9'+1                  ; Is it a decimal digit?
    bcc sys_putc                ; Yes! output it.
    adc #$06                    ; No, convert it to ascii char for A-F
    ;; *********** FALLTHROUGH ***********

    
;;; Print a character to UART0 (Blocking)
;;; Requirements:
;;;   .as
;;; Args:
;;;   A - The character to print
;;; Uses:
;;;   NONE
;;; Return:
;;;   NONE
    .as
sys_putc:                       ; "echo" from older kernels
    pha
_putc_loop: 
    lda >UART0_LSR              ; Check for Tx empty
    and #$20
    beq _putc_loop              ; Not enough empty spaces in Tx FIFO
    pla
    sta >UART0_THR
    rtl    


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
sys_putdec:
sys_putdec_word:
    php
    php                         ; Save A's width
    .al
    rep #$20
    stz syscall.tmp0
    stz syscall.tmp2
    stz syscall.tmp4
    plp                         ; Restore A's width
    sta syscall.tmp4            ; Save binary value to be converted
    .al
    .xs
    rep #$20
    sep #$18                    ; Also set D flag
    stz syscall.tmp0            ; Clear temp var
    stz syscall.tmp1            ; 16-bit A, so should will bcd_tmp+2
    ldx #16                     ; Number of bits to do

_sys_putdec_cnvbit:
    asl syscall.tmp4            ; Shift out bit
    lda syscall.tmp0            ; Add into result
    adc syscall.tmp0
    sta syscall.tmp0
    .as
    sep #$20
    lda syscall.tmp2            ; Upper byte
    adc syscall.tmp2
    sta syscall.tmp2
    .al
    rep #$20
    dex                         ; Next bit
    bne _putdec_cnvbit
    
    lda syscall.tmp1            ; Upper 2 bytes of decimal number
    jsl sys_puthex_word
    .as
    sep #$20
    lda syscall.tmp0            ; this loads 8-bit value
    jsl sys_puthex
    plp                         ; Restore A's width
    rtl

    
;;; Echo a null-terminated string to the UART0
;;; Requirements:
;;;   .as
;;;   .xl
;;; Args:
;;;   (S) - string to print
;;; Uses:
;;;   A, Y
;;; Return:
;;;   NONE
    .as
    .xl
sys_puts:
    ldy #0
_sys_puts_loop:
	lda (4,s),y                 ; Get the first byte of the string at the strptr position
	beq _sys_puts_donstr	    ;  | If the value pulled is $00, we are done
    cmp #'\n'                   ; If newline, print a CR first
    bne _sys_puts_echo          ; Otherwise just print the character like normal
    lda #'\r'
    jsl sys_putc                ; Don't really care that this one is a sub call
    lda #'\n'                   ; since it only happens once per string and strings are
                                ; generally *long*
    
_sys_puts_echo:                 ; This is a local copy of the sys_putc routine to help shave a
                                ; few cycles off each iteration
    pha	                        
_sys_psl_pc: 
    lda >UART0_LSR              ; Check for Tx empty
    and #$20
    beq _sys_psl_pc             ; Not enough empty spaces in Tx FIFO
    pla
    sta >UART0_THR
	iny     	                ; Increment string character pointer
	bne _sys_puts_loop          ; Loop if we have not hit the limit of the y index
        ;; Fall through if y-index is exhausted
_sys_puts_donstr:
    rtl
    

;;; Parse a null-terminated string for a hex value
;;; Requirements:
;;;   .as
;;;   .xl
;;; Args: [top to bottom]
;;;   (S) - string to print
;;;   S+2 - Parsed value high byte [return]
;;; Uses:
;;;   A, X, Y
;;; Return:
;;;   C   - Holds 16-bit hex value.
;;;   S+2 - Holds bits 23..16 of the parsed value
;;;   p.c - 1 if at least one hex char was given, 0 otherwise
    .as
    .xl
sys_parsehex:
    ldy #0
    stz s_px_l
    sty s_px_m
_sys_ph_loop:    
    clc
    lda (4,s),y                 ; Get the first byte of the string at the strptr position
    beq _sys_ph_done            ;  | If the value pulled is $00, we are done
    eor #$30                    ; Maps digits to 0-9
    cmp #9+1                    ; Is this a decimal digit?
    bcc _sys_ph_dig             ; Yes
    and #$5f                    ; Ignore case
    adc #$a8                    ; Map to $FA-$FF
    cmp #$fa                    ; Is this a hex character?
    bcc _sys_ph_done            ; No

_sys_ph_dig:
    asl                         ; Move hex digit to MSB of A
    asl
    asl
    asl

    ldx #$04                    ; Shift count (# of bits in a hex char)
_sys_ph_hexshift:
    asl                         ; Shift digit left, moving bit into Carry
    rol s_px_l                  ; Rotate into hex storage location
    rol s_px_m
    rol s_px_h
    dex                         ; Done 4 shifts?
    bne _sys_ph_hexshift        ; No, do another
    iny                         ; Advance text index
    bra _sys_ph_loop            ; Next character

_sys_ph_done:
    bne _sys_ph_nov             ; If no invalid chars were given
    tyx                         ; ...
    beq _sys_ph_nov             ; and at least one character was given,
                                ; then set the carry flag:
    sec
    bra _sys_ph_lval
    
_sys_ph_nov:
    clc
_sys_ph_lval:   
    lda s_px_h
    sta 6,s                     ; S+2 high byte return
    lda s_px_m                  ; LSB and Middle Byte are in C
    xba
    lda s_px_l
    rtl


;;; ------------------------------------
;;;  SYSCALL TABLE
;;; ------------------------------------
;;; SYSCALLS can be accessed by loading
;;; X with the syscall index then executing
;;; a jsr (syscall_table,x) instruction.
;;; Alternatively, (a slightly less portable
;;; method is to) just jsl to the syscall
;;; subroutine name
syscall_table: 
    .word sys_delay
    .word sys_puthex
    .word sys_putdec
    .word sys_puthex_word
    .word sys_putdec_word
    .word sys_putc
    .word sys_puts
    .word sys_getc
    .word sys_getc_nb
    .word sys_getc_to
    .word sys_parsehex
    .word sys_memcpy_init
    .word sys_memcpy

    
;;; ------------------------------------
;;;  INTERRUPT HANDLERS
;;; ------------------------------------
vector_cop:
emu_vector_cop:
    jml >emu_vector_reset       ; RESTART

vector_brk:
    jml >emu_vector_reset       ; RESTART
    
emu_vector_nmi  .equ $0
vector_nmi      .equ $0
emu_vector_irq  .equ $80
vector_irq      .equ $80

vector_abort:
emu_vector_abt:
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
    
   

;;;
;;;  ZEDIAC MONITOR
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Syscall defines
;;; 
;;; Updates:
;;; 2023-06-03: Created file
;;; 

;;; KEEP IN SYNC WITH THE SYSCALL TABLE (_syscall_table)
;;; IN THE monitor.asm FILE!
SYS_DELAY   .equ 0
SYS_PH      .equ 2
SYS_PD      .equ 4
SYS_PHW     .equ 6
SYS_PDW     .equ 8
SYS_PUTS    .equ 10
SYS_GETC    .equ 12
SYS_PUTC    .equ 14

    

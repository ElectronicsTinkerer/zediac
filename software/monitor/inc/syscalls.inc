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
enum define SYS {
    DELAY = 0,
    PH = 2,
    PD = 4,
    PHW = 6,
    PDW = 8,
    PUTC = 10,
    PUTS = 12,
    GETC = 14,
    GETCNB = 16,
    GETCTO = 18,
    PARX = 20
}





;;;
;;;  ZEDIAC 16C2752 DUART DEFINES
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-05-16: Created file
;;;
;;;


UART_THR    .equ 0 // W
UART_RHR    .equ 0 // R
UART_IER    .equ 1 // R/W
UART_IIR    .equ 2 // R
UART_FCR    .equ 2 // W
UART_LCR    .equ 3 // R/W
UART_MCR    .equ 4 // R/W
UART_LSR    .equ 5 // R
UART_MSR    .equ 6 // R
UART_SCR    .equ 7 // R/W
UART_DLL    .equ 0 // R/W
UART_DREV   .equ 0 // R
UART_DLM    .equ 1 // R/W
UART_AFR    .equ 2 // R/W
UART_EFR    .equ 2 // R/W

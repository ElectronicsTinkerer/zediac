;;;
;;;  ZEDIAC HARDWARE DEFINES
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-04-28: Created file
;;; 2023-05-16: Added VIA0 register defs
;;; 2023-06-09: Update symbols to make use of new .def SLIME support
;;;
;;;

#include "../inc/65C22.inc"    
#include "../inc/16C2752.inc"    

;; #ifndef HW_INC
;; #define HW_INC

;;; ===============================
;;; ADDRESS LOCATIONS
;;; ===============================
RAM_BASE        .equ $0
VIA_BASE        .equ $8000    
DUART_BASE      .equ $8800
AUXCSB0_BASE    .equ $A000
AUXCSB1_BASE    .equ $A800
AUXCSB2_BASE    .equ $B000
AUXCSB3_BASE    .equ $B800
ROM_BASE        .equ $C000

;;; ===============================
;;; VARIOUS DEFS
;;; ===============================
ROM_SIZE        .def $4000

;;; ===============================
;;; HARDWARE REGISTERS
;;; ===============================
VIA0_ORB    .equ VIA_BASE + VIA_ORB
VIA0_IRB    .equ VIA_BASE + VIA_IRB
VIA0_ORA    .equ VIA_BASE + VIA_ORA
VIA0_IRA    .equ VIA_BASE + VIA_IRA
VIA0_DDRB   .equ VIA_BASE + VIA_DDRB
VIA0_DDRA   .equ VIA_BASE + VIA_DDRA
VIA0_T1CL   .equ VIA_BASE + VIA_T1CL
VIA0_T1CH   .equ VIA_BASE + VIA_T1CH
VIA0_T1LL   .equ VIA_BASE + VIA_T1LL
VIA0_T1LH   .equ VIA_BASE + VIA_T1LH
VIA0_T2CL   .equ VIA_BASE + VIA_T2CL
VIA0_T2CH   .equ VIA_BASE + VIA_T2CH
VIA0_SR     .equ VIA_BASE + VIA_SR 
VIA0_ACR    .equ VIA_BASE + VIA_ACR
VIA0_PCR    .equ VIA_BASE + VIA_PCR
VIA0_IFR    .equ VIA_BASE + VIA_IFR
VIA0_IER    .equ VIA_BASE + VIA_IER
VIA0_ORAF   .equ VIA_BASE + VIA_ORAF
VIA0_IRAF   .equ VIA_BASE + VIA_IRAF


UART0_THR   .equ DUART_BASE + UART_THR + 8 // W
UART0_RHR   .equ DUART_BASE + UART_RHR + 8 // R
UART0_IER   .equ DUART_BASE + UART_IER + 8 // R/W
UART0_IIR   .equ DUART_BASE + UART_IIR + 8 // R
UART0_FCR   .equ DUART_BASE + UART_FCR + 8 // W
UART0_LCR   .equ DUART_BASE + UART_LCR + 8 // R/W
UART0_MCR   .equ DUART_BASE + UART_MCR + 8 // R/W
UART0_LSR   .equ DUART_BASE + UART_LSR + 8 // R
UART0_MSR   .equ DUART_BASE + UART_MSR + 8 // R
UART0_SCR   .equ DUART_BASE + UART_SCR + 8 // R/W
UART0_DLL   .equ DUART_BASE + UART_DLL + 8 // R/W
UART0_DREV  .equ DUART_BASE + UART_DREV + 8 // R
UART0_DLM   .equ DUART_BASE + UART_DLM + 8 // R/W
UART0_AFR   .equ DUART_BASE + UART_AFR + 8 // R/W
UART0_EFR   .equ DUART_BASE + UART_EFR + 8 // R/W

UART1_THR   .equ DUART_BASE + UART_THR // W
UART1_RHR   .equ DUART_BASE + UART_RHR // R
UART1_IER   .equ DUART_BASE + UART_IER // R/W
UART1_IIR   .equ DUART_BASE + UART_IIR // R
UART1_FCR   .equ DUART_BASE + UART_FCR // W
UART1_LCR   .equ DUART_BASE + UART_LCR // R/W
UART1_MCR   .equ DUART_BASE + UART_MCR // R/W
UART1_LSR   .equ DUART_BASE + UART_LSR // R
UART1_MSR   .equ DUART_BASE + UART_MSR // R
UART1_SCR   .equ DUART_BASE + UART_SCR // R/W
UART1_DLL   .equ DUART_BASE + UART_DLL // R/W
UART1_DREV  .equ DUART_BASE + UART_DREV // R
UART1_DLM   .equ DUART_BASE + UART_DLM // R/W
UART1_AFR   .equ DUART_BASE + UART_AFR // R/W
UART1_EFR   .equ DUART_BASE + UART_EFR // R/W

;; endif



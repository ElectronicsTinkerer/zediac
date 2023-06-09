;;;
;;;  ZEDIAC 16C2752 DUART DEFINES
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-05-16: Created file
;;; 2023-06-09: Update symbols to make use of new .def SLIME support
;;;
;;;


UART_THR    .def 0 // W
UART_RHR    .def 0 // R
UART_IER    .def 1 // R/W
UART_IIR    .def 2 // R
UART_FCR    .def 2 // W
UART_LCR    .def 3 // R/W
UART_MCR    .def 4 // R/W
UART_LSR    .def 5 // R
UART_MSR    .def 6 // R
UART_SCR    .def 7 // R/W
UART_DLL    .def 0 // R/W
UART_DREV   .def 0 // R
UART_DLM    .def 1 // R/W
UART_AFR    .def 2 // R/W
UART_EFR    .def 2 // R/W

;;;
;;;  ZEDIAC 65C22 VIA DEFINES
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-05-16: Created file (Copied from EMMA-2 source)
;;; 2023-06-09: Update symbols to make use of new .def SLIME support
;;;
;;;


;; #ifndef VIA_INC
;; #define VIA_INC

VIA_ORB     .def 0
VIA_IRB     .def 0
VIA_ORA     .def 1
VIA_IRA     .def 1
VIA_DDRB    .def 2
VIA_DDRA    .def 3
VIA_T1CL    .def 4
VIA_T1CH    .def 5
VIA_T1LL    .def 6
VIA_T1LH    .def 7
VIA_T2CL    .def 8
VIA_T2CH    .def 9
VIA_SR      .def 10
VIA_ACR     .def 11
VIA_PCR     .def 12
VIA_IFR     .def 13
VIA_IER     .def 14
VIA_ORAF    .def 15
VIA_IRAF    .def 15

;; #endif

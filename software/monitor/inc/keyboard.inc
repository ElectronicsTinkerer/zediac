;;;
;;;  KEYBOARD (ASCII) VALUES
;;;
;;; (C) Ray Clemens 2023
;;;
;;; Updates:
;;; 2023-04-28: Created file
;;; 2023-06-03: Add CR/LF
;;; 2023-06-09: Update symbols to make use of new .def SLIME support
;;;

;; #ifndef KEYBOARD_INC
;; #define KEYBOARD_INC

KEY_SOH             .def $01
KEY_CTRL_B          .def $02
KEY_CTRL_C          .def $03
KEY_EOT             .def $04
KEY_ACK             .def $06
KEY_CTRL_F          .def $06
KEY_BELL            .def '\a'
KEY_BS              .def '\b'
KEY_LF              .def '\n'
KEY_CR              .def '\r'
KEY_CTRL_N          .def $0E
KEY_CTRL_P          .def $10
KEY_CTRL_R          .def $12
KEY_NAK             .def $15
KEY_CAN             .def $18
KEY_ESC             .def '\['   
KEY_DEL             .def 127

    
;; #endif


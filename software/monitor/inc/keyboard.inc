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

KEY_BELL            .def '\a'
KEY_BS              .def '\b'
KEY_LF              .def '\n'
KEY_CR              .def '\r'
KEY_ESC             .def '\['   
KEY_DEL             .def 127

    
;; #endif


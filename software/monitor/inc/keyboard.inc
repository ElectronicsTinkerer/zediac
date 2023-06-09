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

KEY_BS              .def $08
KEY_LF              .def $0a
KEY_CR              .def $0d
KEY_ESC             .def $1b    
    
;; #endif


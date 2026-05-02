;;; ============================================================================
;;; VeloxVM R7RS String Higher-Order Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small string-map and string-for-each, single-string variants
;;; (R7RS also accepts multiple strings; that's an extension a future
;;; revision can add).
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; string-map: apply proc to each character of str, collect results into
;; a new string. R7RS §6.7.
(define (string-map proc str)
  (list->string (map proc (string->list str))))

;; string-for-each: apply proc to each character of str for side effect.
;; Returns unspecified. R7RS §6.7.
(define (string-for-each proc str)
  (for-each proc (string->list str)))

;;; End of r7rs-strings.scm

;;; ============================================================================
;;; VeloxVM R7RS Error Object Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small error object procedures. The representation is a tagged
;;; vector: (vector *error-tag* message irritants). The error procedure
;;; constructs one and raises it; guard catches the raised value, and
;;; the predicate plus accessors lets the handler inspect it.
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; Tag used to discriminate error objects from other vectors. A symbol
;; is fine here: the predicate also checks vector length to keep stray
;; one-element vectors with this symbol from false-positiving.
(define *error-tag* 'error-object)

;; (error msg irritant ...) raises an error object containing the
;; message and a list of irritants.
(define (error msg . irritants)
  (raise (vector *error-tag* msg irritants)))

;; (error-object? obj) is true iff obj is an error object built by the
;; error procedure above.
(define (error-object? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) *error-tag*)))

;; (error-object-message obj) returns the message string passed to
;; error. Behaviour on a non-error-object is unspecified.
(define (error-object-message obj)
  (vector-ref obj 1))

;; (error-object-irritants obj) returns the list of irritants passed to
;; error. Behaviour on a non-error-object is unspecified.
(define (error-object-irritants obj)
  (vector-ref obj 2))

;;; End of r7rs-errors.scm

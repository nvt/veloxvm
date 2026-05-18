;;; ============================================================================
;;; VeloxVM R7RS Error Object Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small error object procedures. The representation is a tagged
;;; vector: (vector *error-tag* message irritants). The error procedure
;;; constructs one and raises it; guard catches the raised value, and
;;; the predicate plus accessors lets the handler inspect it.
;;;
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

;; R7RS §6.11 with-exception-handler. The handler is installed for the
;; dynamic extent of (thunk); on raise, control unwinds to the guard
;; clause and the handler is invoked with the raised object. The
;; handler's return value becomes the value of the
;; with-exception-handler form.
;;
;; Limitation vs. the R7RS spec: the spec mandates that raise produce
;; a secondary error if the handler returns (i.e. raise is
;; non-continuable). VeloxVM does not currently implement that
;; secondary-error behaviour; the handler's return value is simply
;; returned. In practice handlers either run to completion (the case
;; this implementation handles) or escape via an outer continuation
;; (not the targeted use case for this VM).
(define (with-exception-handler handler thunk)
  (guard (e (else (handler e)))
    (thunk)))

;; R7RS §6.11 raise-continuable. The full spec calls the handler in
;; the dynamic extent of the raise and threads the handler's return
;; value back to the raise-continuable call site; that requires a
;; per-thread handler stack the VM does not have. Aliased to raise so
;; user code that catches the raised object still works -- the
;; handler's return value flows out of the with-exception-handler
;; form rather than back into the raise-continuable site.
(define raise-continuable raise)

;;; End of r7rs-errors.scm

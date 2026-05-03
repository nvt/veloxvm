;;; VeloxVM Unit Tests - R7RS error objects
;;; Tests: error, error-object?, error-object-message,
;;;        error-object-irritants, integration with raise/guard.

(include "../unit-test-framework.scm")

;; Inline definitions from
;; languages/scheme-racket/runtime/r7rs-errors.scm
(define *error-tag* 'error-object)

(define (error msg . irritants)
  (raise (vector *error-tag* msg irritants)))

(define (error-object? obj)
  (and (vector? obj)
       (= (vector-length obj) 3)
       (eq? (vector-ref obj 0) *error-tag*)))

(define (error-object-message obj)
  (vector-ref obj 1))

(define (error-object-irritants obj)
  (vector-ref obj 2))

(test-suite "R7RS error objects")

;; Construct an error object via guard catching error
(define caught-no-irritants
  (guard (exc (else exc))
    (error "boom")))

(assert-equal #t (error-object? caught-no-irritants)
              "caught value is an error object")
(assert-equal "boom" (error-object-message caught-no-irritants)
              "message round-trips")
(assert-equal '() (error-object-irritants caught-no-irritants)
              "no irritants -> empty list")

;; With irritants
(define caught-with-irritants
  (guard (exc (else exc))
    (error "bad value" 42 'foo "extra")))

(assert-equal #t (error-object? caught-with-irritants)
              "with-irritants is an error object")
(assert-equal "bad value" (error-object-message caught-with-irritants)
              "message with irritants")
(assert-equal '(42 foo "extra")
              (error-object-irritants caught-with-irritants)
              "irritants captured in order")

;; Predicate type-isolation
(assert-equal #f (error-object? 42)
              "predicate rejects integer")
(assert-equal #f (error-object? "boom")
              "predicate rejects string")
(assert-equal #f (error-object? '(error-object "x" ()))
              "predicate rejects list")
(assert-equal #f (error-object? (vector 1 2 3))
              "predicate rejects regular vector")
(assert-equal #f (error-object? (vector *error-tag*))
              "predicate rejects 1-element vector with the tag")
(assert-equal #f (error-object? (vector *error-tag* "msg"))
              "predicate rejects 2-element vector with the tag")

;; raise on a non-error-object value still works as before;
;; error-object? rejects whatever else gets caught.
(define caught-symbol
  (guard (exc (else exc))
    (raise 'plain-symbol)))

(assert-equal 'plain-symbol caught-symbol
              "raise of plain symbol round-trips")
(assert-equal #f (error-object? caught-symbol)
              "raised symbol is not an error object")

;; Nested guards: inner catches first
(define inner-result
  (guard (outer (else (list 'outer outer)))
    (guard (inner (else (list 'inner inner)))
      (error "inner error"))))

(assert-equal 'inner (car inner-result)
              "inner guard caught")
(assert-equal #t (error-object? (cadr inner-result))
              "caught object passed through correctly")

(test-summary)

;;; VeloxVM Unit Tests - Runtime-library prelude auto-prepend
;;; Confirms that procedures shipped under runtime/r7rs-*.scm are
;;; reachable from a user program WITHOUT an explicit (include ...).
;;;
;;; The compiler auto-prepends a fixed list of runtime files (see
;;; languages/scheme/main.rkt:prelude-files) so these names
;;; resolve out of the box; the dead-define pass strips unused
;;; bindings, so a program that doesn't use the prelude pays no
;;; bytecode cost for it.

(include "../unit-test-framework.scm")

(test-suite "Runtime-library prelude")

;; --- r7rs-numeric.scm ---------------------------------------------------
(assert-equal 25 (square 5) "square from prelude")
(assert-equal #t (exact-integer? 7) "exact-integer? from prelude")
(assert-equal #f (exact-integer? 1/2) "exact-integer? rejects rational")
(assert-equal 3 (truncate-quotient 10 3) "truncate-quotient from prelude")
(assert-equal -4 (floor-quotient 10 -3) "floor-quotient from prelude")

;; --- r7rs-strings.scm ---------------------------------------------------
(assert-equal "ABC"
              (string-map char-upcase "abc")
              "string-map from prelude")

;; --- r7rs-features.scm --------------------------------------------------
(assert-equal #t (list? (features)) "features returns a list")

;; --- r7rs-bytevectors.scm -----------------------------------------------
(define bv (make-bytevector 4 0))
(bytevector-u8-set! bv 1 200)
(assert-equal 0   (bytevector-u8-ref bv 0) "bytevector-u8-ref default fill")
(assert-equal 200 (bytevector-u8-ref bv 1) "bytevector-u8-ref after set!")
(assert-equal 4   (bytevector-length bv)   "bytevector-length from prelude")

;; --- r7rs-parameters.scm ------------------------------------------------
(define p (make-parameter 10))
(assert-equal 10 (p)    "make-parameter reads initial value")
(p 20)
(assert-equal 20 (p)    "make-parameter writes new value")

;; --- r7rs-errors.scm ----------------------------------------------------
;; error raises, so we must trip it inside a guard to observe the object.
(define caught
  (guard (e (#t e))
    (error "boom" 1 2 3)))
(assert-equal #t (error-object? caught)
              "error-object? recognises error from prelude")
(assert-equal "boom" (error-object-message caught)
              "error-object-message from prelude")
(assert-equal '(1 2 3) (error-object-irritants caught)
              "error-object-irritants from prelude")

;; --- r5rs-io.scm --------------------------------------------------------
;; Only confirm the binding is reachable; the procedure itself needs a
;; filesystem port to exercise.
(assert-equal #t (procedure? call-with-input-file)
              "call-with-input-file reachable from prelude")
(assert-equal #t (procedure? call-with-output-file)
              "call-with-output-file reachable from prelude")

(test-summary)

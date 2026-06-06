;;; Library file loaded by name via the R7RS library search path.
;;; Imported as (mathx) by test-library-search.scm in the same directory.
(define-library (mathx)
  (import (scheme base))
  (export cube halve)
  (begin
    (define (cube x) (* x x x))
    (define (halve x) (quotient x 2))))

;;; VeloxVM Unit Tests - R7RS library macros
;;; A library's define-syntax names are mangled like its value
;;; definitions, so macros are isolated per library and participate in
;;; export / import sets, and a macro template's references to the
;;; library's own (possibly non-exported) bindings are rewritten
;;; consistently. See doc/r7rs-library-system-design.md.

(include "../unit-test-framework.scm")

;; Exported macro that expands to a use of a NON-exported helper.
(define-library (mac-a)
  (export quad)
  (begin
    (define (dbl x) (* x 2))
    (define-syntax quad (syntax-rules () ((_ e) (dbl (dbl e)))))))

;; Two libraries with identical macro and helper names: must stay isolated.
(define-library (mac-b)
  (export bump)
  (begin
    (define (step x) (+ x 1))
    (define-syntax bump (syntax-rules () ((_ e) (step e))))))

(define-library (mac-c)
  (export bump)
  (begin
    (define (step x) (+ x 10))
    (define-syntax bump (syntax-rules () ((_ e) (step e))))))

;; Macro that expands to a use of a library value binding.
(define-library (mac-d)
  (export with-base)
  (begin
    (define base 1000)
    (define-syntax with-base (syntax-rules () ((_ e) (+ base e))))))

(import (mac-a))
(import (rename (mac-b) (bump bump1)))
(import (prefix (mac-c) c:))
(import (mac-d))

(test-suite "R7RS library macros")

(assert-equal 40   (quad 10)     "exported macro over a non-exported helper")
(assert-equal 6    (bump1 5)     "macro imported via rename uses its own helper")
(assert-equal 15   (c:bump 5)    "macro imported via prefix; isolated from mac-b")
(assert-equal 1005 (with-base 5) "macro expands to use of a library binding")

(test-summary)

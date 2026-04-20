;; VeloxVM Unit Tests - R5RS Macro System (define-syntax, syntax-rules)
;; Tests for: R5RS §4.3 (Macros), hygiene, pattern matching, recursion

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Simple Macros")

;; when macro
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))

(define x 0)
(when (< x 10)
  (set! x 5))
(assert-equal 5 x "when macro executes body when test is true")

;; unless macro
(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
     (if (not test) (begin body ...)))))

(set! x 0)
(unless (> x 10)
  (set! x 7))
(assert-equal 7 x "unless macro executes body when test is false")

(test-suite "Pattern Matching")

;; Custom let implementation
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val)) body ...)
     ((lambda (var) body ...) val))))

(assert-equal 42 (my-let ((y 42)) y) "custom let macro binds variable")
(assert-equal 10 (my-let ((y 5)) (+ y 5)) "custom let macro evaluates body")

(test-suite "Recursive Macros")

;; Recursive and macro
(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)
    ((my-and test) test)
    ((my-and test1 test2 ...)
     (if test1 (my-and test2 ...) #f))))

(assert-equal #t (my-and) "my-and with no arguments returns #t")
(assert-equal #t (my-and #t) "my-and with single true returns #t")
(assert-equal #f (my-and #f) "my-and with single false returns #f")
(assert-equal #t (my-and #t #t #t) "my-and with all true returns #t")
(assert-equal #f (my-and #t #f #t) "my-and with one false returns #f")

;; Recursive or macro
(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or test) test)
    ((my-or test1 test2 ...)
     (if test1 #t (my-or test2 ...)))))

(assert-equal #f (my-or) "my-or with no arguments returns #f")
(assert-equal #t (my-or #t) "my-or with single true returns #t")
(assert-equal #f (my-or #f) "my-or with single false returns #f")
(assert-equal #t (my-or #f #t #f) "my-or with one true returns #t")
(assert-equal #f (my-or #f #f #f) "my-or with all false returns #f")

(test-suite "Ellipsis Patterns")

;; List construction with ellipsis
(define-syntax my-list
  (syntax-rules ()
    ((my-list elem ...)
     (list elem ...))))

(assert-equal '(1 2 3) (my-list 1 2 3) "my-list macro with ellipsis")

;; Variable swap macro
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((temp a))
       (set! a b)
       (set! b temp)))))

(define a 1)
(define b 2)
(swap! a b)
(assert-equal 2 a "swap! exchanges first variable")
(assert-equal 1 b "swap! exchanges second variable")

(test-suite "Macro Hygiene")

;; Hygienic macro that uses 'temp' internally
(define-syntax my-inc
  (syntax-rules ()
    ((my-inc x)
     (let ((temp x))
       (set! x (+ temp 1))))))

(define temp 5)
(define z 10)
(my-inc z)
(assert-equal 5 temp "hygiene preserves outer 'temp' variable")
(assert-equal 11 z "hygiene correctly increments variable")

(test-suite "Multiple Patterns with Auxiliary Keywords")

;; Custom cond implementation
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else result ...))
     (begin result ...))
    ((my-cond (test result ...))
     (if test (begin result ...)))
    ((my-cond (test result ...) clause ...)
     (if test
         (begin result ...)
         (my-cond clause ...)))))

(assert-equal 'first
              (my-cond (#t 'first)
                       (else 'second))
              "my-cond selects first true clause")
(assert-equal 'second
              (my-cond (#f 'first)
                       (#t 'second)
                       (else 'third))
              "my-cond skips false clauses")
(assert-equal 'else
              (my-cond (#f 'first)
                       (#f 'second)
                       (else 'else))
              "my-cond falls through to else")

(test-suite "Nested Macros")

;; while loop using when macro
(define-syntax while
  (syntax-rules ()
    ((while test body ...)
     (let loop ()
       (when test
         body ...
         (loop))))))

(define counter 0)
(while (< counter 5)
  (set! counter (+ counter 1)))
(assert-equal 5 counter "while loop executes until condition is false")

(test-suite "Complex Macros")

;; Custom let* implementation
(define-syntax my-let*
  (syntax-rules ()
    ((my-let* () body ...)
     (begin body ...))
    ((my-let* ((var1 val1) (var2 val2) ...) body ...)
     (let ((var1 val1))
       (my-let* ((var2 val2) ...) body ...)))))

(assert-equal 30
              (my-let* ((x 10)
                        (y (+ x 10))
                        (z (+ y 10)))
                z)
              "my-let* allows sequential binding")

(test-summary)

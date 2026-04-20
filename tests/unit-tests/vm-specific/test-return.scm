;; VeloxVM Unit Tests - Return Primitive
;; Tests for: return (early return from function)

;; Include test framework
(include "../unit-test-framework.scm")

(test-suite "Return Primitive")

;; Test early return
(define test-early-return
  (lambda (x)
    (if (< x 0)
        (return 999)
        #f)
    42))

(assert-equal 999 (test-early-return -5) "Early return from negative input")
(assert-equal 42 (test-early-return 5) "Normal return from positive input")
(assert-equal 999 (test-early-return -100) "Early return from large negative")
(assert-equal 42 (test-early-return 0) "Normal return from zero")

;; Test return as final expression
(define test-return-final
  (lambda ()
    (return 42)))

(assert-equal 42 (test-return-final) "Return as final expression")

;; Test multiple conditionals with return
(define test-multi-return
  (lambda (x)
    (if (< x 0)
        (return -1)
        #f)
    (if (= x 0)
        (return 0)
        #f)
    (if (> x 100)
        (return 1000)
        #f)
    x))

(assert-equal -1 (test-multi-return -10) "First early return condition")
(assert-equal 0 (test-multi-return 0) "Second early return condition")
(assert-equal 1000 (test-multi-return 500) "Third early return condition")
(assert-equal 50 (test-multi-return 50) "No early return, normal flow")

;; Test return with different value types
(define test-return-string
  (lambda (flag)
    (if flag
        (return "early")
        #f)
    "normal"))

(assert-equal "early" (test-return-string #t) "Early return with string")
(assert-equal "normal" (test-return-string #f) "Normal return with string")

(define test-return-list
  (lambda (flag)
    (if flag
        (return '(1 2 3))
        #f)
    '(4 5 6)))

(assert-equal '(1 2 3) (test-return-list #t) "Early return with list")
(assert-equal '(4 5 6) (test-return-list #f) "Normal return with list")

(test-summary)

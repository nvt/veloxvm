#lang racket

;; Test derived forms and rewrite rules

(require "../rewriter.rkt")

(displayln "Testing Derived Forms and Rewrite Rules\n")
(displayln "========================================\n")

;; Test cond
(displayln "Test 1: cond")
(define cond-test
  '(cond ((< x 0) "negative")
         ((= x 0) "zero")
         ((> x 0) "positive")))
(displayln (format "  Input:  ~a" cond-test))
(define cond-result (rewrite-expr cond-test))
(displayln (format "  Output: ~a\n" cond-result))

;; Test cond with else
(displayln "Test 2: cond with else")
(define cond-else-test
  '(cond ((< x 0) "negative")
         (else "non-negative")))
(displayln (format "  Input:  ~a" cond-else-test))
(define cond-else-result (rewrite-expr cond-else-test))
(displayln (format "  Output: ~a\n" cond-else-result))

;; Test case
(displayln "Test 3: case")
(define case-test
  '(case x
     ((1 2 3) "small")
     ((4 5) "medium")
     (else "large")))
(displayln (format "  Input:  ~a" case-test))
(define case-result (rewrite-expr case-test))
(displayln (format "  Output: ~a\n" case-result))

;; Test let
(displayln "Test 4: let")
(define let-test
  '(let ((x 1) (y 2))
     (+ x y)))
(displayln (format "  Input:  ~a" let-test))
(define let-result (rewrite-expr let-test))
(displayln (format "  Output: ~a\n" let-result))

;; Test let*
(displayln "Test 5: let*")
(define let*-test
  '(let* ((x 1) (y (+ x 1)))
     (* x y)))
(displayln (format "  Input:  ~a" let*-test))
(define let*-result (rewrite-expr let*-test))
(displayln (format "  Output: ~a\n" let*-result))

;; Test when
(displayln "Test 6: when")
(define when-test
  '(when (> x 0)
     (print "positive")
     (print x)))
(displayln (format "  Input:  ~a" when-test))
(define when-result (rewrite-expr when-test))
(displayln (format "  Output: ~a\n" when-result))

;; Test unless
(displayln "Test 7: unless")
(define unless-test
  '(unless (= x 0)
     (print "non-zero")))
(displayln (format "  Input:  ~a" unless-test))
(define unless-result (rewrite-expr unless-test))
(displayln (format "  Output: ~a\n" unless-result))

;; Test do
(displayln "Test 8: do")
(define do-test
  '(do ((i 0 (+ i 1)))
       ((>= i 10) i)
     (print i)))
(displayln (format "  Input:  ~a" do-test))
(define do-result (rewrite-expr do-test))
(displayln (format "  Output: ~a\n" do-result))

;; Test zero?
(displayln "Test 9: zero?")
(define zero-test '(zero? x))
(displayln (format "  Input:  ~a" zero-test))
(define zero-result (rewrite-expr zero-test))
(displayln (format "  Output: ~a\n" zero-result))

;; Test char-ci=?
(displayln "Test 10: char-ci=?")
(define char-ci-test '(char-ci=? #\A #\a))
(displayln (format "  Input:  ~a" char-ci-test))
(define char-ci-result (rewrite-expr char-ci-test))
(displayln (format "  Output: ~a\n" char-ci-result))

;; Test char-alphabetic?
(displayln "Test 11: char-alphabetic?")
(define char-alpha-test '(char-alphabetic? #\a))
(displayln (format "  Input:  ~a" char-alpha-test))
(define char-alpha-result (rewrite-expr char-alpha-test))
(displayln (format "  Output: ~a\n" char-alpha-result))

(displayln "========================================")
(displayln "All derived forms tests completed!")

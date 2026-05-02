;;; VeloxVM Unit Tests - Rest arguments (variadic lambdas)
;;; Tests for: (lambda args body), (lambda (a . rest) body),
;;;            (define (f . args) body), (define (f a . rest) body).
;;; The compiler emits bind_function_rest for these forms; the rest
;;; formal receives a freshly-allocated list of any extras.

(include "../unit-test-framework.scm")

(test-suite "Rest arguments")

;; (lambda args body): all actuals collected into args
(define collect-all (lambda args args))
(assert-equal '() (collect-all)
              "bare-symbol formal: no actuals -> empty list")
(assert-equal '(1) (collect-all 1)
              "bare-symbol formal: one actual")
(assert-equal '(1 2 3) (collect-all 1 2 3)
              "bare-symbol formal: many actuals")

;; (lambda (a . rest) body): one fixed + rest
(define head-rest (lambda (a . rest) (cons a rest)))
(assert-equal '(only) (head-rest 'only)
              "fixed + rest with no extras")
(assert-equal '(a b c) (head-rest 'a 'b 'c)
              "fixed + rest collects extras")

;; (define (f . args) body): function shorthand variant
(define (define-collect . xs) xs)
(assert-equal '() (define-collect)
              "define shorthand bare-rest with no actuals")
(assert-equal '(1 2 3) (define-collect 1 2 3)
              "define shorthand bare-rest with many actuals")

;; (define (f a b . rest) body): function shorthand with fixed + rest
(define (two-and-rest a b . r) (list a b r))
(assert-equal '(1 2 ()) (two-and-rest 1 2)
              "two fixed + empty rest")
(assert-equal '(1 2 (3)) (two-and-rest 1 2 3)
              "two fixed + one extra")
(assert-equal '(1 2 (3 4 5)) (two-and-rest 1 2 3 4 5)
              "two fixed + multiple extras")

;; rest formal is a real list: usable with car/cdr/length/null?
(define (count-args . xs) (length xs))
(assert-equal 0 (count-args) "rest list length: 0")
(assert-equal 4 (count-args 1 2 3 4) "rest list length: 4")

(define (first-rest . xs)
  (if (null? xs) #f (car xs)))
(assert-equal #f (first-rest) "rest list null? when empty")
(assert-equal 'first (first-rest 'first 'second) "rest list car")

;; Rest list type and contents are independent across calls
(define (snap . xs) xs)
(define snap1 (snap 'a))
(define snap2 (snap 'b))
(assert-equal '(a) snap1 "first call's rest list")
(assert-equal '(b) snap2 "second call's rest list")
(assert-equal #f (eq? snap1 snap2)
              "each call gets a fresh rest list")

(test-summary)

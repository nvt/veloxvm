#lang racket

;; VeloxVM Racket Compiler - Primitives Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../primitives.rkt")

;; Test type constants
(check-equal? VM-TYPE-BOOLEAN 0 "Boolean type")
(check-equal? VM-TYPE-INTEGER 1 "Integer type")
(check-equal? VM-TYPE-RATIONAL 2 "Rational type")
(check-equal? VM-TYPE-SYMBOL 5 "Symbol type")

;; Test primitive recognition
(check-true (vm-primitive? '+) "+ is primitive")
(check-true (vm-primitive? 'cons) "cons is primitive")
(check-true (vm-primitive? 'lambda) "lambda is primitive")
(check-false (vm-primitive? 'foo) "foo is not primitive")
(check-false (vm-primitive? 'unknown) "unknown is not primitive")

;; Test primitive count
(check-true (> (length vm-primitives) 100)
            "Has many primitives (>100)")

;; Test specific primitives exist
(define expected-prims
  '(+ - * / cons car cdr lambda if define
    quote begin and or apply map for-each))

(for ([prim expected-prims])
  (check-true (vm-primitive? prim)
              (format "~a is in primitives" prim)))

(displayln "All primitives tests passed!")

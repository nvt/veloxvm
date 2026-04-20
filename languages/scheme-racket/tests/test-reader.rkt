#lang racket

;; VeloxVM Racket Compiler - Reader Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../reader.rkt")

;; Test single expression reading
(check-equal? (read-expr "(+ 1 2)")
              '(+ 1 2)
              "Read simple expression")

(check-equal? (read-expr "(lambda (x) (+ x 1))")
              '(lambda (x) (+ x 1))
              "Read lambda expression")

(check-equal? (read-expr "'foo")
              '(quote foo)
              "Read quoted symbol")

;; Test quasiquotation parsing
(check-equal? (read-expr "`(a b c)")
              '(quasiquote (a b c))
              "Read quasiquote")

(check-equal? (read-expr "`(a ,b c)")
              '(quasiquote (a (unquote b) c))
              "Read quasiquote with unquote")

(check-equal? (read-expr "`(a ,@lst c)")
              '(quasiquote (a (unquote-splicing lst) c))
              "Read quasiquote with splice")

;; Test reading multiple expressions
(check-equal? (read-all-exprs "(+ 1 2) (* 3 4)")
              '((+ 1 2) (* 3 4))
              "Read multiple expressions")

(check-equal? (read-all-exprs "(define x 42)\n(+ x 1)")
              '((define x 42) (+ x 1))
              "Read multiple lines")

;; Test numbers
(check-equal? (read-expr "42")
              42
              "Read integer")

(check-equal? (read-expr "3.14")
              3.14
              "Read real")

(check-equal? (read-expr "1/2")
              1/2
              "Read rational")

;; Test strings
(check-equal? (read-expr "\"hello\"")
              "hello"
              "Read string")

;; Test characters
(check-equal? (read-expr "#\\a")
              #\a
              "Read character")

(check-equal? (read-expr "#\\newline")
              #\newline
              "Read named character")

;; Test booleans
(check-equal? (read-expr "#t")
              #t
              "Read true")

(check-equal? (read-expr "#f")
              #f
              "Read false")

(displayln "All reader tests passed!")

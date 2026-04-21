#lang racket

;; VeloxVM Racket Compiler - Rewriter Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../rewriter.rkt")

;; Test numerical predicates
(check-equal? (rewrite-expr '(positive? x))
              '(> x 0)
              "Rewrite positive?")

(check-equal? (rewrite-expr '(negative? x))
              '(< x 0)
              "Rewrite negative?")

(check-equal? (rewrite-expr '(odd? x))
              '(= (remainder x 2) 1)
              "Rewrite odd?")

(check-equal? (rewrite-expr '(even? x))
              '(= (remainder x 2) 0)
              "Rewrite even?")

;; Test abs
(check-equal? (rewrite-expr '(abs x))
              '(if (< x 0) (- x) x)
              "Rewrite abs")

;; Test max/min
(check-equal? (rewrite-expr '(max a b))
              '(if (> a b) a b)
              "Rewrite max")

(check-equal? (rewrite-expr '(min a b))
              '(if (< a b) a b)
              "Rewrite min")

;; Test I/O
(check-equal? (rewrite-expr '(newline))
              '(write-char #\newline)
              "Rewrite newline")

(check-equal? (rewrite-expr '(display "hello"))
              '(print "hello")
              "Rewrite display")

;; println expands through print + newline; newline is itself a rewriter
;; that expands to (write-char #\newline), and rewrite-expr applies
;; rewriters recursively, so the final form flattens to write-char.
(check-equal? (rewrite-expr '(println "hello" "world"))
              '(begin (print "hello" "world") (write-char #\newline))
              "Rewrite println")

;; Test composite car/cdr
(check-equal? (rewrite-expr '(caar x))
              '(car (car x))
              "Rewrite caar")

(check-equal? (rewrite-expr '(cadr x))
              '(car (cdr x))
              "Rewrite cadr")

(check-equal? (rewrite-expr '(caddr x))
              '(car (cdr (cdr x)))
              "Rewrite caddr")

;; Test character comparisons
(check-equal? (rewrite-expr '(char=? a b))
              '(= (char-compare a b) 0)
              "Rewrite char=?")

(check-equal? (rewrite-expr '(char<? a b))
              '(< (char-compare a b) 0)
              "Rewrite char<?")

;; Test string comparisons
(check-equal? (rewrite-expr '(string=? a b))
              '(= (string-compare a b) 0)
              "Rewrite string=?")

(check-equal? (rewrite-expr '(string<? a b))
              '(< (string-compare a b) 0)
              "Rewrite string<?")

;; Test nested rewriting
(check-equal? (rewrite-expr '(+ (abs x) (max a b)))
              '(+ (if (< x 0) (- x) x) (if (> a b) a b))
              "Rewrite nested expressions")

;; Test no rewriting for unknown forms
(check-equal? (rewrite-expr '(unknown-form a b))
              '(unknown-form a b)
              "Don't rewrite unknown forms")

(displayln "All rewriter tests passed!")

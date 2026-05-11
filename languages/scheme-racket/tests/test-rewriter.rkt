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

;; Test abs. The rewriter binds the argument to a temp so the
;; expression isn't duplicated; after the let rewriter runs, the
;; result is a single-arg lambda application.
(let ([result (rewrite-expr '(abs x))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,t1) (if (< ,t2 0) (- ,t3) ,t4)) x)
                   (and (eq? t1 t2) (eq? t1 t3) (eq? t1 t4))]
                  [_ #f]))
              result
              "abs binds the argument once and uses the temp three times"))

;; Test max/min. Both arguments bound first, then the if dispatches
;; over the temps.
(let ([result (rewrite-expr '(max a b))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,ta ,tb) (if (> ,ta1 ,tb1) ,ta2 ,tb2)) a b)
                   (and (eq? ta ta1) (eq? ta ta2)
                        (eq? tb tb1) (eq? tb tb2)
                        (not (eq? ta tb)))]
                  [_ #f]))
              result
              "max binds both args once each"))

(let ([result (rewrite-expr '(min a b))])
  (check-pred (lambda (r)
                (match r
                  [`((lambda (,ta ,tb) (if (< ,ta1 ,tb1) ,ta2 ,tb2)) a b)
                   (and (eq? ta ta1) (eq? ta ta2)
                        (eq? tb tb1) (eq? tb tb2)
                        (not (eq? ta tb)))]
                  [_ #f]))
              result
              "min binds both args once each"))

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

;; Test nested rewriting. abs and max each expand to a lambda
;; application around the arg-binding temp; check the shape rather
;; than exact symbol identity.
(let ([result (rewrite-expr '(+ (abs x) (max a b)))])
  (check-pred (lambda (r)
                (match r
                  [`(+ ((lambda (,_) (if (< ,_ 0) (- ,_) ,_)) x)
                       ((lambda (,_ ,_) (if (> ,_ ,_) ,_ ,_)) a b))
                   #t]
                  [_ #f]))
              result
              "nested abs/max each expand to a lambda app, leaving the outer + intact"))

;; Test no rewriting for unknown forms
(check-equal? (rewrite-expr '(unknown-form a b))
              '(unknown-form a b)
              "Don't rewrite unknown forms")

(displayln "All rewriter tests passed!")

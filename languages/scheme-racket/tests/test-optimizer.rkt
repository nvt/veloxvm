#lang racket

;; VeloxVM Racket Compiler - Optimizer Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../optimizer.rkt")

(parameterize ([optimization-level 1])

  ;; ============================================================================
  ;; Bottom-up / fixpoint folding (the case that motivated post-order)
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ (- 3 1) 4)) 6
                "Outer + folds after inner - is folded")

  (check-equal? (optimize-expr '(* (+ 1 2) (- 5 1))) 12
                "Both children fold, then outer *")

  (check-equal? (optimize-expr '(+ (+ (+ 1 2) 3) 4)) 10
                "Deeply nested binary + collapses end-to-end")

  (check-equal? (optimize-expr '(if (= 1 1) 'a 'b)) ''a
                "if folds after its test folds")

  (check-equal? (optimize-expr '(if (= 1 2) 'a 'b)) ''b
                "if false-branch via folded test")

  (check-equal? (optimize-expr '(not (= 1 1))) #f
                "Chained rule firing: (= 1 1) -> #t, then (not #t) -> #f")

  (check-equal? (optimize-expr '(* (+ 0 5) 1)) 5
                "Identity rules compose with arithmetic folding")

  ;; ============================================================================
  ;; Existing rules still work (regression coverage)
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ 2 3)) 5
                "Simple constant folding")

  (check-equal? (optimize-expr '(+ x 0)) 'x
                "(+ x 0) identity")

  (check-equal? (optimize-expr '(* x 0)) 0
                "(* x 0) zero")

  (check-equal? (optimize-expr '(if #t 'yes 'no)) ''yes
                "(if #t ...) literal folding")

  (check-equal? (optimize-expr '(begin)) #f
                "Empty begin")

  (check-equal? (optimize-expr '(begin 42)) 42
                "Single-form begin")

  ;; ============================================================================
  ;; Quote opacity (must not fold inside quoted data, R5RS)
  ;; ============================================================================

  (check-equal? (optimize-expr '(quote (+ 1 2))) '(quote (+ 1 2))
                "Do not fold inside quoted lists")

  (check-equal? (optimize-expr '(quote (if #t a b))) '(quote (if #t a b))
                "Do not fold special forms inside quote")

  ;; ============================================================================
  ;; Lambda and define recurse only into bodies
  ;; ============================================================================

  (check-equal? (optimize-expr '(lambda (x) (+ 1 2)))
                '(lambda (x) 3)
                "Lambda body is optimized")

  (check-equal? (optimize-expr '(lambda (x . rest) (+ 1 2)))
                '(lambda (x . rest) 3)
                "Variadic lambda formals preserved")

  (check-equal? (optimize-expr '(define (f x) (+ (- 3 1) x)))
                '(define (f x) (+ 2 x))
                "Define-shorthand body optimized; (+ 2 x) not foldable")

  ;; ============================================================================
  ;; Don't fold on non-numeric arguments
  ;; ============================================================================

  (check-equal? (optimize-expr '(+ x 3)) '(+ x 3)
                "Won't fold when one operand is a variable")

  (check-equal? (optimize-expr '(/ 10 0)) '(/ 10 0)
                "Don't fold division by zero literal")

  ;; ============================================================================
  ;; Optimization disabled
  ;; ============================================================================

  (parameterize ([enable-optimizations #f])
    (check-equal? (optimize-expr '(+ (- 3 1) 4)) '(+ (- 3 1) 4)
                  "No folding when optimizations disabled"))

  (parameterize ([optimization-level 0])
    (check-equal? (optimize-expr '(+ 2 3)) '(+ 2 3)
                  "No folding at level 0")))

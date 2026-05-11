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
                  "No folding at level 0"))

  ;; ============================================================================
  ;; Beta-reduce single-use let bindings
  ;; (let rewriter has already turned (let ((x v)) body) into
  ;;  ((lambda (x) body) v) by the time the optimizer sees it.)
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) (+ x 1)) 5)) 6
                "Numeric literal inlined, then (+ 5 1) folds")

  (check-equal? (optimize-expr '((lambda (x) (+ x x)) 5)) 10
                "Literal inlined at multiple use sites")

  (check-equal? (optimize-expr '((lambda (x) (g x x)) 7))
                '(g 7 7)
                "Literal inlined twice into a non-foldable call")

  (check-equal? (optimize-expr '((lambda (flag) (if flag 'yes 'no)) #t))
                ''yes
                "Boolean inline + if folding")

  (check-equal? (optimize-expr '((lambda (x y) (+ x y)) 5 10)) 15
                "Multi-binding lambda: both literals inlined, then folded")

  (check-equal? (optimize-expr '((lambda (x y) (+ x y)) 5 (g)))
                '((lambda (y) (+ 5 y)) (g))
                "Partial inline: literal x inlined, non-literal y kept")

  (check-equal? (optimize-expr '((lambda (x) (begin (set! x 6) x)) 5))
                '((lambda (x) (begin (set! x 6) x)) 5)
                "set! on parameter blocks inlining")

  (check-equal? (optimize-expr '((lambda (x) (g x)) (h)))
                '((lambda (x) (g x)) (h))
                "Non-literal value: not inlined (would change order)")

  (check-equal? (optimize-expr '((lambda (x) ((lambda (y) (+ x y)) 4)) 3)) 7
                "Nested lambda apps: cascade through both inlines and folds")

  (check-equal? (optimize-expr '((lambda (x) (lambda (x) x)) 5))
                '(lambda (x) x)
                "Inner lambda shadows: outer x inlined, inner x stays")

  ;; ============================================================================
  ;; Variadic and arity-mismatched lambdas are left alone
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda args (length args)) 1 2 3))
                '((lambda args (length args)) 1 2 3)
                "Bare-symbol formals not beta-reduced")

  (check-equal? (optimize-expr '((lambda (x . rest) x) 1 2 3))
                '((lambda (x . rest) x) 1 2 3)
                "Dotted formals not beta-reduced")

  ;; ============================================================================
  ;; Quote opacity in substitution
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) '(x x x)) 5))
                ''(x x x)
                "Don't substitute into quoted data")

  (check-equal? (optimize-expr '((lambda (x) (quote x)) 5))
                ''x
                "Quoted symbol stays even if name matches")

  ;; ============================================================================
  ;; Quoted-atom literals inline
  ;; ============================================================================

  (check-equal? (optimize-expr '((lambda (x) (eq? x 'a)) 'a))
                '(eq? 'a 'a)
                "Quoted symbol literal inlined")

  (check-equal? (optimize-expr '((lambda (x) (null? x)) '()))
                '(null? '())
                "Quoted empty list literal inlined"))

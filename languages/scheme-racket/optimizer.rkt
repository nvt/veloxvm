#lang racket

;; VeloxVM Racket Compiler - Optimizer
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Performs compile-time optimizations:
;; - Constant folding
;; - Dead code elimination
;; - Algebraic simplifications

(provide optimize-expr
         enable-optimizations
         optimization-level)

;; Optimization parameters
(define enable-optimizations (make-parameter #t))
(define optimization-level (make-parameter 1))  ; 0=none, 1=basic, 2=aggressive

;; ============================================================================
;; Main Optimization Entry Point
;; ============================================================================

(define (optimize-expr expr)
  (if (enable-optimizations)
      (optimize-recursive expr (optimization-level))
      expr))

(define (optimize-recursive expr level)
  (cond
    [(= level 0) expr]  ; No optimization
    [(= level 1) (optimize-basic expr)]
    [(>= level 2) (optimize-aggressive expr)]
    [else expr]))

;; ============================================================================
;; Basic Optimizations (Level 1)
;; ============================================================================

;; Post-order rewrite: optimize children first, then apply local rules at
;; this node and iterate until they stop firing. Bottom-up is required so
;; that a fold in a sub-expression feeds the outer rule:
;;   (+ (- 3 1) 4)
;;     children -> (+ 2 4)
;;     local rule -> 6
;; Top-down would never fold the outer (+) because (- 3 1) is still a pair
;; when the outer rule is tested. Local fixpoint also handles cases where
;; one rule's output enables another, e.g. (not (= 1 1)) -> (not #t) -> #t.
(define (optimize-basic expr)
  (cond
    ;; Atoms have nothing to fold.
    [(not (pair? expr)) expr]

    ;; Quote: R5RS forbids evaluating the datum, so we must not recurse
    ;; into it (and no rule rewrites a quote form).
    [(eq? (car expr) 'quote) expr]

    ;; lambda: the formal-parameter spec is metadata and may be a
    ;; dotted-pair list or bare symbol for variadic lambdas. Optimize the
    ;; body, leave the formals untouched.
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     `(lambda ,(cadr expr) ,@(map optimize-basic (cddr expr)))]

    ;; (define (name . formals) body...): same treatment as lambda.
    ;; (define name value) falls through to the general case.
    [(and (eq? (car expr) 'define) (pair? (cadr expr)))
     `(define ,(cadr expr) ,@(map optimize-basic (cddr expr)))]

    ;; General compound form: post-order recurse, then run local rules
    ;; to fixpoint at this node.
    [else
     (apply-basic-rules-fixpoint
      (cons (optimize-basic (car expr))
            (map optimize-basic (cdr expr))))]))

;; Apply local rules until they stop firing at this node. eq? is the
;; right termination check: rules that fire either build a fresh value
;; (e.g. (+ 1 2) -> 3) or return a sub-element (e.g. (+ e 0) -> e), and
;; in both cases the result is not eq? to the input pair. When no rule
;; matches, apply-basic-rules returns the input unchanged and eq?
;; succeeds.
(define (apply-basic-rules-fixpoint expr)
  (let ([next (apply-basic-rules expr)])
    (if (eq? next expr)
        expr
        (apply-basic-rules-fixpoint next))))

;; Local rules. Each operates on the current node only and assumes
;; children are already optimized -- so identity rules can return the
;; surviving sub-expression directly without re-running optimize-basic.
(define (apply-basic-rules expr)
  (match expr
    ;; Constant folding for arithmetic
    [`(+ ,n1 ,n2) #:when (and (number? n1) (number? n2)) (+ n1 n2)]
    [`(- ,n1 ,n2) #:when (and (number? n1) (number? n2)) (- n1 n2)]
    [`(* ,n1 ,n2) #:when (and (number? n1) (number? n2)) (* n1 n2)]
    [`(/ ,n1 ,n2) #:when (and (number? n1) (number? n2) (not (zero? n2)))
     (/ n1 n2)]

    ;; Constant folding for comparisons
    [`(= ,n1 ,n2) #:when (and (number? n1) (number? n2)) (= n1 n2)]
    [`(< ,n1 ,n2) #:when (and (number? n1) (number? n2)) (< n1 n2)]
    [`(> ,n1 ,n2) #:when (and (number? n1) (number? n2)) (> n1 n2)]
    [`(<= ,n1 ,n2) #:when (and (number? n1) (number? n2)) (<= n1 n2)]
    [`(>= ,n1 ,n2) #:when (and (number? n1) (number? n2)) (>= n1 n2)]

    ;; Constant folding for boolean operations
    [`(not #t) #f]
    [`(not #f) #t]
    [`(not ,b) #:when (boolean? b) (not b)]

    ;; Identity optimizations
    [`(+ ,e 0) e]
    [`(+ 0 ,e) e]
    [`(- ,e 0) e]
    [`(* ,e 1) e]
    [`(* 1 ,e) e]
    [`(* ,e 0) 0]
    [`(* 0 ,e) 0]

    ;; If optimizations
    [`(if #t ,conseq ,_) conseq]
    [`(if #f ,_ ,alt) alt]

    ;; Begin optimizations
    [`(begin) #f]
    [`(begin ,e) e]

    ;; No rule matched.
    [else expr]))

;; ============================================================================
;; Aggressive Optimizations (Level 2)
;; ============================================================================

(define (optimize-aggressive expr)
  (let ([basic (optimize-basic expr)])
    (match basic
      ;; Algebraic simplifications
      [`(+ (+ ,a ,b) ,c) #:when (and (number? b) (number? c))
       (optimize-aggressive `(+ ,a ,(+ b c)))]

      [`(* (* ,a ,b) ,c) #:when (and (number? b) (number? c))
       (optimize-aggressive `(* ,a ,(* b c)))]

      ;; Strength reduction
      [`(* ,expr 2) `(+ ,expr ,expr)]
      [`(* 2 ,expr) `(+ ,expr ,expr)]

      ;; Boolean short-circuits
      [`(and #f ,_) #f]
      [`(and #t ,expr) (optimize-aggressive expr)]
      [`(or #t ,_) #t]
      [`(or #f ,expr) (optimize-aggressive expr)]

      ;; Dead code in begin
      [`(begin ,exprs ...)
       (let ([optimized (map optimize-aggressive exprs)])
         (match optimized
           [(list) #f]
           [(list single) single]
           [else `(begin ,@optimized)]))]

      ;; Nested if simplification
      [`(if ,test (if ,test2 ,c1 ,a1) ,alt)
       #:when (equal? test test2)
       (optimize-aggressive `(if ,test ,c1 ,alt))]

      [else basic])))

;; ============================================================================
;; Dead Code Elimination
;; ============================================================================

(define (has-side-effects? expr)
  "Check if expression has side effects (must be evaluated)"
  (match expr
    ;; Forms with side effects
    [`(define . ,_) #t]
    [`(set! . ,_) #t]
    [`(print . ,_) #t]
    [`(display . ,_) #t]
    [`(write . ,_) #t]
    [`(write-char . ,_) #t]

    ;; Function calls might have side effects (conservative)
    [(cons _ _) #t]

    ;; Pure values
    [_ #f]))

(define (eliminate-dead-code exprs)
  "Remove expressions without side effects from a sequence"
  (filter has-side-effects? exprs))

;; ============================================================================
;; Optimization Statistics
;; ============================================================================

(define optimization-stats (make-hash))

(define (record-optimization type)
  (hash-update! optimization-stats type add1 0))

(define (get-optimization-stats)
  (hash-copy optimization-stats))

(define (reset-optimization-stats!)
  (set! optimization-stats (make-hash)))

;; ============================================================================
;; Utilities
;; ============================================================================

(define (optimize-all exprs)
  "Optimize a list of expressions"
  (map optimize-expr exprs))

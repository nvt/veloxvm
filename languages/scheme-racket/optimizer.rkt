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

(define (optimize-basic expr)
  (match expr
    ;; Constant folding for arithmetic
    [`(+ ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (+ n1 n2)]

    [`(- ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (- n1 n2)]

    [`(* ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (* n1 n2)]

    [`(/ ,n1 ,n2) #:when (and (number? n1) (number? n2) (not (zero? n2)))
     (/ n1 n2)]

    ;; Constant folding for comparisons
    [`(= ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (= n1 n2)]

    [`(< ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (< n1 n2)]

    [`(> ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (> n1 n2)]

    [`(<= ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (<= n1 n2)]

    [`(>= ,n1 ,n2) #:when (and (number? n1) (number? n2))
     (>= n1 n2)]

    ;; Constant folding for boolean operations
    [`(not #t) #f]
    [`(not #f) #t]
    [`(not ,b) #:when (boolean? b) (not b)]

    ;; Identity optimizations
    [`(+ ,expr 0) (optimize-basic expr)]
    [`(+ 0 ,expr) (optimize-basic expr)]
    [`(- ,expr 0) (optimize-basic expr)]
    [`(* ,expr 1) (optimize-basic expr)]
    [`(* 1 ,expr) (optimize-basic expr)]
    [`(* ,expr 0) 0]
    [`(* 0 ,expr) 0]

    ;; If optimizations
    [`(if #t ,conseq ,_) (optimize-basic conseq)]
    [`(if #f ,_ ,alt) (optimize-basic alt)]

    ;; Begin optimizations
    [`(begin) #f]  ; Empty begin
    [`(begin ,expr) (optimize-basic expr)]  ; Single expression

    ;; Quote: DO NOT optimize quoted expressions!
    ;; R5RS: quote suppresses ALL evaluation, so we must not perform
    ;; constant folding or any other optimizations on quoted data.
    [`(quote ,datum) expr]  ; Return as-is, don't recurse into datum

    ;; lambda: the formal-parameter spec is metadata, not code, and may
    ;; be a dotted-pair list or bare symbol for variadic lambdas. Walk
    ;; the body, leave the formals untouched.
    [(? (lambda (e)
          (and (pair? e) (eq? (car e) 'lambda) (>= (length e) 3))) expr)
     `(lambda ,(cadr expr)
        ,@(map optimize-basic (cddr expr)))]

    ;; (define (name . rest-or-formals) body...): function-shorthand
    ;; define carries the formal-parameter spec in (cadr expr), which
    ;; like lambda's may be improper. Skip it and walk the body only.
    [(? (lambda (e)
          (and (pair? e) (eq? (car e) 'define) (pair? (cadr e)))) expr)
     `(define ,(cadr expr)
        ,@(map optimize-basic (cddr expr)))]

    ;; Recursively optimize sub-expressions
    [(cons head tail)
     (cons (optimize-basic head) (map optimize-basic tail))]

    ;; Atoms and other expressions
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

#lang racket

;; VeloxVM Racket Compiler - Dead Define Elimination Tests
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require rackunit
         "../dead-define.rkt")

;; ============================================================================
;; Unreferenced define with a pure value is dropped entirely
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define dead-helper (lambda (x) (* x x)))
                  (define live 5)
                  (+ live live)))
              '((define live 5)
                (+ live live))
              "dead helper with lambda value (pure) is dropped")

(check-equal? (eliminate-dead-defines
                '((define unused 42)
                  (foo)))
              '((foo))
              "dead numeric constant is dropped")

;; ============================================================================
;; Unreferenced define with an impure value: keep the value, drop the binding
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define dead (display "side effect"))
                  (foo)))
              '((display "side effect")
                (foo))
              "dead binding with impure value: keep the value")

;; ============================================================================
;; Function shorthand whose name is referenced -- keep
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define (square x) (* x x))
                  (square 5)))
              '((define (square x) (* x x))
                (square 5))
              "referenced function shorthand is kept")

;; ============================================================================
;; Transitive deadness: helper used only by a dead define
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define helper (lambda (x) (+ x 1)))
                  (define wrapper (lambda (x) (helper x)))
                  ;; nothing references wrapper
                  ))
              '()
              "helper and wrapper both reaped via iteration to fixpoint")

;; ============================================================================
;; Lambda formal shadows the outer name: top-level x is genuinely unused
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define x 5)
                  (define (f x) (* x x))
                  (f 10)))
              '((define (f x) (* x x))
                (f 10))
              "top-level x not kept alive by f's parameter x (shadow check)")

;; ============================================================================
;; set! on a binding counts as a reference (the set! would otherwise fail)
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define counter 0)
                  (set! counter (+ counter 1))))
              '((define counter 0)
                (set! counter (+ counter 1)))
              "set!'d top-level binding is kept")

;; ============================================================================
;; Non-define top-level forms pass through unchanged
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((display "hello")
                  (+ 1 2)
                  (foo)))
              '((display "hello")
                (+ 1 2)
                (foo))
              "non-define top-level forms preserved")

;; ============================================================================
;; Quoted reference doesn't count
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define dead 5)
                  (quote dead)
                  (foo)))
              '((quote dead)
                (foo))
              "(quote NAME) is a literal symbol, not a reference to the binding")

;; ============================================================================
;; Self-recursive define is kept (the self-reference counts as a use)
;; ============================================================================

(check-equal? (eliminate-dead-defines
                '((define (loop) (loop))))
              '((define (loop) (loop)))
              "self-recursive function: own body references its name (conservative)")

#lang racket

;; VeloxVM Scheme Compiler - Letrec Lifting Pass Tests
;; Copyright (c) 2026, RISE Research Institutes of Sweden AB

(require rackunit
         "../letrec-lifting.rkt")

;; Helper: pull out the lifted defines and the rewritten body.
(define (split-lifted exprs)
  (let loop ([es exprs] [defs '()])
    (cond
      [(null? es) (values (reverse defs) '())]
      [(and (pair? (car es))
            (eq? (caar es) 'define)
            (symbol? (cadar es))
            (regexp-match? #rx"^\\$rec-" (symbol->string (cadar es))))
       (loop (cdr es) (cons (car es) defs))]
      [else (values (reverse defs) es)])))

;; ============================================================================
;; The qualifying shape: ((lambda (f) (set! f LAMBDA) BODY) #f)
;; where LAMBDA references f and only refers to f, params, and primitives.
;; ============================================================================

(let-values ([(defs body)
              (split-lifted
                (lift-self-recursive-letrec
                  '(((lambda (f)
                       (set! f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))
                       (f 5))
                    #f))))])
  (check-equal? (length defs) 1
                "qualifying self-rec letrec emits one $rec define")
  (check-pred (lambda (d)
                (match d
                  [`(define ,name (lambda (n) (if (= n 0) 1 (* n (,name1 (- n 1))))))
                   (and (eq? name name1)
                        (regexp-match? #rx"^\\$rec-"
                                       (symbol->string name)))]
                  [_ #f]))
              (car defs)
              "lifted lambda has self-references renamed to fresh $rec-N")
  (check-pred (lambda (b)
                (match b
                  [`((,name 5)) (regexp-match? #rx"^\\$rec-"
                                                (symbol->string name))]
                  [_ #f]))
              body
              "use site rewritten to call $rec-N"))

;; ============================================================================
;; Lambda body that captures an outer-scope variable: NOT lifted
;; ============================================================================

(let-values ([(defs body)
              (split-lifted
                (lift-self-recursive-letrec
                  '((lambda (x)
                      ((lambda (f)
                         (set! f (lambda (n) (if (= n 0) x (f (- n 1)))))
                         (f 5))
                       #f)))))])
  (check-equal? (length defs) 0
                "lambda body references outer x: not eligible for lifting")
  (check-equal? (length body) 1
                "original program preserved"))

;; ============================================================================
;; Non-self-recursive single-binding: NOT this pattern (would already
;; have been collapsed to plain let by the rewriter's non-recursive
;; letrec rule, but verify safety here too).
;; ============================================================================

(let-values ([(defs body)
              (split-lifted
                (lift-self-recursive-letrec
                  '(((lambda (f)
                       (set! f (lambda (x) (* x x)))
                       (f 5))
                    #f))))])
  (check-equal? (length defs) 0
                "lambda body doesn't reference f: not lifted"))

;; ============================================================================
;; Multiple use sites in body get renamed consistently
;; ============================================================================

(let-values ([(defs body)
              (split-lifted
                (lift-self-recursive-letrec
                  '(((lambda (f)
                       (set! f (lambda (n) (if (<= n 1) 1 (* n (f (- n 1))))))
                       (f 5)
                       (f 10))
                    #f))))])
  (check-equal? (length defs) 1
                "still one define")
  (check-pred (lambda (b)
                (match b
                  [`((begin (,name 5) (,name2 10)))
                   (and (eq? name name2)
                        (regexp-match? #rx"^\\$rec-"
                                       (symbol->string name)))]
                  [_ #f]))
              body
              "multi-expression body wrapped in begin, both call sites renamed"))

;; ============================================================================
;; Non-matching shapes are left alone
;; ============================================================================

(check-equal? (lift-self-recursive-letrec '((+ 1 2 3)))
              '((+ 1 2 3))
              "arithmetic call left alone")

(check-equal? (lift-self-recursive-letrec '((define (f x) (* x x))
                                            (f 5)))
              '((define (f x) (* x x)) (f 5))
              "top-level defines left alone")

;; Non-#f argument: not the shape we lift.
(check-equal? (lift-self-recursive-letrec
                '(((lambda (f) (set! f (lambda () (f))) (f)) 0)))
              '(((lambda (f) (set! f (lambda () (f))) (f)) 0))
              "non-#f argument: not the post-rewrite letrec shape")

;; Single-binding non-recursive (lambda body doesn't reference f): not lifted.
(check-equal? (lift-self-recursive-letrec
                '(((lambda (g)
                     (set! g (lambda (x) (+ x 1)))
                     (g 5))
                  #f)))
              '(((lambda (g)
                   (set! g (lambda (x) (+ x 1)))
                   (g 5))
                #f))
              "non-self-recursive: not lifted (handled by the rewriter's non-recursive letrec rule)")

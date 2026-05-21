#lang racket

;; VeloxVM Scheme Compiler - Self-Recursive Letrec Lifting
;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;
;; Hoists single-binding self-recursive letrec forms to top-level
;; bindings so the compiler doesn't have to box-rewrite the function
;; name:
;;
;;   (letrec ((f (lambda (n) ... (f ...) ...))) (f init))
;;     ===>      (top-level)  (define $rec-N (lambda (n) ... ($rec-N ...) ...))
;;               (call site)  ($rec-N init)
;;
;; By the time this pass runs, the letrec rewriter has already produced
;; the dummy-#f + set! shape:
;;
;;   ((lambda (f) (set! f LAMBDA) body-rest...) #f)
;;
;; That's the shape we detect. With `f` captured by LAMBDA and set!'d
;; once, the compiler's box-rewrite would box `f` -- every recursive
;; call would then go through (box-ref f). Lifting to a top-level
;; binding makes `$rec-N` a global symbol, which is looked up directly
;; without the box.
;;
;; Safety conditions for the lift:
;;
;;   - LAMBDA's body free variables are a subset of:
;;       {f, LAMBDA's own params, VM primitives}
;;     i.e. the lambda doesn't capture any outer-scope user variable.
;;     If it did, lifting to global scope would lose those captures.
;;
;;   - LAMBDA's body actually references `f` (real self-recursion).
;;     If it doesn't, the rewriter's non-recursive-letrec rule would
;;     have collapsed the letrec to a plain `let` already; we don't
;;     see that shape here.

(require "primitives.rkt")
(provide lift-self-recursive-letrec)

;; Hash for O(1) primitive lookup.
(define primitive-set
  (let ([h (make-hash)])
    (for ([p (in-list vm-primitives)])
      (hash-set! h p #t))
    h))

(define (primitive? sym)
  (hash-ref primitive-set sym #f))

;; Counter for fresh top-level names. Persists across calls so two
;; programs compiled in the same Racket process get disjoint names.
(define rec-counter (box 0))

(define (fresh-rec-name)
  (let ([n (unbox rec-counter)])
    (set-box! rec-counter (+ n 1))
    (string->symbol (string-append "$rec-" (number->string n)))))

;; ============================================================================
;; Shape detection
;; ============================================================================

;; Detect the post-rewrite shape of single self-recursive letrec:
;;
;;   ((lambda (NAME) (set! NAME LAMBDA) body-rest...) #f)
;;
;; Returns (list NAME LAMBDA body-rest) on match, #f otherwise.
(define (match-self-rec-letrec expr)
  (and (pair? expr)
       (= (length expr) 2)
       (eq? (cadr expr) #f)
       (pair? (car expr))
       (let ([op (car expr)])
         (and (eq? (car op) 'lambda)
              (>= (length op) 4)
              (pair? (cadr op))
              (= (length (cadr op)) 1)
              (symbol? (caadr op))
              (let* ([name (caadr op)]
                     [first-stmt (caddr op)]
                     [rest (cdddr op)])
                (and (pair? first-stmt)
                     (eq? (car first-stmt) 'set!)
                     (= (length first-stmt) 3)
                     (eq? (cadr first-stmt) name)
                     (let ([lam (caddr first-stmt)])
                       (and (pair? lam)
                            (eq? (car lam) 'lambda)
                            (>= (length lam) 3)
                            (lambda-self-contained? lam name)
                            (list name lam rest)))))))))

;; True iff `lam`'s body references `self-name` (real self-recursion)
;; and references nothing else outside its formal parameters and the
;; VM primitives.
(define (lambda-self-contained? lam self-name)
  (let* ([params (formals->list (cadr lam))]
         [body (cddr lam)]
         [allowed (cons self-name params)])
    (and (ormap (lambda (e) (refers-to? e self-name)) body)
         (andmap (lambda (e) (only-allowed? e allowed)) body))))

;; True iff `expr` has a free occurrence of `name`.
(define (refers-to? expr name)
  (cond
    [(eq? expr name) #t]
    [(symbol? expr) #f]
    [(not (pair? expr)) #f]
    [(eq? (car expr) 'quote) #f]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (let ([bound (formals->list (cadr expr))])
       (and (not (member name bound))
            (ormap (lambda (e) (refers-to? e name)) (cddr expr))))]
    [else
     (ormap (lambda (e) (refers-to? e name)) expr)]))

;; True iff every free variable of `expr` is in `allowed` or is a
;; VM primitive.
(define (only-allowed? expr allowed)
  (cond
    [(symbol? expr)
     (or (member expr allowed) (primitive? expr))]
    [(not (pair? expr)) #t]
    [(eq? (car expr) 'quote) #t]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (let ([inner-params (formals->list (cadr expr))])
       (andmap (lambda (e) (only-allowed? e (append inner-params allowed)))
               (cddr expr)))]
    [(eq? (car expr) 'set!)
     ;; A set! to a non-allowed symbol means we'd be mutating outer
     ;; scope; treat as out-of-bounds.
     (and (or (member (cadr expr) allowed) (primitive? (cadr expr)))
          (only-allowed? (caddr expr) allowed))]
    [else
     (andmap (lambda (e) (only-allowed? e allowed)) expr)]))

(define (formals->list formals)
  (cond
    [(null? formals) '()]
    [(symbol? formals) (list formals)]
    [(pair? formals) (cons (car formals) (formals->list (cdr formals)))]
    [else '()]))

;; ============================================================================
;; Substitution
;; ============================================================================

;; Substitute every free occurrence of `from` with `to` in `expr`,
;; respecting lambda formal shadowing and quote opacity. set! targets
;; are also rewritten when they reference `from`.
(define (rename-free expr from to)
  (cond
    [(eq? expr from) to]
    [(not (pair? expr)) expr]
    [(eq? (car expr) 'quote) expr]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (let ([bound (formals->list (cadr expr))])
       (if (member from bound)
           expr
           `(lambda ,(cadr expr)
              ,@(map (lambda (e) (rename-free e from to)) (cddr expr)))))]
    [(eq? (car expr) 'set!)
     `(set! ,(if (eq? (cadr expr) from) to (cadr expr))
            ,(rename-free (caddr expr) from to))]
    [else
     (cons (rename-free (car expr) from to)
           (map (lambda (e) (rename-free e from to)) (cdr expr)))]))

;; ============================================================================
;; Pass entry point
;; ============================================================================

(define (lift-self-recursive-letrec exprs)
  (let ([lifted '()])
    (define (emit! def)
      (set! lifted (cons def lifted)))
    (let ([transformed (map (lambda (e) (walk e emit!)) exprs)])
      (append (reverse lifted) transformed))))

(define (walk expr emit!)
  (cond
    [(not (pair? expr)) expr]
    [(eq? (car expr) 'quote) expr]
    [else
     (let ([m (match-self-rec-letrec expr)])
       (if m
           (let* ([name (car m)]
                  [lam (cadr m)]
                  [body-rest (caddr m)]
                  [rec-name (fresh-rec-name)]
                  [new-lam (walk (rename-free lam name rec-name) emit!)]
                  [new-body (map (lambda (e)
                                   (walk (rename-free e name rec-name) emit!))
                                 body-rest)])
             (emit! `(define ,rec-name ,new-lam))
             (if (= (length new-body) 1)
                 (car new-body)
                 `(begin ,@new-body)))
           (recurse-into expr emit!)))]))

(define (recurse-into expr emit!)
  (cond
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     `(lambda ,(cadr expr)
        ,@(map (lambda (e) (walk e emit!)) (cddr expr)))]
    [(and (eq? (car expr) 'define) (pair? (cadr expr)))
     `(define ,(cadr expr)
        ,@(map (lambda (e) (walk e emit!)) (cddr expr)))]
    [else
     (cons (walk (car expr) emit!)
           (map (lambda (e) (walk e emit!)) (cdr expr)))]))

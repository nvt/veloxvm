#lang racket

;; VeloxVM Racket Compiler - Dead Top-Level Define Elimination
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Removes top-level (define NAME VAL) forms whose NAME is never
;; referenced anywhere else in the program. When VAL is pure the
;; whole define vanishes; when it has side effects the value is
;; kept as a bare top-level expression so the effects still happen.
;;
;; Iterates to fixpoint so transitively-dead defines (a helper used
;; only by another dead define) all get reaped together. Variables
;; introduced by surrounding lambdas (formal parameters) shadow the
;; outer scope, so a top-level (define x ...) isn't kept alive by
;; (define (f x) ...) just because f's body mentions x.

(require "optimizer.rkt")   ; for pure?
(provide eliminate-dead-defines)

(define (eliminate-dead-defines exprs)
  (let loop ([es exprs])
    (let* ([defined (collect-defined-names es)]
           [refs (collect-references es)]
           [dead (filter (lambda (n) (not (hash-ref refs n #f))) defined)])
      (cond
        [(null? dead) es]
        [else
         (let* ([dead-set (names->set dead)]
                [next (rewrite-dropping es dead-set)])
           (if (equal? next es)
               es
               (loop next)))]))))

;; ---------------------------------------------------------------------------
;; Definitions

;; Return the name a top-level define binds, or #f if `expr` isn't a
;; (define ...) form we recognise.
(define (define-name expr)
  (and (pair? expr)
       (eq? (car expr) 'define)
       (>= (length expr) 2)
       (let ([target (cadr expr)])
         (cond
           [(symbol? target) target]
           [(pair? target) (car target)]
           [else #f]))))

;; Return a representative value expression for a define, synthesising
;; a (lambda ...) for the function-shorthand. Used solely as the input
;; to pure? when deciding whether the body must be preserved as a
;; side-effecting top-level expression after the define is dropped.
(define (define-value expr)
  (cond
    [(and (pair? expr)
          (eq? (car expr) 'define)
          (= (length expr) 3)
          (symbol? (cadr expr)))
     (caddr expr)]
    [(and (pair? expr)
          (eq? (car expr) 'define)
          (>= (length expr) 3)
          (pair? (cadr expr)))
     `(lambda ,(cdr (cadr expr)) ,@(cddr expr))]
    [else #f]))

(define (collect-defined-names exprs)
  (filter values (map define-name exprs)))

(define (names->set names)
  (let ([h (make-hash)])
    (for ([n (in-list names)])
      (hash-set! h n #t))
    h))

;; ---------------------------------------------------------------------------
;; Reference collection

;; Flatten a lambda formal-parameter spec (proper, dotted, or bare
;; symbol) into a plain list. Used for shadow tracking.
(define (formals->list formals)
  (cond
    [(null? formals) '()]
    [(symbol? formals) (list formals)]
    [(pair? formals) (cons (car formals) (formals->list (cdr formals)))]
    [else '()]))

;; Walk every top-level expression and record every symbol that
;; appears in a reference position -- anywhere except a binding
;; position (lambda formal, define LHS) or inside a quote. set!
;; targets count as references: dropping the binding would break the
;; assignment.
(define (collect-references exprs)
  (let ([refs (make-hash)])
    (for-each (lambda (e) (walk-refs e refs '())) exprs)
    refs))

(define (walk-refs expr refs shadowed)
  (cond
    [(symbol? expr)
     (unless (member expr shadowed)
       (hash-set! refs expr #t))]
    [(not (pair? expr)) (void)]
    [(eq? (car expr) 'quote) (void)]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (let ([formals (formals->list (cadr expr))])
       (for-each (lambda (e) (walk-refs e refs (append formals shadowed)))
                 (cddr expr)))]
    [(and (eq? (car expr) 'define) (>= (length expr) 3))
     (let ([target (cadr expr)])
       (cond
         [(pair? target)
          ;; function shorthand: formals shadow the body's name lookups.
          (let ([params (formals->list (cdr target))])
            (for-each (lambda (e) (walk-refs e refs (append params shadowed)))
                      (cddr expr)))]
         [else
          ;; (define NAME VAL): walk val, target is a binding.
          (walk-refs (caddr expr) refs shadowed)]))]
    [(eq? (car expr) 'set!)
     (when (and (symbol? (cadr expr))
                (not (member (cadr expr) shadowed)))
       (hash-set! refs (cadr expr) #t))
     (when (= (length expr) 3)
       (walk-refs (caddr expr) refs shadowed))]
    [else
     (for-each (lambda (e) (walk-refs e refs shadowed)) expr)]))

;; ---------------------------------------------------------------------------
;; Rewrite

;; Top-level pass that drops dead defines. For each (define NAME VAL)
;; whose NAME is in dead-set:
;;   - if VAL is pure: remove the entire form;
;;   - otherwise: replace the form with VAL alone so the side effects
;;     still run at program startup.
;; Non-define forms pass through unchanged.
(define (rewrite-dropping exprs dead-set)
  (apply append
    (map (lambda (e)
           (let ([n (define-name e)])
             (cond
               [(and n (hash-ref dead-set n #f))
                (let ([val (define-value e)])
                  (cond
                    [(or (not val) (pure? val)) '()]
                    [else (list val)]))]
               [else (list e)])))
         exprs)))

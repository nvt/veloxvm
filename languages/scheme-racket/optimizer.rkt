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

;; Apply local rules until they stop firing at this node. Beta-reduction
;; is tried first; if it doesn't fire, the constant-folding match rules
;; run. eq? is the right termination check: rules that fire either build
;; a fresh value (e.g. (+ 1 2) -> 3) or return a sub-element (e.g.
;; (+ e 0) -> e), and in both cases the result is not eq? to the input.
;; When neither pass changes the expression, eq? succeeds.
(define (apply-basic-rules-fixpoint expr)
  (let* ([beta (try-beta-reduce expr)]
         [next (if (eq? beta expr)
                   (apply-basic-rules expr)
                   beta)])
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
;; Beta-reduction of let bindings
;; ============================================================================

;; The let rewriter turns (let ((x v)) body) into ((lambda (x) body) v),
;; which allocates a bind_function frame at runtime. When v is a literal
;; with no allocation cost, we can drop that binding entirely by inlining
;; v at every reference of x. The lambda disappears once all its
;; parameters are inlined, eliminating the frame allocation.
;;
;; Conservative literal predicate: a value is safe to duplicate at
;; arbitrary use sites if it is self-evaluating and has no eq? identity.
;; Numbers, booleans, characters, and atomic quoted data qualify.
;; Strings and quoted lists/vectors are excluded -- strings have eq?
;; identity, and quoted lists currently allocate fresh storage at each
;; evaluation (item #3 will fix the latter; until then, duplicating a
;; quoted list use can be a runtime regression).
(define (beta-literal? v)
  (or (number? v)
      (boolean? v)
      (char? v)
      (and (pair? v)
           (eq? (car v) 'quote)
           (= (length v) 2)
           (let ([d (cadr v)])
             (or (symbol? d) (null? d)
                 (number? d) (boolean? d) (char? d))))))

;; Try to beta-reduce ((lambda (params...) body...) args...). Inlines
;; each parameter whose value is a literal (see beta-literal?) and
;; whose binding is never the target of a set!. Returns the input expr
;; unchanged if no parameters could be inlined, otherwise returns the
;; inlined body run through optimize-basic so newly-exposed folds fire.
;; Variadic lambdas (dotted or bare-symbol formals) are skipped.
(define (try-beta-reduce expr)
  (cond
    [(and (pair? expr)
          (pair? (car expr))
          (eq? (caar expr) 'lambda)
          (>= (length (car expr)) 3)
          (proper-list-of-symbols? (cadar expr))
          (= (length (cadar expr)) (length (cdr expr))))
     (let* ([params (cadar expr)]
            [body-list (cddar expr)]
            [body (if (= (length body-list) 1)
                      (car body-list)
                      `(begin ,@body-list))]
            [args (cdr expr)]
            [reduced (beta-fold params args body expr)])
       (if (eq? reduced expr)
           expr
           ;; Re-walk so substituted literals can feed folding rules in
           ;; the surrounding positions.
           (optimize-basic reduced)))]
    [else expr]))

;; Walk (param, arg) pairs in order, inlining each pair where arg is a
;; beta-literal and param has no set! in body. Returns:
;;   - `original` (the input expr) if nothing changed,
;;   - the substituted body if all parameters were inlined,
;;   - a smaller (lambda+args) for the partial-inline case.
(define (beta-fold params args body original)
  (let loop ([params params] [args args]
             [keep-params '()] [keep-args '()]
             [body body] [changed? #f])
    (cond
      [(null? params)
       (cond
         [(not changed?) original]
         [(null? keep-params) body]
         [else `((lambda ,(reverse keep-params) ,body)
                 ,@(reverse keep-args))])]
      [else
       (let ([p (car params)] [a (car args)])
         (if (and (beta-literal? a)
                  (= (count-set!s body p) 0))
             (loop (cdr params) (cdr args)
                   keep-params keep-args
                   (substitute body p a) #t)
             (loop (cdr params) (cdr args)
                   (cons p keep-params) (cons a keep-args)
                   body changed?)))])))

(define (proper-list-of-symbols? xs)
  (cond
    [(null? xs) #t]
    [(pair? xs) (and (symbol? (car xs))
                     (proper-list-of-symbols? (cdr xs)))]
    [else #f]))

;; Flatten a formal-parameter spec (proper, dotted, or bare-symbol) into
;; a plain list for shadow checks. Mirrors compiler.rkt's helper.
(define (formals->flat-list formals)
  (cond
    [(null? formals) '()]
    [(symbol? formals) (list formals)]
    [(pair? formals) (cons (car formals) (formals->flat-list (cdr formals)))]
    [else '()]))

;; Substitute free occurrences of `name` with `value` in `expr`,
;; respecting binder forms (lambda formals, define-shorthand formals,
;; (define name ...) targets) and quote opacity. set! targets are
;; binding positions, not references, so they are not substituted; the
;; assignment value is.
(define (substitute expr name value)
  (cond
    [(eq? expr name) value]
    [(symbol? expr) expr]
    [(not (pair? expr)) expr]
    [(eq? (car expr) 'quote) expr]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (if (member name (formals->flat-list (cadr expr)))
         expr
         `(lambda ,(cadr expr)
            ,@(map (lambda (e) (substitute e name value)) (cddr expr))))]
    [(and (eq? (car expr) 'define) (>= (length expr) 3))
     (let ([target (cadr expr)])
       (cond
         [(eq? target name) expr]
         [(and (pair? target)
               (or (eq? (car target) name)
                   (member name (formals->flat-list (cdr target)))))
          expr]
         [else
          `(define ,target
             ,@(map (lambda (e) (substitute e name value)) (cddr expr)))]))]
    [(eq? (car expr) 'set!)
     `(set! ,(cadr expr) ,(substitute (caddr expr) name value))]
    [else
     (cons (substitute (car expr) name value)
           (map (lambda (e) (substitute e name value)) (cdr expr)))]))

;; Count set! statements targeting `name` in `expr`, respecting the
;; same scope rules as substitute.
(define (count-set!s expr name)
  (cond
    [(symbol? expr) 0]
    [(not (pair? expr)) 0]
    [(eq? (car expr) 'quote) 0]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3))
     (if (member name (formals->flat-list (cadr expr)))
         0
         (apply + (map (lambda (e) (count-set!s e name)) (cddr expr))))]
    [(and (eq? (car expr) 'define) (>= (length expr) 3))
     (let ([target (cadr expr)])
       (cond
         [(eq? target name) 0]
         [(and (pair? target)
               (or (eq? (car target) name)
                   (member name (formals->flat-list (cdr target)))))
          0]
         [else (apply + (map (lambda (e) (count-set!s e name)) (cddr expr)))]))]
    [(eq? (car expr) 'set!)
     (+ (if (eq? (cadr expr) name) 1 0)
        (count-set!s (caddr expr) name))]
    [else
     (apply + (map (lambda (e) (count-set!s e name)) expr))]))

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

      ;; Strength reduction. Bind the argument to a temp so the
      ;; expression isn't evaluated twice -- the previous (+ ,expr
      ;; ,expr) substitution silently re-ran side effects and double-
      ;; computed even pure expressions. The optimizer runs after the
      ;; rewriter, so we emit a lambda application directly rather
      ;; than a let form (which would never be rewritten).
      [`(* ,e 2)
       (let ([t (gensym '$mul2)])
         `((lambda (,t) (+ ,t ,t)) ,e))]
      [`(* 2 ,e)
       (let ([t (gensym '$mul2)])
         `((lambda (,t) (+ ,t ,t)) ,e))]

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

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
    ;; Variadic constant folding for arithmetic. Matches any arity
    ;; where every argument is a literal number. Subsumes the prior
    ;; binary-only rules.
    [(list '+ (? number? ns) ...) (apply + ns)]
    [(list '* (? number? ns) ...) (apply * ns)]
    [(list '- (? number? ns) ...)
     #:when (not (null? ns))
     (apply - ns)]
    [(list '/ (? number? ns) ...)
     #:when (and (>= (length ns) 2) (not (member 0 (cdr ns))))
     (apply / ns)]

    ;; Variadic constant folding for comparisons. R5RS requires at
    ;; least one argument; we fold the typical case of two or more
    ;; (single-arg comparisons are vacuously true and aren't worth
    ;; a rule).
    [(list '= (? number? ns) ...)  #:when (>= (length ns) 2) (apply = ns)]
    [(list '< (? number? ns) ...)  #:when (>= (length ns) 2) (apply < ns)]
    [(list '> (? number? ns) ...)  #:when (>= (length ns) 2) (apply > ns)]
    [(list '<= (? number? ns) ...) #:when (>= (length ns) 2) (apply <= ns)]
    [(list '>= (? number? ns) ...) #:when (>= (length ns) 2) (apply >= ns)]

    ;; Constant folding for boolean operations. The general (not LIT)
    ;; rule below subsumes the (not #t) / (not #f) cases.
    [`(not ,v) #:when (literal-truthy-known? v) (not v)]
    [`(not (quote ,d)) (not d)]

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
    ;; Flatten nested begins. (begin a (begin b c) d) -> (begin a b c d).
    ;; Each nested begin would otherwise cost one expression-table slot
    ;; and one form-ref indirection at runtime; collapsing to a single
    ;; inline form saves both.
    [(list 'begin es ...)
     #:when (ormap (lambda (e) (and (pair? e) (eq? (car e) 'begin))) es)
     (let ([flat (apply append
                        (map (lambda (e)
                               (if (and (pair? e) (eq? (car e) 'begin))
                                   (cdr e)
                                   (list e)))
                             es))])
       (cond
         [(null? flat) #f]
         [(= (length flat) 1) (car flat)]
         [else (cons 'begin flat)]))]
    ;; Drop pure non-final expressions: their values are discarded
    ;; and they can't be observed. The final expression's value is
    ;; the begin's value and must be kept even when pure.
    [(list 'begin es ...)
     #:when (and (>= (length es) 2)
                 (ormap pure? (drop-right es 1)))
     (let ([kept (drop-pure-non-final es)])
       (cond
         [(null? kept) #f]
         [(= (length kept) 1) (car kept)]
         [else (cons 'begin kept)]))]

    ;; ---- Pure-builtin folding ----
    ;; All of these have well-defined value semantics that depend only
    ;; on their (literal) arguments; running them at compile time is
    ;; observationally equivalent to running them at runtime.

    ;; (length '(...)) -> integer (only for proper-list datums; the
    ;; compile-quote path handles those)
    [`(length (quote ,d)) #:when (list? d) (length d)]

    ;; (string-length "...") -> integer
    [`(string-length ,s) #:when (string? s) (string-length s)]

    ;; (string-append "a" "b" ...) -> concatenated string. Variadic;
    ;; (string-append) folds to "".
    [(list 'string-append (? string? ss) ...) (apply string-append ss)]

    ;; (char->integer #\X) -> codepoint
    [`(char->integer ,c) #:when (char? c) (char->integer c)]

    ;; (integer->char N) -> #\X. The VM stores characters as uint8_t,
    ;; so refuse to fold codepoints outside [0, 255] -- the runtime
    ;; would reject them and we don't want to bake a value the
    ;; encoder can't emit.
    [`(integer->char ,n)
     #:when (and (exact-integer? n) (>= n 0) (<= n 255))
     (integer->char n)]

    ;; (string->symbol "name") -> 'name
    [`(string->symbol ,s) #:when (string? s)
     (list 'quote (string->symbol s))]

    ;; eq? on literals with deterministic identity. There are two
    ;; channels: (quote DATUM) where DATUM is an interned-atomic, and
    ;; bare self-evaluating literals. A bare SYMBOL in source is a
    ;; variable reference, never a literal -- so symbols are foldable
    ;; only under quote, not as bare arguments.
    [`(eq? (quote ,a) (quote ,b))
     #:when (and (eq-foldable-quoted? a) (eq-foldable-quoted? b))
     (eq? a b)]
    [`(eq? ,a ,b)
     #:when (and (eq-foldable-unquoted? a) (eq-foldable-unquoted? b))
     (eq? a b)]

    ;; eqv? extends eq? with numbers (well-defined across exact/inexact).
    [`(eqv? (quote ,a) (quote ,b))
     #:when (and (eqv-foldable-quoted? a) (eqv-foldable-quoted? b))
     (eqv? a b)]
    [`(eqv? ,a ,b)
     #:when (and (eqv-foldable-unquoted? a) (eqv-foldable-unquoted? b))
     (eqv? a b)]

    ;; equal? is structural and safe to fold on any pair of literal
    ;; data, including nested quoted lists/vectors.
    [`(equal? (quote ,a) (quote ,b)) (equal? a b)]
    [`(equal? ,a ,b)
     #:when (and (self-evaluating-literal? a)
                 (self-evaluating-literal? b))
     (equal? a b)]

    ;; (apply f arg1 ... '(elem1 elem2 ...)) -> (f arg1 ... elem1 elem2 ...)
    ;; Removes the runtime apply-machinery dispatch and the temporary
    ;; argument-list allocation. Only fires when the final argument is
    ;; a quoted proper-list literal; elements that are not self-
    ;; evaluating get re-wrapped in (quote ...) so the result preserves
    ;; the original value semantics.
    [(list 'apply f rest ...)
     #:when (and (not (null? rest))
                 (let ([last-arg (last rest)])
                   (and (pair? last-arg)
                        (eq? (car last-arg) 'quote)
                        (= (length last-arg) 2)
                        (list? (cadr last-arg)))))
     (let* ([prefix (drop-right rest 1)]
            [tail (cadr (last rest))])
       (cons f (append prefix (map maybe-quote-elem tail))))]

    ;; No rule matched.
    [else expr]))

;; Re-attach a quote to a value unless it's self-evaluating. Used when
;; splicing the elements of a quoted-list argument into a call -- a
;; symbol becomes a variable reference if left bare.
(define (maybe-quote-elem v)
  (cond
    [(number? v) v]
    [(boolean? v) v]
    [(char? v) v]
    [(string? v) v]
    [else (list 'quote v)]))

;; ============================================================================
;; Predicates supporting pure-builtin folding
;; ============================================================================

;; The (not LIT) rule above fires when we can statically determine
;; the literal's truthiness. Every value here has a known boolean
;; coercion: #f is false, everything else is true (R5RS 6.3.1).
(define (literal-truthy-known? v)
  (or (number? v) (boolean? v) (char? v) (string? v)))

;; Inside (quote DATUM), DATUM is a literal regardless of its shape;
;; eq? has deterministic identity on symbols (interned), booleans
;; (unique values), characters, and the empty list.
(define (eq-foldable-quoted? x)
  (or (symbol? x) (boolean? x) (char? x) (null? x)))

;; eqv? extends eq? to numbers (well-defined across exact/inexact).
(define (eqv-foldable-quoted? x)
  (or (eq-foldable-quoted? x) (number? x)))

;; When the argument is NOT wrapped in quote, only self-evaluating
;; literals count -- symbols, () and similar reach this position only
;; as variable references. Folding (eq? sym1 sym2) would treat the
;; variable names as values.
(define (eq-foldable-unquoted? x)
  (or (boolean? x) (char? x)))

(define (eqv-foldable-unquoted? x)
  (or (eq-foldable-unquoted? x) (number? x)))

;; A self-evaluating literal in source -- includes strings, which are
;; not interned-identity-comparable, so they're only safe for equal?.
(define (self-evaluating-literal? x)
  (or (number? x) (boolean? x) (char? x) (string? x)))

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
;; Effect Analysis (used by the begin dead-code rule)
;; ============================================================================

;; VM primitives whose execution is observable only through their
;; return value. They never modify state, never perform I/O, and
;; never spawn threads. Allocation (cons, list, vector) is treated
;; as pure -- R5RS programs cannot observe GC, and dropping a pure
;; allocation just saves work. Some of these can raise errors
;; (e.g. car on '()); we accept that risk on the same footing as
;; Racket's own dead-code elimination.
(define pure-primitive-table
  (let ([h (make-hash)])
    (for ([sym (in-list
                '(+ - * / gcd lcm modulo quotient remainder
                  numerator denominator
                  = /= < <= > >= zero?
                  eq? eqv? equal? not
                  car cdr cons list pair? null? list? length
                  reverse append memq memv member assq assv assoc
                  list-ref list-tail
                  vector vector? vector-length vector-ref
                  vector->list list->vector make-vector
                  string? string-length string-ref substring
                  string-append string-copy string-compare
                  string->list list->string make-string
                  string->symbol symbol->string
                  string->number number->string
                  char? char-compare char-class
                  char->integer integer->char
                  char-upcase char-downcase
                  number? integer? rational? real? complex?
                  exact? inexact? procedure? symbol? boolean? port?
                  abs max min
                  floor ceiling round truncate
                  exp log sin cos tan asin acos atan
                  sqrt expt exact->inexact inexact->exact
                  bit-and bit-or bit-invert bit-not bit-xor bit-shift
                  box box-ref))])
      (hash-set! h sym #t))
    h))

(define (pure-primitive? sym)
  (hash-ref pure-primitive-table sym #f))

;; True iff `expr` can be evaluated without observable side effects:
;; no I/O, no mutation, no thread interaction, no application of
;; potentially-impure user procedures. Lambda forms are pure (they
;; don't evaluate the body). Variable references and atoms are pure.
;; Pure-primitive calls are pure iff all argument expressions are pure.
(define (pure? expr)
  (cond
    [(not (pair? expr)) #t]
    [(eq? (car expr) 'quote) #t]
    [(and (eq? (car expr) 'lambda) (>= (length expr) 3)) #t]
    [(eq? (car expr) 'if) (andmap pure? (cdr expr))]
    [(eq? (car expr) 'and) (andmap pure? (cdr expr))]
    [(eq? (car expr) 'or) (andmap pure? (cdr expr))]
    [(eq? (car expr) 'begin) (andmap pure? (cdr expr))]
    [(and (symbol? (car expr)) (pure-primitive? (car expr)))
     (andmap pure? (cdr expr))]
    [else #f]))

;; Used by the (begin) optimization rule below. Filters out pure
;; expressions in non-final position; the final expression's value
;; is the begin's value and must be kept regardless of purity.
(define (drop-pure-non-final exprs)
  (let-values ([(non-final final) (split-at-right exprs 1)])
    (append (filter (lambda (e) (not (pure? e))) non-final) final)))

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

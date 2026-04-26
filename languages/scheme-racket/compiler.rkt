#lang racket

;; VeloxVM Racket Compiler - Core Compiler
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Compiles Scheme expressions to VeloxVM bytecode

(require "bytecode.rkt"
         "primitives.rkt")

(provide compile-expr
         compile-program)

;; Compile single expression
;; expr: Scheme s-expression
;; bc: bytecode structure (mutable)
;; env: lexical environment (list of lambda parameter symbols)
;; Returns: expr-encoding
(define (compile-expr expr bc [env '()])
  (cond
    ;; ========================================================================
    ;; Atoms
    ;; ========================================================================

    [(not (pair? expr))
     (compile-atom expr bc env)]

    ;; ========================================================================
    ;; Special Forms
    ;; ========================================================================

    ;; Quote
    [(eq? (car expr) 'quote)
     (compile-quote (cadr expr) bc env)]

    ;; Lambda
    [(eq? (car expr) 'lambda)
     (compile-lambda (cadr expr) (cddr expr) bc env)]

    ;; If
    [(eq? (car expr) 'if)
     (compile-if (cdr expr) bc env)]

    ;; Define
    [(eq? (car expr) 'define)
     (compile-define (cdr expr) bc env)]

    ;; Set!
    [(eq? (car expr) 'set!)
     (compile-set (cadr expr) (caddr expr) bc env)]

    ;; Begin
    [(eq? (car expr) 'begin)
     (compile-begin (cdr expr) bc env)]

    ;; And
    [(eq? (car expr) 'and)
     (compile-and (cdr expr) bc env)]

    ;; Or
    [(eq? (car expr) 'or)
     (compile-or (cdr expr) bc env)]

    ;; ========================================================================
    ;; Function Application
    ;; ========================================================================

    [else
     (compile-application expr bc env)]))

;; ============================================================================
;; Atom Compilation
;; ============================================================================

(define (compile-atom atom bc [env '()])
  (cond
    ;; Number
    [(number? atom)
     (cond
       [(and (integer? atom) (exact? atom))
        (encode-integer atom)]
       [(and (rational? atom) (exact? atom))
        (encode-rational atom)]
       [else
        (encode-real atom)])]

    ;; String
    [(string? atom)
     (encode-string atom bc)]

    ;; Symbol
    [(symbol? atom)
     (encode-symbol atom bc env)]

    ;; Boolean
    [(boolean? atom)
     (encode-boolean atom)]

    ;; Character
    [(char? atom)
     (encode-character atom)]

    ;; Null/empty list
    [(null? atom)
     (encode-nil)]

    [else
     (error 'compile-atom "Unknown atom type: ~a" atom)]))

;; ============================================================================
;; Special Form Compilation (stubs for now)
;; ============================================================================

(define (compile-quote datum bc [env '()])
  ;; Quote is a VM special form that doesn't evaluate its argument
  ;; We must encode it as: inline(2) + quote-symbol + datum
  ;; The quote operator receives the unevaluated datum and returns it as-is
  ;;
  ;; For lists: The CL compiler rewrites (quote (a b c)) to (list (quote a) (quote b) (quote c))
  ;; For vectors: Rewrite (quote #(a b c)) to (vector (quote a) (quote b) (quote c))
  ;; For atoms: We generate (quote atom) as an inline form
  ;;
  ;; R5RS COMPLIANCE: Empty list '() must be distinct from #f (see R5RS 6.3.1)
  ;; We handle empty lists by rewriting '() to (list), creating a VM_TYPE_LIST object
  (cond
    [(and (list? datum) (not (null? datum)))
     ;; Non-empty list: rewrite to list constructor with quoted elements
     ;; This matches CL compiler behavior: (quote (a b)) => (list (quote a) (quote b))
     (compile-expr `(list ,@(map (lambda (e) `(quote ,e)) datum)) bc env)]
    [(null? datum)
     ;; Empty list: rewrite '() to (list) to create VM_TYPE_LIST object
     ;; This ensures '() is distinct from #f per R5RS requirements
     (compile-expr '(list) bc env)]
    [(vector? datum)
     ;; Vector literal: rewrite to vector constructor with quoted elements
     ;; (quote #(a b c)) => (vector (quote a) (quote b) (quote c))
     (let ([elements (vector->list datum)])
       (if (null? elements)
           ;; Empty vector
           (compile-expr '(vector) bc env)
           ;; Non-empty vector
           (compile-expr `(vector ,@(map (lambda (e) `(quote ,e)) elements)) bc env)))]
    [else
     ;; Atom: encode as inline form (quote datum)
     (let* ([quote-enc (encode-symbol 'quote bc env)]
            [datum-enc (compile-atom datum bc env)]
            [all-bytes (append (expr-encoding-data (encode-form-inline 2))
                               (expr-encoding-data quote-enc)
                               (expr-encoding-data datum-enc))])
       (expr-encoding 'form all-bytes))]))

(define (compile-lambda args body bc [env '()])
  ;; (lambda (args...) body...)
  ;; Compile as: (bind_function args... (begin body...)) or (bind_function args... body)
  ;; Use bind_function to mark actual function boundaries (enables proper return unwinding)
  ;; Store in expression table and return a lambda form reference
  ;; Extend env with lambda parameters when compiling body
  ;; The VM's bind_function requires exact arity matching (see
  ;; core/expr-primitives.c:bind_function), so R5RS variadic parameter
  ;; lists — (lambda args body) and (lambda (a . rest) body) — are rejected.
  (unless (and (list? args) (andmap symbol? args))
    (error 'compile-lambda
           "variadic lambda parameters not supported by the VM: ~a"
           args))
  (let* ([extended-env (append args env)]  ; Add lambda parameters to env
         ;; Box-rewrite: any param that is both captured by an inner lambda
         ;; AND mutated via set! must live in shared (heap-allocated) storage
         ;; so writes through one alias are visible through all aliases.
         ;; rewrite-body wraps each such param at function entry with
         ;; (set! p (box p)) and rewrites reads/writes in the rest of the
         ;; body to go through box-ref / box-set!. Plain reads of non-boxed
         ;; params are untouched.
         [boxed-params (params-needing-box body args)]
         [body (box-rewrite body boxed-params)]
         [bind-expr (if (> (length body) 1)
                        ;; Multiple body expressions: wrap in begin
                        `(bind_function ,@args (begin ,@body))
                        ;; Single body expression
                        `(bind_function ,@args ,@body))]
         ;; Compile the bind expression directly into main bc with extended env
         ;; This ensures all form references are correct from the start
         [bind-enc (compile-expr bind-expr bc extended-env)]
         ;; Add to main bc and get its index
         [expr-id (bytecode-expression-count bc)]
         [_ (add-expr bc bind-enc)]
         ;; Free-variable analysis: identify names referenced in the body
         ;; that come from an outer lambda's scope (env) but aren't bound
         ;; by this lambda's own params. These are the captures the
         ;; runtime needs to snapshot when this lambda is evaluated.
         [captures (lambda-captures body args env)])
    (record-captures! bc expr-id captures)
    ;; Return lambda form referencing the bind expression
    (encode-form-lambda expr-id)))

;; ============================================================================
;; Free-Variable Analysis (for closure capture)
;; ============================================================================

;; Compute the captures-list for a lambda whose body is body, parameters are
;; params, and surrounding lexical scope is outer-env. A captured name is
;; one that is referenced in body, is not bound by this lambda (params or
;; any nested binder), and is bound in outer-env (so it's an outer
;; lambda's local). Top-level globals resolve through symbol_bindings and
;; don't need capturing.
;;
;; The primitive-ness of a name is irrelevant here: if a name is in
;; outer-env it's a local that shadows any same-named primitive, and the
;; closure must capture the local binding, not the primitive. (count is a
;; VM primitive but is also a perfectly valid let-bound variable name.)
;;
;; Returns a deduplicated list of symbol names.
(define (lambda-captures body params outer-env)
  (let* ([fvs (apply append
                     (map (lambda (e) (free-symbols e params)) body))]
         [unique (remove-duplicates fvs)])
    (filter (lambda (s) (member s outer-env)) unique)))

;; Symbols referenced in expr that aren't bound by local (a list of
;; names introduced by enclosing lambda/let/etc. binders).
(define (free-symbols expr local)
  (cond
    [(symbol? expr)
     (if (member expr local) '() (list expr))]
    [(not (pair? expr)) '()]
    [(eq? (car expr) 'quote) '()]
    [(eq? (car expr) 'lambda)
     ;; (lambda (inner-params...) inner-body...) -- inner-params shadow outer
     (let ([inner-params (cadr expr)]
           [inner-body (cddr expr)])
       (apply append
              (map (lambda (e) (free-symbols e (append inner-params local)))
                   inner-body)))]
    [(eq? (car expr) 'define)
     ;; (define name val) or (define (name args...) body...)
     ;; The name itself is bound; the value/body sees it in scope.
     (let* ([target (cadr expr)]
            [binding-name (if (pair? target) (car target) target)]
            [val-expr (if (pair? target)
                          `(lambda ,(cdr target) ,@(cddr expr))
                          (caddr expr))])
       (free-symbols val-expr (cons binding-name local)))]
    [(eq? (car expr) 'set!)
     ;; (set! name val) -- name is referenced (read for the binding lookup),
     ;; val is the new value. Both contribute to free vars.
     (append (free-symbols (cadr expr) local)
             (free-symbols (caddr expr) local))]
    [else
     ;; if / begin / and / or / application / bind_function: union over all
     ;; subexpressions. The operator is also walked, which is fine because
     ;; primitives are filtered out by lambda-captures.
     (apply append (map (lambda (e) (free-symbols e local)) expr))]))

;; ============================================================================
;; Box Rewriting (for set! on captured variables)
;; ============================================================================

;; A parameter needs boxing iff it is (a) captured by some inner lambda
;; nested in body and (b) the target of a set! somewhere in body. Without
;; the box, the inner lambda's snapshot is independent of the parent's
;; binding (and of any other closure capturing the same param), so writes
;; through one path are invisible to the others.
(define (params-needing-box body params)
  (let ([captured (params-captured-by-inner body params)]
        [mutated (params-mutated-in body params)])
    (filter (lambda (p) (and (member p captured) (member p mutated))) params)))

;; Subset of params that are referenced inside any inner lambda nested in
;; body, without that inner lambda or anything between rebinding the name.
(define (params-captured-by-inner body params)
  (define hits '())
  (define (walk expr inside? local)
    (cond
      [(symbol? expr)
       (when (and inside? (member expr params) (not (member expr local)))
         (set! hits (cons expr hits)))]
      [(not (pair? expr)) (void)]
      [(eq? (car expr) 'quote) (void)]
      [(eq? (car expr) 'lambda)
       (let ([inner-params (cadr expr)]
             [inner-body (cddr expr)])
         (for-each (lambda (e) (walk e #t (append inner-params local)))
                   inner-body))]
      [(eq? (car expr) 'set!)
       ;; The target itself isn't a "reference" -- it's the assignment
       ;; LHS. Only the value expression contributes.
       (walk (caddr expr) inside? local)]
      [(eq? (car expr) 'define)
       (let* ([target (cadr expr)]
              [name (if (pair? target) (car target) target)]
              [val (if (pair? target)
                       `(lambda ,(cdr target) ,@(cddr expr))
                       (caddr expr))])
         (walk val inside? (cons name local)))]
      [else
       (for-each (lambda (e) (walk e inside? local)) expr)]))
  (for-each (lambda (e) (walk e #f '())) body)
  (remove-duplicates hits))

;; Subset of params that appear as the target of a set! somewhere in body
;; (in a scope where the name still refers to this lambda's param, i.e.
;; not shadowed by an inner binder that rebinds the same name).
(define (params-mutated-in body params)
  (define hits '())
  (define (walk expr local)
    (cond
      [(symbol? expr) (void)]
      [(not (pair? expr)) (void)]
      [(eq? (car expr) 'quote) (void)]
      [(eq? (car expr) 'lambda)
       (let ([inner-params (cadr expr)]
             [inner-body (cddr expr)])
         (for-each (lambda (e) (walk e (append inner-params local)))
                   inner-body))]
      [(eq? (car expr) 'set!)
       (let ([target (cadr expr)])
         (when (and (member target params) (not (member target local)))
           (set! hits (cons target hits))))
       (walk (caddr expr) local)]
      [(eq? (car expr) 'define)
       (let* ([target (cadr expr)]
              [name (if (pair? target) (car target) target)]
              [val (if (pair? target)
                       `(lambda ,(cdr target) ,@(cddr expr))
                       (caddr expr))])
         (walk val (cons name local)))]
      [else
       (for-each (lambda (e) (walk e local)) expr)]))
  (for-each (lambda (e) (walk e '())) body)
  (remove-duplicates hits))

;; Rewrite body so that boxed-params live in heap boxes:
;;   * Inject (set! p (box p)) at function entry to wrap the initial value.
;;   * Replace each read of p with (box-ref p).
;;   * Replace each (set! p v) with (box-set! p v).
;; The wraps are constructed AFTER rewriting so the (box p) inside them
;; remains a plain reference. Inner lambdas that rebind a name are
;; respected -- the rebound scope sees the inner-params, not the outer
;; box-rewritten name.
(define (box-rewrite body boxed-params)
  (if (null? boxed-params)
      body
      (let ([rewritten (map (lambda (e) (box-rewrite-expr e boxed-params)) body)])
        (append
          (map (lambda (p) `(set! ,p (box ,p))) boxed-params)
          rewritten))))

(define (box-rewrite-expr expr boxed)
  (cond
    [(symbol? expr)
     (if (member expr boxed) `(box-ref ,expr) expr)]
    [(not (pair? expr)) expr]
    [(eq? (car expr) 'quote) expr]
    [(eq? (car expr) 'lambda)
     ;; Inner lambda: drop any boxed names that the inner params shadow.
     (let* ([inner-params (cadr expr)]
            [inner-body (cddr expr)]
            [active (filter (lambda (b) (not (member b inner-params))) boxed)])
       `(lambda ,inner-params
          ,@(map (lambda (e) (box-rewrite-expr e active)) inner-body)))]
    [(eq? (car expr) 'set!)
     (let ([target (cadr expr)]
           [val (caddr expr)])
       (if (member target boxed)
           `(box-set! ,target ,(box-rewrite-expr val boxed))
           `(set! ,target ,(box-rewrite-expr val boxed))))]
    [(eq? (car expr) 'define)
     ;; v1 of the box rewrite doesn't try to box internal-define bindings.
     ;; The value expression is rewritten so any references to outer boxed
     ;; names go through box-ref.
     (let ([target (cadr expr)]
           [val-or-body (cddr expr)])
       (cons 'define
             (cons target
                   (map (lambda (e) (box-rewrite-expr e boxed)) val-or-body))))]
    [else
     (map (lambda (e) (box-rewrite-expr e boxed)) expr)]))

;; Compile a sub-expression of a compound form.
;;   - Atoms (non-pair) are compiled inline and their bytes embed directly
;;     into the enclosing form.
;;   - Lambdas are compiled inline too: compile-lambda already stores the
;;     bind_function body as its own expression and returns a lambda-form
;;     byte that the enclosing form can embed, so no extra indirection is
;;     needed.
;;   - Any other compound expression is compiled into its own expression
;;     table slot and represented in the enclosing form by a form-ref.
(define (compile-subexpr e bc env)
  (if (and (pair? e) (not (eq? (car e) 'lambda)))
      (encode-form-ref (add-expr bc (compile-expr e bc env)))
      (compile-expr e bc env)))

(define (compile-if args bc [env '()])
  ;; (if test consequent [alternate])
  ;; Compile as a call to the 'if primitive
  ;; Inline form comes FIRST, then operator, then arguments
  (let* ([test (car args)]
         [consequent (cadr args)]
         [alternate (if (null? (cddr args)) #f (caddr args))]
         [argc (if alternate 4 3)]  ; if + test + consequent [+ alternate]
         [if-enc (encode-symbol 'if bc env)]
         [test-enc (compile-subexpr test bc env)]
         [cons-enc (compile-subexpr consequent bc env)]
         [alt-enc (and alternate (compile-subexpr alternate bc env))]
         [head-bytes (append (expr-encoding-data (encode-form-inline argc))
                             (expr-encoding-data if-enc)
                             (expr-encoding-data test-enc)
                             (expr-encoding-data cons-enc))]
         [all-bytes (if alternate
                        (append head-bytes (expr-encoding-data alt-enc))
                        head-bytes)])
    (expr-encoding 'form all-bytes)))

(define (compile-define args bc [env '()])
  ;; (define var expr) or (define (name args...) body...)
  ;; Compile as a call to the 'define primitive
  ;; Inline form comes FIRST, then operator, then arguments
  (let ([first-arg (car args)])
    (if (pair? first-arg)
        ;; Function definition: (define (name args...) body...)
        ;; Rewrite as: (define name (lambda (args...) body...))
        (let* ([name (car first-arg)]
               [params (cdr first-arg)]
               [body (cdr args)]
               [define-enc (encode-symbol 'define bc env)]
               [name-enc (encode-symbol name bc env)]
               [lambda-enc (compile-lambda params body bc env)]
               [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                                  (expr-encoding-data define-enc)
                                  (expr-encoding-data name-enc)
                                  (expr-encoding-data lambda-enc))])
          (expr-encoding 'form all-bytes))
        ;; Variable definition: (define var expr)
        (let* ([var first-arg]
               [val (cadr args)]
               [define-enc (encode-symbol 'define bc env)]
               [var-enc (encode-symbol var bc env)]
               [val-enc (compile-subexpr val bc env)]
               [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                                  (expr-encoding-data define-enc)
                                  (expr-encoding-data var-enc)
                                  (expr-encoding-data val-enc))])
          (expr-encoding 'form all-bytes)))))

(define (compile-set var val bc [env '()])
  ;; (set! var val)
  ;; Compile as a call to the 'set! primitive
  ;; Inline form comes FIRST, then operator, then arguments
  (let* ([set-enc (encode-symbol 'set! bc env)]
         [var-enc (encode-symbol var bc env)]
         [val-enc (compile-subexpr val bc env)]
         [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                            (expr-encoding-data set-enc)
                            (expr-encoding-data var-enc)
                            (expr-encoding-data val-enc))])
    (expr-encoding 'form all-bytes)))

;; Shared helper: compile (op-sym arg1 arg2 ...) into an inline form where
;; the operator is a core VM symbol (begin / and / or) and each argument
;; goes through compile-subexpr.
(define (compile-inline-form op-sym args bc env)
  (let* ([argc (+ 1 (length args))]
         [op-enc (encode-symbol op-sym bc env)]
         [arg-encs (map (lambda (e) (compile-subexpr e bc env)) args)])
    (expr-encoding 'form
                   (append (expr-encoding-data (encode-form-inline argc))
                           (expr-encoding-data op-enc)
                           (apply append (map expr-encoding-data arg-encs))))))

(define (compile-begin exprs bc [env '()])
  (compile-inline-form 'begin exprs bc env))

(define (compile-and exprs bc [env '()])
  (compile-inline-form 'and exprs bc env))

(define (compile-or exprs bc [env '()])
  (compile-inline-form 'or exprs bc env))

(define (compile-application expr bc [env '()])
  ;; Function application: (func arg1 arg2 ...)
  ;; Both the function position and each argument go through compile-subexpr:
  ;; compound sub-expressions are lifted into the expression table as form-refs
  ;; so the runtime sees a single token. Inlining a compound operator (e.g.
  ;; ((box-ref loop) 0)) corrupts byte parsing because vm_get_object advances
  ;; past the inline header only, leaving the inner body to be misread as the
  ;; outer call's arguments. Atoms and lambdas remain inlined since they
  ;; encode as a single token.
  (let* ([func (car expr)]
         [args (cdr expr)]
         [total-count (+ 1 (length args))]
         [func-enc (compile-subexpr func bc env)]
         [arg-encs (map (lambda (arg) (compile-subexpr arg bc env)) args)])
    (expr-encoding 'form
                   (append (expr-encoding-data (encode-form-inline total-count))
                           (expr-encoding-data func-enc)
                           (apply append (map expr-encoding-data arg-encs))))))

;; ============================================================================
;; Program Compilation
;; ============================================================================

(define (compile-program exprs)
  (define bc (make-bytecode))
  (for ([expr exprs])
    (compile-expr expr bc))
  bc)

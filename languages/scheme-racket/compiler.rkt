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
;; Returns: expr-encoding
(define (compile-expr expr bc)
  (cond
    ;; ========================================================================
    ;; Atoms
    ;; ========================================================================

    [(not (pair? expr))
     (compile-atom expr bc)]

    ;; ========================================================================
    ;; Special Forms
    ;; ========================================================================

    ;; Quote
    [(eq? (car expr) 'quote)
     (compile-quote (cadr expr) bc)]

    ;; Lambda
    [(eq? (car expr) 'lambda)
     (compile-lambda (cadr expr) (cddr expr) bc)]

    ;; If
    [(eq? (car expr) 'if)
     (compile-if (cdr expr) bc)]

    ;; Define
    [(eq? (car expr) 'define)
     (compile-define (cdr expr) bc)]

    ;; Set!
    [(eq? (car expr) 'set!)
     (compile-set (cadr expr) (caddr expr) bc)]

    ;; Begin
    [(eq? (car expr) 'begin)
     (compile-begin (cdr expr) bc)]

    ;; And
    [(eq? (car expr) 'and)
     (compile-and (cdr expr) bc)]

    ;; Or
    [(eq? (car expr) 'or)
     (compile-or (cdr expr) bc)]

    ;; ========================================================================
    ;; Function Application
    ;; ========================================================================

    [else
     (compile-application expr bc)]))

;; ============================================================================
;; Atom Compilation
;; ============================================================================

(define (compile-atom atom bc)
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
     (encode-symbol atom bc)]

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

(define (compile-quote datum bc)
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
     (compile-expr `(list ,@(map (lambda (e) `(quote ,e)) datum)) bc)]
    [(null? datum)
     ;; Empty list: rewrite '() to (list) to create VM_TYPE_LIST object
     ;; This ensures '() is distinct from #f per R5RS requirements
     (compile-expr '(list) bc)]
    [(vector? datum)
     ;; Vector literal: rewrite to vector constructor with quoted elements
     ;; (quote #(a b c)) => (vector (quote a) (quote b) (quote c))
     (let ([elements (vector->list datum)])
       (if (null? elements)
           ;; Empty vector
           (compile-expr '(vector) bc)
           ;; Non-empty vector
           (compile-expr `(vector ,@(map (lambda (e) `(quote ,e)) elements)) bc)))]
    [else
     ;; Atom: encode as inline form (quote datum)
     (let* ([quote-enc (encode-symbol 'quote bc)]
            [datum-enc (compile-atom datum bc)]
            [all-bytes (append (expr-encoding-data (encode-form-inline 2))
                               (expr-encoding-data quote-enc)
                               (expr-encoding-data datum-enc))])
       (expr-encoding 'form all-bytes))]))

(define (compile-lambda args body bc)
  ;; (lambda (args...) body...)
  ;; Compile as: (bind args... (begin body...)) or (bind args... body)
  ;; Store in expression table and return a lambda form reference
  (let* ([bind-expr (if (> (length body) 1)
                        ;; Multiple body expressions: wrap in begin
                        `(bind ,@args (begin ,@body))
                        ;; Single body expression
                        `(bind ,@args ,@body))]
         ;; Compile the bind expression directly into main bc
         ;; This ensures all form references are correct from the start
         [bind-enc (compile-expr bind-expr bc)]
         ;; Add to main bc and get its index
         [expr-id (length (bytecode-expressions bc))]
         [_ (add-expr bc bind-enc)])
    ;; Return lambda form referencing the bind expression
    (encode-form-lambda expr-id)))

(define (compile-if args bc)
  ;; (if test consequent [alternate])
  ;; Compile as a call to the 'if primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  ;; IMPORTANT: For nested expressions, store as separate expressions and use form refs
  (let* ([test (car args)]
         [consequent (cadr args)]
         [alternate (if (null? (cddr args)) #f (caddr args))]
         [argc (if alternate 4 3)]  ; if + test + consequent [+ alternate]
         [if-enc (encode-symbol 'if bc)]
         ;; Compile test - if it's nested, store separately
         [test-enc (if (pair? test)
                       (let* ([nested (compile-expr test bc)]
                              [expr-id (length (bytecode-expressions bc))])
                         (add-expr bc nested)
                         (encode-form-ref expr-id))
                       (compile-expr test bc))]
         ;; Compile consequent - if it's nested, store separately
         [cons-enc (if (pair? consequent)
                       (let* ([nested (compile-expr consequent bc)]
                              [expr-id (length (bytecode-expressions bc))])
                         (add-expr bc nested)
                         (encode-form-ref expr-id))
                       (compile-expr consequent bc))]
         ;; Compile alternate - if it's nested, store separately
         [alt-enc (if alternate
                      (if (pair? alternate)
                          (let* ([nested (compile-expr alternate bc)]
                                 [expr-id (length (bytecode-expressions bc))])
                            (add-expr bc nested)
                            (encode-form-ref expr-id))
                          (compile-expr alternate bc))
                      #f)]
         [all-bytes (if alternate
                        (append (expr-encoding-data (encode-form-inline argc))
                                (expr-encoding-data if-enc)
                                (expr-encoding-data test-enc)
                                (expr-encoding-data cons-enc)
                                (expr-encoding-data alt-enc))
                        (append (expr-encoding-data (encode-form-inline argc))
                                (expr-encoding-data if-enc)
                                (expr-encoding-data test-enc)
                                (expr-encoding-data cons-enc)))])
    (expr-encoding 'form all-bytes)))

(define (compile-define args bc)
  ;; (define var expr) or (define (name args...) body...)
  ;; Compile as a call to the 'define primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  (let ([first-arg (car args)])
    (if (pair? first-arg)
        ;; Function definition: (define (name args...) body...)
        ;; Rewrite as: (define name (lambda (args...) body...))
        (let* ([name (car first-arg)]
               [params (cdr first-arg)]
               [body (cdr args)]
               [define-enc (encode-symbol 'define bc)]
               [name-enc (encode-symbol name bc)]
               [lambda-enc (compile-lambda params body bc)]
               [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                                  (expr-encoding-data define-enc)
                                  (expr-encoding-data name-enc)
                                  (expr-encoding-data lambda-enc))])
          (expr-encoding 'form all-bytes))
        ;; Variable definition: (define var expr)
        (let* ([var first-arg]
               [val (cadr args)]
               [define-enc (encode-symbol 'define bc)]
               [var-enc (encode-symbol var bc)]
               ;; If value is a nested expression (but not lambda), store separately and use form ref
               ;; Lambdas are special: compile-lambda already handles storing bind expressions
               [val-enc (if (and (pair? val) (not (eq? (car val) 'lambda)))
                           (let* ([nested (compile-expr val bc)]
                                  [expr-id (length (bytecode-expressions bc))])
                             (add-expr bc nested)
                             (encode-form-ref expr-id))
                           (compile-expr val bc))]
               [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                                  (expr-encoding-data define-enc)
                                  (expr-encoding-data var-enc)
                                  (expr-encoding-data val-enc))])
          (expr-encoding 'form all-bytes)))))

(define (compile-set var val bc)
  ;; (set! var val)
  ;; Compile as a call to the 'set! primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  (let* ([set-enc (encode-symbol 'set! bc)]
         [var-enc (encode-symbol var bc)]
         ;; If value is a nested expression (but not lambda), store separately and use form ref
         [val-enc (if (and (pair? val) (not (eq? (car val) 'lambda)))
                     (let* ([nested (compile-expr val bc)]
                            [expr-id (length (bytecode-expressions bc))])
                       (add-expr bc nested)
                       (encode-form-ref expr-id))
                     (compile-expr val bc))]
         [all-bytes (append (expr-encoding-data (encode-form-inline 3))
                            (expr-encoding-data set-enc)
                            (expr-encoding-data var-enc)
                            (expr-encoding-data val-enc))])
    (expr-encoding 'form all-bytes)))

(define (compile-begin exprs bc)
  ;; (begin expr1 expr2 ... exprN)
  ;; Compile as a call to the 'begin primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  ;; IMPORTANT: For nested expressions, store as separate expressions and use form refs
  (let* ([argc (+ 1 (length exprs))]  ; begin symbol + expressions
         [begin-enc (encode-symbol 'begin bc)]
         ;; Handle nested expressions - store them separately and use form refs
         [expr-encs (map (lambda (e)
                           (if (pair? e)
                               (let* ([nested (compile-expr e bc)]
                                      [expr-id (length (bytecode-expressions bc))])
                                 (add-expr bc nested)
                                 (encode-form-ref expr-id))
                               (compile-expr e bc)))
                         exprs)]
         ;; Concatenate: FORM FIRST, then begin, then exprs
         [all-bytes (append (expr-encoding-data (encode-form-inline argc))
                            (expr-encoding-data begin-enc)
                            (apply append (map expr-encoding-data expr-encs)))])
    (expr-encoding 'form all-bytes)))

(define (compile-and exprs bc)
  ;; (and expr1 expr2 ... exprN)
  ;; Compile as a call to the 'and primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  ;; IMPORTANT: For nested expressions, store as separate expressions and use form refs
  (let* ([argc (+ 1 (length exprs))]  ; and symbol + expressions
         [and-enc (encode-symbol 'and bc)]
         ;; Handle nested expressions
         [expr-encs (map (lambda (e)
                           (if (pair? e)
                               (let* ([nested (compile-expr e bc)]
                                      [expr-id (length (bytecode-expressions bc))])
                                 (add-expr bc nested)
                                 (encode-form-ref expr-id))
                               (compile-expr e bc)))
                         exprs)]
         [all-bytes (append (expr-encoding-data (encode-form-inline argc))
                            (expr-encoding-data and-enc)
                            (apply append (map expr-encoding-data expr-encs)))])
    (expr-encoding 'form all-bytes)))

(define (compile-or exprs bc)
  ;; (or expr1 expr2 ... exprN)
  ;; Compile as a call to the 'or primitive
  ;; IMPORTANT: Inline form comes FIRST, then operator, then arguments
  ;; IMPORTANT: For nested expressions, store as separate expressions and use form refs
  (let* ([argc (+ 1 (length exprs))]  ; or symbol + expressions
         [or-enc (encode-symbol 'or bc)]
         ;; Handle nested expressions
         [expr-encs (map (lambda (e)
                           (if (pair? e)
                               (let* ([nested (compile-expr e bc)]
                                      [expr-id (length (bytecode-expressions bc))])
                                 (add-expr bc nested)
                                 (encode-form-ref expr-id))
                               (compile-expr e bc)))
                         exprs)]
         [all-bytes (append (expr-encoding-data (encode-form-inline argc))
                            (expr-encoding-data or-enc)
                            (apply append (map expr-encoding-data expr-encs)))])
    (expr-encoding 'form all-bytes)))

(define (compile-application expr bc)
  ;; Function application: (func arg1 arg2 ...)
  ;; IMPORTANT: Inline form comes FIRST, then operator and arguments!
  ;; For nested expressions (non-atoms), use form references instead of embedding
  (let* ([func (car expr)]
         [args (cdr expr)]
         [total-count (+ 1 (length args))]  ; func + args
         ;; Compile function (usually a symbol, so it's an atom)
         [func-enc (compile-expr func bc)]
         ;; For each argument, check if it's a nested expression
         [arg-encs (map (lambda (arg)
                          (if (and (pair? arg) (not (eq? (car arg) 'lambda)))
                              ;; Nested expression (but not lambda): compile it, add to table, return form ref
                              ;; Lambdas are special: compile-lambda already handles storing bind expressions
                              (let* ([nested-enc (compile-expr arg bc)]
                                     [expr-id (length (bytecode-expressions bc))])
                                (add-expr bc nested-enc)  ; Add to table
                                (encode-form-ref expr-id)) ; Return form reference
                              ;; Atom or lambda: compile normally (lambda returns its own form encoding)
                              (compile-expr arg bc)))
                        args)]
         ;; Concatenate: FORM FIRST, then func, then args
         [all-bytes (append (expr-encoding-data (encode-form-inline total-count))
                            (expr-encoding-data func-enc)
                            (apply append (map expr-encoding-data arg-encs)))])
    ;; Return a single expression encoding with all bytes
    (expr-encoding 'form all-bytes)))

;; ============================================================================
;; Program Compilation
;; ============================================================================

(define (compile-program exprs)
  (define bc (make-bytecode))
  (for ([expr exprs])
    (compile-expr expr bc))
  bc)

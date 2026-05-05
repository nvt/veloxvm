#lang racket

;; VeloxVM Racket Compiler - Expression Rewriter
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Implements rewrite rules similar to scheme-lib.lisp
;; Transforms R5RS convenience forms into VM primitives

(provide rewrite-expr
         define-rewriter
         rewriters
         finalize-guard)

;; Rewriter registry: symbol -> (expr -> expr)
(define rewriters (make-hash))

;; Define a rewrite rule
(define-syntax-rule (define-rewriter (name expr) body ...)
  (hash-set! rewriters 'name
    (lambda (expr) body ...)))

;; Apply rewriters recursively
(define (rewrite-expr expr)
  (cond
    ;; Atom: no rewriting
    [(not (pair? expr)) expr]

    ;; Special handling for lambda: don't rewrite parameter list
    ;; (lambda params body...) - params should not be rewritten
    [(and (symbol? (car expr)) (eq? (car expr) 'lambda) (>= (length expr) 3))
     (let ([params (cadr expr)]
           [body (cddr expr)])
       `(lambda ,params ,@(map rewrite-expr body)))]

    ;; Special handling for define with lambda shorthand
    ;; (define (name params...) body...) - name and params should not be rewritten
    [(and (symbol? (car expr)) (eq? (car expr) 'define) (pair? (cadr expr)))
     (let ([name-and-params (cadr expr)]
           [body (cddr expr)])
       `(define ,name-and-params ,@(map rewrite-expr body)))]

    ;; Check for rewriter
    [(and (symbol? (car expr)) (hash-has-key? rewriters (car expr)))
     (let ([rewriter (hash-ref rewriters (car expr))])
       (rewrite-expr (rewriter expr)))]

    ;; Recursively rewrite list elements
    [(list? expr)
     (map rewrite-expr expr)]

    ;; Improper list
    [(pair? expr)
     (cons (rewrite-expr (car expr))
           (rewrite-expr (cdr expr)))]

    [else expr]))

;; Post-processing: Convert vm-guard back to guard for VM
;; The guard rewriter uses vm-guard internally to avoid infinite recursion
(define (finalize-guard expr)
  (cond
    ;; Atom: no change
    [(not (pair? expr)) expr]

    ;; Convert vm-guard to guard
    [(and (symbol? (car expr)) (eq? (car expr) 'vm-guard))
     `(guard ,@(map finalize-guard (cdr expr)))]

    ;; Recursively process list elements
    [(list? expr)
     (map finalize-guard expr)]

    ;; Improper list
    [(pair? expr)
     (cons (finalize-guard (car expr))
           (finalize-guard (cdr expr)))]

    [else expr]))

;; ============================================================================
;; Rewrite Rules
;; ============================================================================

;; Numerical predicates
(define-rewriter (positive? expr)
  `(> ,(cadr expr) 0))

(define-rewriter (negative? expr)
  `(< ,(cadr expr) 0))

(define-rewriter (odd? expr)
  `(= (remainder ,(cadr expr) 2) 1))

(define-rewriter (even? expr)
  `(= (remainder ,(cadr expr) 2) 0))

;; Absolute value
(define-rewriter (abs expr)
  (let ([x (cadr expr)])
    `(if (< ,x 0) (- ,x) ,x)))

;; Max/min (binary for now)
(define-rewriter (max expr)
  (let ([a (cadr expr)]
        [b (caddr expr)])
    `(if (> ,a ,b) ,a ,b)))

(define-rewriter (min expr)
  (let ([a (cadr expr)]
        [b (caddr expr)])
    `(if (< ,a ,b) ,a ,b)))

;; I/O
(define-rewriter (newline expr)
  (if (null? (cdr expr))
      `(write-char #\newline)
      `(write-char #\newline ,(cadr expr))))

;; No rewrite for display: it must reach the VM's display operator so
;; output flows through the port machinery, which is how the REPL
;; routes application output into IO_OUT frames.

(define-rewriter (println expr)
  `(begin
     (display ,@(cdr expr))
     (newline)))

;; Composite car/cdr (all 28 combinations: c[ad]{2,4}r)

;; 2-level compositions
(define-rewriter (caar expr)
  `(car (car ,(cadr expr))))

(define-rewriter (cadr expr)
  `(car (cdr ,(cadr expr))))

(define-rewriter (cdar expr)
  `(cdr (car ,(cadr expr))))

(define-rewriter (cddr expr)
  `(cdr (cdr ,(cadr expr))))

;; 3-level compositions
(define-rewriter (caaar expr)
  `(car (car (car ,(cadr expr)))))

(define-rewriter (caadr expr)
  `(car (car (cdr ,(cadr expr)))))

(define-rewriter (cadar expr)
  `(car (cdr (car ,(cadr expr)))))

(define-rewriter (caddr expr)
  `(car (cdr (cdr ,(cadr expr)))))

(define-rewriter (cdaar expr)
  `(cdr (car (car ,(cadr expr)))))

(define-rewriter (cdadr expr)
  `(cdr (car (cdr ,(cadr expr)))))

(define-rewriter (cddar expr)
  `(cdr (cdr (car ,(cadr expr)))))

(define-rewriter (cdddr expr)
  `(cdr (cdr (cdr ,(cadr expr)))))

;; 4-level compositions
(define-rewriter (caaaar expr)
  `(car (car (car (car ,(cadr expr))))))

(define-rewriter (caaadr expr)
  `(car (car (car (cdr ,(cadr expr))))))

(define-rewriter (caadar expr)
  `(car (car (cdr (car ,(cadr expr))))))

(define-rewriter (caaddr expr)
  `(car (car (cdr (cdr ,(cadr expr))))))

(define-rewriter (cadaar expr)
  `(car (cdr (car (car ,(cadr expr))))))

(define-rewriter (cadadr expr)
  `(car (cdr (car (cdr ,(cadr expr))))))

(define-rewriter (caddar expr)
  `(car (cdr (cdr (car ,(cadr expr))))))

(define-rewriter (cadddr expr)
  `(car (cdr (cdr (cdr ,(cadr expr))))))

(define-rewriter (cdaaar expr)
  `(cdr (car (car (car ,(cadr expr))))))

(define-rewriter (cdaadr expr)
  `(cdr (car (car (cdr ,(cadr expr))))))

(define-rewriter (cdadar expr)
  `(cdr (car (cdr (car ,(cadr expr))))))

(define-rewriter (cdaddr expr)
  `(cdr (car (cdr (cdr ,(cadr expr))))))

(define-rewriter (cddaar expr)
  `(cdr (cdr (car (car ,(cadr expr))))))

(define-rewriter (cddadr expr)
  `(cdr (cdr (car (cdr ,(cadr expr))))))

(define-rewriter (cdddar expr)
  `(cdr (cdr (cdr (car ,(cadr expr))))))

(define-rewriter (cddddr expr)
  `(cdr (cdr (cdr (cdr ,(cadr expr))))))

;; Character operations
(define-rewriter (char=? expr)
  `(= (char-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (char<? expr)
  `(< (char-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (char>? expr)
  `(> (char-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (char<=? expr)
  `(<= (char-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (char>=? expr)
  `(>= (char-compare ,(cadr expr) ,(caddr expr)) 0))

;; String operations
(define-rewriter (string=? expr)
  `(= (string-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (string<? expr)
  `(< (string-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (string>? expr)
  `(> (string-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (string<=? expr)
  `(<= (string-compare ,(cadr expr) ,(caddr expr)) 0))

(define-rewriter (string>=? expr)
  `(>= (string-compare ,(cadr expr) ,(caddr expr)) 0))

;; ============================================================================
;; Derived Forms
;; ============================================================================

;; cond: Transform into nested if expressions
(define-rewriter (cond expr)
  (define (rewrite-rule rule)
    (cond
      [(< (length rule) 2)
       (error 'rewrite-cond "Invalid COND rule: ~a" rule)]
      [(eq? (car rule) 'else)
       ;; (else expr1 expr2...) => (begin expr1 expr2...)
       (if (= (length (cdr rule)) 1)
           (cadr rule)
           `(begin ,@(cdr rule)))]
      [else
       ;; (test expr1 expr2...) => (if test (begin expr1 expr2...))
       (let ([condition (car rule)]
             [actions (cdr rule)])
         (if (= (length actions) 1)
             `(if ,condition ,(car actions))
             `(if ,condition (begin ,@actions))))]))

  (define (merge-rules rules)
    (if (null? rules)
        #f  ; No else clause
        (let ([first-if (rewrite-rule (car rules))]
              [rest (cdr rules)])
          (if (null? rest)
              first-if
              ;; Merge: (if test action) becomes (if test action (merge rest))
              (if (and (pair? first-if) (eq? (car first-if) 'if))
                  (if (= (length first-if) 3)
                      ;; No else clause yet, add merged rest
                      `(if ,(cadr first-if) ,(caddr first-if) ,(merge-rules rest))
                      ;; Already has else (shouldn't happen with proper cond)
                      first-if)
                  ;; Not an if (was else clause)
                  first-if)))))

  (merge-rules (cdr expr)))

;; case: Transform into let + cond with memv
(define-rewriter (case expr)
  (let ([key-expr (cadr expr)]
        [clauses (cddr expr)])
    (define temp-var (gensym '$case))
    (define (make-cond-clause clause)
      (if (eq? (car clause) 'else)
          clause
          (let ([datums (car clause)]
                [actions (cdr clause)])
            `((memv ,temp-var (list ,@datums)) ,@actions))))
    `(let ((,temp-var ,key-expr))
       (cond ,@(map make-cond-clause clauses)))))

;; let: Transform into lambda application
;; Supports both regular let and named let (R5RS)
;; Regular: (let ((var val)...) body...)
;; Named:   (let name ((var val)...) body...)
(define-rewriter (let expr)
  (let ([second (cadr expr)])
    (if (symbol? second)
        ;; Named let: (let name ((var val)...) body...)
        ;; Transform to: (letrec ((name (lambda (var...) body...))) (name val...))
        (let* ([name second]
               [bindings (caddr expr)]
               [body (cdddr expr)]
               [vars (map car bindings)]
               [vals (map cadr bindings)])
          `(letrec ((,name (lambda ,vars ,@body)))
             (,name ,@vals)))
        ;; Regular let
        (let ([bindings second]
              [body (cddr expr)])
          (if (null? bindings)
              ;; (let () body...) => (begin body...)
              (if (= (length body) 1)
                  (car body)
                  `(begin ,@body))
              ;; (let ((var val)...) body...) => ((lambda (var...) body...) val...)
              (let ([vars (map car bindings)]
                    [vals (map cadr bindings)])
                `((lambda ,vars ,@body) ,@vals)))))))

;; let*: Transform into nested lets
(define-rewriter (let* expr)
  (let ([bindings (cadr expr)]
        [body (cddr expr)])
    (if (null? bindings)
        ;; (let* () body...) => (begin body...)
        (if (= (length body) 1)
            (car body)
            `(begin ,@body))
        ;; (let* ((var val) rest...) body...)
        ;; => (let ((var val)) (let* (rest...) body...))
        (let ([first-binding (car bindings)]
              [rest-bindings (cdr bindings)])
          (if (null? rest-bindings)
              ;; Last binding
              `(let (,first-binding) ,@body)
              ;; More bindings
              `(let (,first-binding) (let* ,rest-bindings ,@body)))))))

;; letrec: Bind mutually recursive functions
;; R5RS semantics: variables bound first (to undefined), then initialized
;; (letrec ((var val)...) body...)
;; => (let ((var #f)...) (set! var val)... body...)
(define-rewriter (letrec expr)
  (let ([bindings (cadr expr)]
        [body (cddr expr)])
    (if (null? bindings)
        ;; (letrec () body...) => (begin body...)
        (if (= (length body) 1)
            (car body)
            `(begin ,@body))
        ;; (letrec ((var val)...) body...)
        ;; => (let ((var #f)...) (set! var val)... body...)
        (let ([vars (map car bindings)]
              [vals (map cadr bindings)])
          `(let ,(map (lambda (v) `(,v #f)) vars)
             ,@(map (lambda (v val) `(set! ,v ,val)) vars vals)
             ,@body)))))

;; do: Transform into named let with recursion
(define-rewriter (do expr)
  (let* ([var-clauses (cadr expr)]
         [test-clause (caddr expr)]
         [commands (cdddr expr)]
         [loop-name (gensym '$do-loop)]
         [var-names (map car var-clauses)]
         [var-inits (map cadr var-clauses)]
         [var-steps (map (lambda (clause)
                          (if (>= (length clause) 3)
                              (caddr clause)
                              (car clause)))  ; No step => use var itself
                        var-clauses)]
         [test-expr (car test-clause)]
         [result-exprs (cdr test-clause)])
    `(begin
       (define ,loop-name
         (lambda ,var-names
           (if ,test-expr
               ,(if (null? result-exprs)
                    #f  ; No result exprs
                    (if (= (length result-exprs) 1)
                        (car result-exprs)
                        `(begin ,@result-exprs)))
               (begin
                 ,@commands
                 (,loop-name ,@var-steps)))))
       (,loop-name ,@var-inits))))

;; Per-expansion counter to give nested dynamic-winds unique formal
;; names. Plain string-based names instead of gensym to keep the
;; symbols interned through the bytecode pipeline.
(define dw-counter (box 0))
(define (dw-fresh prefix)
  (let ([n (unbox dw-counter)])
    (set-box! dw-counter (+ n 1))
    (string->symbol (string-append prefix (number->string n)))))

;; dynamic-wind: wrap the body in a guard so the after-thunk runs on
;; both normal return and exception unwinding.
;;
;; Expansion:
;;   (dynamic-wind before thunk after)
;;     ↦
;;   (let* ((b before) (t thunk) (a after))
;;     (b)
;;     (guard (exc (else (a) (raise exc)))
;;       (let ((r (t)))
;;         (a)
;;         r)))
;;
;; The primitive at ID 191 is unused; the rewriter consumes the form
;; before encode-symbol reaches it.
(define-rewriter (dynamic-wind expr)
  (unless (= (length expr) 4)
    (error 'dynamic-wind
           "expects exactly 3 arguments (before thunk after); got: ~a"
           expr))
  (let ([before (cadr expr)]
        [thunk (caddr expr)]
        [after (cadddr expr)]
        [b-name (dw-fresh "dw-before-")]
        [t-name (dw-fresh "dw-thunk-")]
        [a-name (dw-fresh "dw-after-")]
        [r-name (dw-fresh "dw-result-")]
        [e-name (dw-fresh "dw-exc-")])
    `(let* ((,b-name ,before)
            (,t-name ,thunk)
            (,a-name ,after))
       (,b-name)
       (guard (,e-name (else (,a-name) (raise ,e-name)))
         (let ((,r-name (,t-name)))
           (,a-name)
           ,r-name)))))

;; parameterize: dynamic binding of parameter procedures.
;;
;; Expansion:
;;   (parameterize ((p1 v1) (p2 v2) ...) body...)
;;     ↦
;;   (let* ((p1' p1) (v1' v1) ... (old1 (p1')) ...)
;;     (dynamic-wind
;;       (lambda () (p1' v1') ...)
;;       (lambda () body...)
;;       (lambda () (p1' old1) ...)))
;;
;; Each parameter is read once via let* into a per-binding name, the
;; new values are installed before the body, and the old values are
;; restored on exit. The dynamic-wind wrapper carries restoration
;; through both normal return and exception unwinding.
(define-rewriter (parameterize expr)
  (let* ([bindings (cadr expr)]
         [body (cddr expr)]
         [params (map car bindings)]
         [vals (map cadr bindings)]
         [param-names (map (lambda (_) (dw-fresh "param-")) params)]
         [val-names   (map (lambda (_) (dw-fresh "val-"))   vals)]
         [old-names   (map (lambda (_) (dw-fresh "old-"))   params)]
         [param-bindings (map list param-names params)]
         [val-bindings   (map list val-names vals)]
         [old-bindings
           (map (lambda (o p) (list o (list p))) old-names param-names)]
         [install-calls
           (map (lambda (p v) (list p v)) param-names val-names)]
         [restore-calls
           (map (lambda (p o) (list p o)) param-names old-names)])
    `(let* ,(append param-bindings val-bindings old-bindings)
       (dynamic-wind
         (lambda () ,@install-calls)
         (lambda () ,@body)
         (lambda () ,@restore-calls)))))

;; case-lambda: R7RS variable-arity dispatch (R7RS §4.2.9).
;; Syntax:
;;   (case-lambda (formals1 body1) (formals2 body2) ...)
;; Returns a procedure that dispatches to the first clause whose
;; formals shape matches the actual argument count. Each clause has
;; the same formal-spec syntax as lambda: a proper list of symbols, a
;; dotted-pair (fixed + rest), or a bare symbol (all-args catch-all).
;;
;; Expansion strategy: build one outer (lambda args ...) that captures
;; all actuals into a list, then chain a sequence of `if` checks over
;; the clauses. Each clause emits an arity test and a body wrapped in
;; a let-binding that pulls per-formal values out of the args list.
(define-rewriter (case-lambda expr)
  (let ([clauses (cdr expr)])
    (when (null? clauses)
      (error 'case-lambda "case-lambda requires at least one clause"))
    (let ([dispatch
           (let recur ([cs clauses])
             (if (null? cs)
                 ;; All clauses missed: signal a runtime error so the
                 ;; caller sees a clear failure rather than #<unspecified>.
                 `(error "case-lambda: no clause matches argument count")
                 (let* ([clause (car cs)]
                        [formals (car clause)]
                        [body (cdr clause)])
                   (case-lambda-clause formals body (recur (cdr cs))))))])
      `(lambda args ,dispatch))))

;; Build one branch for a single case-lambda clause. Returns an
;; expression that either evaluates the clause body (when the arity
;; matches) or evaluates `else-expr` (the chained dispatch for
;; subsequent clauses).
(define (case-lambda-clause formals body else-expr)
  (cond
    ;; Bare symbol: catch-all that binds the entire args list.
    [(symbol? formals)
     `(let ((,formals args)) ,@body)]
    ;; Proper list: arity must match exactly.
    [(case-lambda-proper-list? formals)
     (let* ([n (length formals)]
            [bindings (case-lambda-proper-bindings formals 0)])
       `(if (= (length args) ,n)
            (let ,bindings ,@body)
            ,else-expr))]
    ;; Dotted-pair: at least M fixed args, rest gets the tail.
    [(pair? formals)
     (let* ([fixed (case-lambda-fixed-formals formals)]
            [rest (case-lambda-rest-formal formals)]
            [m (length fixed)]
            [fixed-bindings (case-lambda-proper-bindings fixed 0)]
            [rest-binding `(,rest ,(case-lambda-drop-expr 'args m))])
       `(if (>= (length args) ,m)
            (let ,(append fixed-bindings (list rest-binding))
              ,@body)
            ,else-expr))]
    [else
     (error 'case-lambda
            "malformed formal-parameter list: ~a" formals)]))

;; True iff `xs` is a proper list of symbols.
(define (case-lambda-proper-list? xs)
  (cond
    [(null? xs) #t]
    [(pair? xs)
     (and (symbol? (car xs))
          (case-lambda-proper-list? (cdr xs)))]
    [else #f]))

;; Returns a list of let-bindings for each formal, pulling the i-th
;; element out of the `args` list via car/cdr composition starting at
;; offset.
(define (case-lambda-proper-bindings formals offset)
  (if (null? formals)
      '()
      (cons `(,(car formals) ,(case-lambda-nth-expr 'args offset))
            (case-lambda-proper-bindings (cdr formals) (+ offset 1)))))

;; (case-lambda-nth-expr 'args 0) => (car args)
;; (case-lambda-nth-expr 'args 1) => (car (cdr args))
;; (case-lambda-nth-expr 'args n) => (car (cdr ... (cdr args)))
(define (case-lambda-nth-expr lst n)
  (if (= n 0)
      `(car ,lst)
      `(car ,(case-lambda-drop-expr lst n))))

;; (case-lambda-drop-expr 'args 0) => args
;; (case-lambda-drop-expr 'args n) => (cdr (cdr ... (cdr args)))
(define (case-lambda-drop-expr lst n)
  (if (= n 0)
      lst
      `(cdr ,(case-lambda-drop-expr lst (- n 1)))))

;; Walk a dotted-pair formal-spec and return the proper-list prefix.
(define (case-lambda-fixed-formals formals)
  (cond
    [(null? formals) '()]
    [(symbol? formals) '()]
    [(pair? formals)
     (cons (car formals) (case-lambda-fixed-formals (cdr formals)))]
    [else
     (error 'case-lambda
            "malformed formal-parameter list: ~a" formals)]))

;; Walk a dotted-pair formal-spec and return the rest-symbol tail.
(define (case-lambda-rest-formal formals)
  (cond
    [(null? formals) #f]
    [(symbol? formals) formals]
    [(pair? formals) (case-lambda-rest-formal (cdr formals))]
    [else #f]))

;; define-record-type: R7RS records (R7RS §5.5).
;; Syntax:
;;   (define-record-type <type-name>
;;     (<constructor> <param>...)
;;     <predicate>
;;     (<field> <accessor> [<mutator>])...)
;;
;; Expands to a (begin) of top-level definitions: a tag symbol bound
;; to the type-name, the constructor (a vector with the tag in slot 0
;; and field values starting at slot 1), the predicate (vector? +
;; tag-eq?), and one accessor (and optional mutator) per declared
;; field. Field-slot order follows declaration order in the
;; field-spec list. Constructor params that do not appear among the
;; field-names are rejected; fields not mentioned in the constructor
;; receive an unspecified initial value (encoded as #f, matching
;; pyvelox's None convention) and are typically set via the field's
;; mutator before first read.
(define-rewriter (define-record-type expr)
  (let* ([type-name (cadr expr)]
         [constructor-spec (caddr expr)]
         [constructor-name (car constructor-spec)]
         [constructor-params (cdr constructor-spec)]
         [predicate-name (cadddr expr)]
         [field-specs (cddddr expr)]
         [field-names (map car field-specs)]
         [tag-name (string->symbol
                    (string-append "*record-tag-"
                                   (symbol->string type-name)
                                   "*"))])
    ;; Reject constructor params that name nonexistent fields.
    (for ([p (in-list constructor-params)])
      (unless (member p field-names)
        (error 'define-record-type
               "constructor parameter ~a is not a declared field of ~a"
               p type-name)))
    ;; For each field-name, the constructor body picks either the
    ;; matching param or #f if the field isn't in the constructor.
    (let* ([slot-values
             (map (lambda (f)
                    (if (member f constructor-params) f #f))
                  field-names)]
           [accessor-mutator-defs
             (apply append
               (map (lambda (spec idx)
                      (let ([accessor-name (cadr spec)]
                            [mutator-name
                              (if (null? (cddr spec))
                                  #f
                                  (caddr spec))])
                        (if mutator-name
                            (list
                              `(define (,accessor-name r) (vector-ref r ,idx))
                              `(define (,mutator-name r v)
                                 (vector-set! r ,idx v)))
                            (list
                              `(define (,accessor-name r) (vector-ref r ,idx))))))
                    field-specs
                    (build-list (length field-specs) (lambda (i) (+ i 1)))))])
      `(begin
         (define ,tag-name ',type-name)
         (define (,constructor-name ,@constructor-params)
           (vector ,tag-name ,@slot-values))
         (define (,predicate-name obj)
           (and (vector? obj)
                (eq? (vector-ref obj 0) ,tag-name)))
         ,@accessor-mutator-defs))))

;; when: (when test expr...) => (if test (begin expr...))
(define-rewriter (when expr)
  (let ([test (cadr expr)]
        [actions (cddr expr)])
    (if (= (length actions) 1)
        `(if ,test ,(car actions))
        `(if ,test (begin ,@actions)))))

;; unless: (unless test expr...) => (if (not test) (begin expr...))
(define-rewriter (unless expr)
  (let ([test (cadr expr)]
        [actions (cddr expr)])
    (if (= (length actions) 1)
        `(if (not ,test) ,(car actions))
        `(if (not ,test) (begin ,@actions)))))

;; ============================================================================
;; Exception Handling
;; ============================================================================

;; guard: R5RS exception handling
;; R5RS syntax: (guard (var clause...) body...)
;; VM expects: (guard var (cond clause...) (begin body...))
;; Uses 'vm-guard internally to avoid infinite rewriting, converted back to 'guard later
;; NOTE: Full R6RS/R7RS compliance (as of 2025-12-15):
;;       - Exception propagation from handler clauses works correctly
;;       - Automatic re-raising when no else clause (implicit else added)
(define-rewriter (guard expr)
  ;; Transform R5RS format to VM format using temporary 'vm-guard symbol
  ;; This avoids infinite recursion since vm-guard won't trigger this rewriter
  (let* ([exception-var (car (cadr expr))]     ; Extract exception variable
         [handler-clauses (cdr (cadr expr))]   ; Extract handler clauses
         [body-exprs (cddr expr)]              ; Extract body expressions
         ;; Check if there's an else clause (R6RS/R7RS compliance)
         [has-else? (and (pair? handler-clauses)
                         (ormap (lambda (clause)
                                  (and (pair? clause)
                                       (eq? (car clause) 'else)))
                                handler-clauses))]
         ;; If no else clause, add implicit re-raise (R6RS/R7RS requirement)
         ;; "If every cond clause's test evaluates to #f and there is no else
         ;;  clause, then raise is re-invoked on the raised object"
         [final-clauses (if has-else?
                            handler-clauses
                            (append handler-clauses
                                    `((else (raise ,exception-var)))))])
    `(vm-guard ,exception-var
               (cond ,@final-clauses)
               (begin ,@body-exprs))))

;; ============================================================================
;; Additional Predicates
;; ============================================================================

;; zero?: (zero? x) => (= x 0)
(define-rewriter (zero? expr)
  `(= ,(cadr expr) 0))

;; list?: Check if argument is a proper list
;; NOTE: Removed broken rewriter that used (or (null? x) (pair? x))
;; This was incorrect because improper pairs like (cons 1 2) are pairs but NOT lists.
;; Now calls the VM primitive which correctly checks the VM_LIST_FLAG_PAIR flag.
;; (define-rewriter (list? expr)
;;   (let ([x (cadr expr)])
;;     `(or (null? ,x) (pair? ,x))))

;; call/cc: Abbreviation for call-with-current-continuation
(define-rewriter (call/cc expr)
  `(call-with-current-continuation ,@(cdr expr)))

;; ============================================================================
;; Character Classification Predicates
;; ============================================================================

;; Case-insensitive character comparisons
(define-rewriter (char-ci=? expr)
  `(= (char-compare (char-downcase ,(cadr expr))
                    (char-downcase ,(caddr expr))) 0))

(define-rewriter (char-ci<? expr)
  `(< (char-compare (char-downcase ,(cadr expr))
                    (char-downcase ,(caddr expr))) 0))

(define-rewriter (char-ci>? expr)
  `(> (char-compare (char-downcase ,(cadr expr))
                    (char-downcase ,(caddr expr))) 0))

(define-rewriter (char-ci<=? expr)
  `(<= (char-compare (char-downcase ,(cadr expr))
                     (char-downcase ,(caddr expr))) 0))

(define-rewriter (char-ci>=? expr)
  `(>= (char-compare (char-downcase ,(cadr expr))
                     (char-downcase ,(caddr expr))) 0))

;; Character classification (using char-class VM primitive)
(define-rewriter (char-alphabetic? expr)
  `(= (char-class ,(cadr expr)) 1))  ; Class 1 = alphabetic

(define-rewriter (char-numeric? expr)
  `(= (char-class ,(cadr expr)) 2))  ; Class 2 = numeric

(define-rewriter (char-whitespace? expr)
  `(= (char-class ,(cadr expr)) 3))  ; Class 3 = whitespace

(define-rewriter (char-upper-case? expr)
  `(= (char-class ,(cadr expr)) 4))  ; Class 4 = uppercase

(define-rewriter (char-lower-case? expr)
  `(= (char-class ,(cadr expr)) 5))  ; Class 5 = lowercase

;; ============================================================================
;; String Operations (Case-Insensitive)
;; ============================================================================

;; Case-insensitive string comparisons
;; Note: These would need VM support for string-downcase or similar
;; For now, we'll implement simplified versions
(define-rewriter (string-ci=? expr)
  `(string=? ,@(cdr expr)))  ; TODO: Add proper case-insensitive support

(define-rewriter (string-ci<? expr)
  `(string<? ,@(cdr expr)))  ; TODO: Add proper case-insensitive support

(define-rewriter (string-ci>? expr)
  `(string>? ,@(cdr expr)))  ; TODO: Add proper case-insensitive support

(define-rewriter (string-ci<=? expr)
  `(string<=? ,@(cdr expr)))  ; TODO: Add proper case-insensitive support

(define-rewriter (string-ci>=? expr)
  `(string>=? ,@(cdr expr)))  ; TODO: Add proper case-insensitive support

;; ============================================================================
;; Quasiquotation (Phase 3)
;; ============================================================================

;; Quasiquote expansion algorithm based on R5RS specification
;; Transforms quasiquote templates into combinations of quote, list, cons, append

(define-rewriter (quasiquote expr)
  (expand-quasiquote (cadr expr) 0))

;; Helper function to expand quasiquote expressions
;; expr: the expression to expand
;; depth: nesting depth of quasiquotes (0 = expand unquotes, >0 = keep them quoted)
(define (expand-quasiquote expr depth)
  (cond
    ;; Base case: atom or empty list -> quote it (unless it's self-evaluating)
    [(not (pair? expr))
     (if (or (null? expr) (symbol? expr))
         (list 'quote expr)
         expr)]  ; Self-evaluating (numbers, strings, booleans, chars)

    ;; Unquote: (unquote x) or ,x
    [(eq? (car expr) 'unquote)
     (if (zero? depth)
         (cadr expr)  ; Depth 0: evaluate the unquoted expression
         ;; Depth > 0: keep the unquote, but recurse with decreased depth
         (list 'list
               (list 'quote 'unquote)
               (expand-quasiquote (cadr expr) (- depth 1))))]

    ;; Nested quasiquote: increase depth
    [(eq? (car expr) 'quasiquote)
     (list 'list
           (list 'quote 'quasiquote)
           (expand-quasiquote (cadr expr) (+ depth 1)))]

    ;; Unquote-splicing at head of list: (unquote-splicing x) or ,@x
    [(and (pair? (car expr)) (eq? (caar expr) 'unquote-splicing))
     (if (zero? depth)
         ;; Depth 0: splice the list
         (if (null? (cdr expr))
             (cadar expr)  ; Just the spliced list (no tail)
             (list 'append (cadar expr) (expand-quasiquote (cdr expr) depth)))
         ;; Depth > 0: keep the unquote-splicing, but recurse with decreased depth
         (let ([head-expanded (list 'list
                                   (list 'quote 'unquote-splicing)
                                   (expand-quasiquote (cadar expr) (- depth 1)))]
               [tail-expanded (expand-quasiquote (cdr expr) depth)])
           (list 'cons head-expanded tail-expanded)))]

    ;; Regular list: recursively process head and tail
    [else
     (let ([head (expand-quasiquote (car expr) depth)]
           [tail (expand-quasiquote (cdr expr) depth)])
       ;; Optimize common cases to use list instead of cons chains
       (cond
         ;; Both quoted -> combine into single quote
         [(and (pair? head) (eq? (car head) 'quote)
               (pair? tail) (eq? (car tail) 'quote))
          (list 'quote (cons (cadr head) (cadr tail)))]

         ;; Tail is quoted empty list -> use list
         [(and (pair? tail) (eq? (car tail) 'quote) (null? (cadr tail)))
          (if (and (pair? head) (eq? (car head) 'quote))
              (list 'quote (list (cadr head)))  ; Both quoted -> quote the whole list
              (list 'list head))]

         ;; Tail is a list form -> extend it
         [(and (pair? tail) (eq? (car tail) 'list))
          (cons 'list (cons head (cdr tail)))]

         ;; Default: cons
         [else
          (list 'cons head tail)]))]))

#lang racket

;; VeloxVM Racket Compiler - Macro Expander
;; Implements pattern matching and template expansion for syntax-rules

(provide match-pattern
         expand-template
         make-syntax-rules-transformer
         pattern-variables
         ellipsis-variable?
         expand-macros
         process-define-syntax
         reset-macro-table!
         macro-table  ; Export for testing (legacy, now a function)
         *FILTERED*   ; Sentinel value for filtering out define-syntax
         ;; New scope management functions (for let-syntax/letrec-syntax)
         push-macro-scope!
         pop-macro-scope!
         lookup-macro
         add-macro!
         ;; let-syntax/letrec-syntax expansion
         expand-let-syntax
         expand-letrec-syntax)

;; ============================================================================
;; PATTERN MATCHING (Milestone 4.1)
;; ============================================================================

;; Match a pattern against input, returning bindings or #f
;; pattern: the pattern to match
;; input: the input expression
;; literals: list of literal symbols that must match exactly
;; Returns: hash table of bindings, or #f if no match
(define (match-pattern pattern input literals)
  (define bindings (make-hash))
  (if (match-pattern-helper pattern input literals bindings)
      bindings
      #f))

;; Helper function that does the actual matching
(define (match-pattern-helper pattern input literals bindings)
  (cond
    ;; Underscore: matches anything without binding
    [(eq? pattern '_) #t]

    ;; Literal: must match exactly
    [(and (symbol? pattern) (member pattern literals))
     (eq? pattern input)]

    ;; Ellipsis pattern: (pat ...)
    [(and (pair? pattern)
          (pair? (cdr pattern))
          (eq? (cadr pattern) '...))
     (match-ellipsis (car pattern) input literals bindings)]

    ;; Pattern variable: bind it
    [(symbol? pattern)
     (hash-set! bindings pattern input)
     #t]

    ;; Empty list
    [(and (null? pattern) (null? input))
     #t]

    ;; Pair pattern: match car and cdr
    [(and (pair? pattern) (pair? input))
     (and (match-pattern-helper (car pattern) (car input) literals bindings)
          (match-pattern-helper (cdr pattern) (cdr input) literals bindings))]

    ;; Exact match for atoms
    [(equal? pattern input) #t]

    ;; No match
    [else #f]))

;; Match ellipsis pattern against a list of inputs
;; pattern: the pattern before the ...
;; inputs: list of input expressions
;; literals: literal symbols
;; bindings: hash table to update with bindings
(define (match-ellipsis pattern inputs literals bindings)
  (cond
    ;; Inputs must be a proper list
    [(not (or (null? inputs) (list? inputs)))
     #f]

    [else
     ;; Get all pattern variables in the pattern
     (define pat-vars (pattern-variables pattern literals))

     ;; Initialize each pattern variable as an empty list
     (for ([var pat-vars])
       (hash-set! bindings var '()))

     ;; Match each input against the pattern
     (for/and ([input inputs])
       (define local-bindings (make-hash))
       (define matched? (match-pattern-helper pattern input literals local-bindings))

       ;; If matched, append to bindings
       (when matched?
         (for ([var pat-vars])
           (hash-update! bindings var
                        (lambda (lst) (append lst (list (hash-ref local-bindings var)))))))

       matched?)]))

;; Extract all pattern variables from a pattern (excluding literals)
(define (pattern-variables pattern literals)
  (cond
    ;; Literal: not a variable
    [(and (symbol? pattern) (member pattern literals))
     '()]

    ;; Underscore: not a variable
    [(eq? pattern '_)
     '()]

    ;; Symbol: it's a pattern variable
    [(symbol? pattern)
     (list pattern)]

    ;; Ellipsis pattern: recurse on the pattern before ...
    [(and (pair? pattern)
          (pair? (cdr pattern))
          (eq? (cadr pattern) '...))
     (pattern-variables (car pattern) literals)]

    ;; Pair: recurse on both parts
    [(pair? pattern)
     (append (pattern-variables (car pattern) literals)
             (pattern-variables (cdr pattern) literals))]

    ;; Atom: not a variable
    [else '()]))

;; Check if a variable is bound to a list (ellipsis-bound)
(define (ellipsis-variable? var bindings)
  (and (hash-has-key? bindings var)
       (list? (hash-ref bindings var))))

;; ============================================================================
;; TEMPLATE EXPANSION (Milestone 4.2)
;; ============================================================================

;; Expand a template using bindings from pattern matching
;; template: the template to expand
;; bindings: hash table of pattern variable bindings
;; Returns: expanded expression
(define (expand-template template bindings)
  (cond
    ;; Pattern variable: substitute with its binding
    [(and (symbol? template) (hash-has-key? bindings template))
     (hash-ref bindings template)]

    ;; Ellipsis template: (tmpl ... rest)
    [(and (pair? template)
          (pair? (cdr template))
          (eq? (cadr template) '...))
     (let ([expanded (expand-ellipsis-template (car template) bindings)]
           [rest (cddr template)])
       (if (null? rest)
           expanded
           (append expanded (expand-template rest bindings))))]

    ;; Empty list
    [(null? template)
     '()]

    ;; Pair: recurse on both parts
    [(pair? template)
     (cons (expand-template (car template) bindings)
           (expand-template (cdr template) bindings))]

    ;; Literal: return as-is
    [else template]))

;; Expand an ellipsis template
;; template: the template before the ...
;; bindings: hash table of bindings
;; Returns: list of expanded elements
(define (expand-ellipsis-template template bindings)
  ;; Find ellipsis variables in the template
  (define ellipsis-vars
    (filter (lambda (v) (ellipsis-variable? v bindings))
            (template-variables template)))

  (cond
    ;; No ellipsis variables: error or return empty
    [(null? ellipsis-vars)
     '()]

    [else
     ;; Check that all ellipsis variables have the same length
     (define lengths (map (lambda (v) (length (hash-ref bindings v))) ellipsis-vars))
     (unless (apply = lengths)
       (error 'expand-ellipsis-template "Ellipsis variables have different lengths: ~a" ellipsis-vars))

     ;; Iterate over the indices
     (define n (car lengths))
     (for/list ([i (in-range n)])
       ;; Create local bindings for this iteration
       (define local-bindings (hash-copy bindings))
       (for ([var ellipsis-vars])
         (hash-set! local-bindings var
                   (list-ref (hash-ref bindings var) i)))

       ;; Expand template with local bindings
       (expand-template template local-bindings))]))

;; Get all variables (symbols) from a template
(define (template-variables template)
  (cond
    [(symbol? template) (list template)]
    [(and (pair? template)
          (pair? (cdr template))
          (eq? (cadr template) '...))
     (template-variables (car template))]
    [(pair? template)
     (append (template-variables (car template))
             (template-variables (cdr template)))]
    [else '()]))

;; ============================================================================
;; SYNTAX-RULES TRANSFORMER (Milestone 4.3)
;; ============================================================================

;; Create a macro transformer from a syntax-rules specification
;; literals: list of literal symbols
;; rules: list of (pattern template) pairs
;; Returns: transformer function that takes a form and returns expanded form
(define (make-syntax-rules-transformer literals rules)
  (lambda (form)
    ;; Try each rule in order
    (for/or ([rule rules])
      (match-define (list pattern template) rule)

      ;; The macro name (first element of pattern) is implicitly a literal
      ;; Add it to the literals list for matching
      (define macro-name (if (pair? pattern) (car pattern) pattern))
      (define extended-literals (cons macro-name literals))

      ;; Try to match the pattern against the full form
      (define bindings (match-pattern pattern form extended-literals))

      (and bindings
           ;; Success: expand the template
           (expand-template template bindings)))))

;; ============================================================================
;; DEFINE-SYNTAX INTEGRATION (Milestone 4.4)
;; ============================================================================

;; Scope-based macro system
;; macro-scopes is a list of hash tables, with the innermost (most local) scope first
;; and the global scope last. Each hash maps macro-name -> transformer function.
(define macro-scopes (list (make-hash)))  ; Start with one global scope

;; Push a new macro scope (for let-syntax, letrec-syntax)
(define (push-macro-scope! bindings)
  (set! macro-scopes (cons bindings macro-scopes)))

;; Pop the current macro scope
(define (pop-macro-scope!)
  (when (null? (cdr macro-scopes))
    (error 'pop-macro-scope! "Cannot pop global scope"))
  (set! macro-scopes (cdr macro-scopes)))

;; Look up a macro in the scope chain (searches from innermost to outermost)
;; Returns: transformer function or #f if not found
(define (lookup-macro name)
  (for/or ([scope macro-scopes])
    (hash-ref scope name #f)))

;; Add a macro to the current (innermost) scope
(define (add-macro! name transformer)
  (hash-set! (car macro-scopes) name transformer))

;; Reset the macro system to single global scope (useful for testing)
(define (reset-macro-table!)
  (set! macro-scopes (list (make-hash))))

;; Legacy accessor for testing compatibility
(define (macro-table)
  (car macro-scopes))

;; Process a define-syntax form
;; name: the macro name (symbol)
;; spec: the macro specification (currently only syntax-rules supported)
(define (process-define-syntax name spec)
  (match spec
    [`(syntax-rules ,literals . ,rules)
     (add-macro! name (make-syntax-rules-transformer literals rules))]
    [else
     (error 'define-syntax "Only syntax-rules is supported, got: ~a" spec)]))

;; ============================================================================
;; LET-SYNTAX (Lexically-scoped macros)
;; ============================================================================

;; Expand a let-syntax form
;; (let-syntax ([name (syntax-rules ...)] ...) body ...)
;; Creates a new scope with local macros, expands body, then removes scope
(define (expand-let-syntax expr)
  (match expr
    [`(let-syntax ([,names ,specs] ...) ,body ...)
     ;; 1. Create new scope with local macros
     (define local-scope (make-hash))
     (for ([name names] [spec specs])
       (match spec
         [`(syntax-rules ,literals . ,rules)
          (hash-set! local-scope name
                     (make-syntax-rules-transformer literals rules))]
         [else
          (error 'let-syntax "Only syntax-rules is supported, got: ~a" spec)]))

     ;; 2. Push the local scope
     (push-macro-scope! local-scope)

     ;; 3. Expand body with local macros visible
     (define expanded-body (map expand-macros body))

     ;; 4. Pop the scope (local macros no longer visible)
     (pop-macro-scope!)

     ;; 5. Return expanded body
     ;; If single expression, return it directly; otherwise wrap in begin
     (cond
       [(null? expanded-body) '(begin)]  ; Empty body
       [(null? (cdr expanded-body)) (car expanded-body)]  ; Single expression
       [else `(begin ,@expanded-body)])]  ; Multiple expressions

    [else
     (error 'let-syntax "Invalid let-syntax form: ~a" expr)]))

;; ============================================================================
;; LETREC-SYNTAX (Recursive lexically-scoped macros)
;; ============================================================================

;; Expand a letrec-syntax form
;; (letrec-syntax ([name (syntax-rules ...)] ...) body ...)
;; Like let-syntax, but macros are visible to their own definitions,
;; enabling recursive and mutually-recursive macro definitions
(define (expand-letrec-syntax expr)
  (match expr
    [`(letrec-syntax ([,names ,specs] ...) ,body ...)
     ;; 1. Create new empty scope and push it first
     ;;    This makes the bindings visible during transformer creation
     (define local-scope (make-hash))
     (push-macro-scope! local-scope)

     ;; 2. Create transformers and add to current scope
     ;;    Since scope is already pushed, transformers can reference each other
     (for ([name names] [spec specs])
       (match spec
         [`(syntax-rules ,literals . ,rules)
          ;; Add to current scope using add-macro! (adds to innermost scope)
          (add-macro! name (make-syntax-rules-transformer literals rules))]
         [else
          ;; Pop scope before erroring to maintain invariant
          (pop-macro-scope!)
          (error 'letrec-syntax "Only syntax-rules is supported, got: ~a" spec)]))

     ;; 3. Expand body with recursive macros visible
     (define expanded-body (map expand-macros body))

     ;; 4. Pop the scope (local macros no longer visible)
     (pop-macro-scope!)

     ;; 5. Return expanded body
     ;; If single expression, return it directly; otherwise wrap in begin
     (cond
       [(null? expanded-body) '(begin)]  ; Empty body
       [(null? (cdr expanded-body)) (car expanded-body)]  ; Single expression
       [else `(begin ,@expanded-body)])]  ; Multiple expressions

    [else
     (error 'letrec-syntax "Invalid letrec-syntax form: ~a" expr)]))

;; Sentinel value to indicate "don't compile this" (used for define-syntax)
;; Using a gensym ensures it won't conflict with any actual Scheme value
(define *FILTERED* (gensym 'filtered))

;; Expand all macros in an expression recursively
;; expr: the expression to expand
;; Returns: the fully expanded expression, or *FILTERED* if it's a define-syntax (should not be compiled)
(define (expand-macros expr)
  (cond
    ;; define-syntax: register the macro and return *FILTERED* (don't compile it)
    [(and (pair? expr) (eq? (car expr) 'define-syntax))
     (process-define-syntax (cadr expr) (caddr expr))
     *FILTERED*]

    ;; let-syntax: expand with local macro scope
    [(and (pair? expr) (eq? (car expr) 'let-syntax))
     (expand-let-syntax expr)]

    ;; letrec-syntax: expand with recursive local macro scope
    [(and (pair? expr) (eq? (car expr) 'letrec-syntax))
     (expand-letrec-syntax expr)]

    ;; Quote: datum is not code, so it must not be expanded. Otherwise a
    ;; quoted symbol that happens to share a name with an in-scope macro
    ;; would be "expanded" as a bogus macro invocation.
    [(and (pair? expr) (eq? (car expr) 'quote))
     expr]

    ;; Macro invocation: look up in scope chain and expand
    [(and (pair? expr) (symbol? (car expr)))
     (define transformer (lookup-macro (car expr)))
     (if transformer
         ;; Found a macro - try to expand it
         (let ([expanded (transformer expr)])
           (if expanded
               ;; Transformer matched a rule — recurse on the expansion
               (expand-macros expanded)
               ;; No rule matched. This is a use of a name that happens
               ;; to be a macro but doesn't match any pattern (e.g. the
               ;; bare (foo) when foo expects (foo x)). Raise rather than
               ;; silently substituting #f.
               (error 'expand-macros
                      "no syntax-rules clause matched for: ~a" expr)))
         ;; Not a macro - recurse on car and cdr
         (let ([car-expanded (expand-macros (car expr))]
               [cdr-expanded (expand-macros (cdr expr))])
           ;; If either part is *FILTERED* (from define-syntax), filter it out
           (cond
             [(and (eq? car-expanded *FILTERED*) (eq? cdr-expanded *FILTERED*)) *FILTERED*]
             [(eq? car-expanded *FILTERED*) cdr-expanded]
             [(eq? cdr-expanded *FILTERED*) (list car-expanded)]
             [else (cons car-expanded cdr-expanded)])))]

    ;; Pair (non-symbol car): recurse on car and cdr
    [(pair? expr)
     (let ([car-expanded (expand-macros (car expr))]
           [cdr-expanded (expand-macros (cdr expr))])
       ;; If either part is *FILTERED* (from define-syntax), filter it out
       (cond
         [(and (eq? car-expanded *FILTERED*) (eq? cdr-expanded *FILTERED*)) *FILTERED*]
         [(eq? car-expanded *FILTERED*) cdr-expanded]
         [(eq? cdr-expanded *FILTERED*) (list car-expanded)]
         [else (cons car-expanded cdr-expanded)]))]

    ;; Atom: return as-is
    [else expr]))

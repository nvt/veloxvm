#lang racket

;; VeloxVM Racket Compiler - S-Expression Reader
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Supports R7RS 'include' for compile-time file inclusion

(provide read-all-exprs
         read-expr)

;; Read all expressions from a string or port
;; source-file: optional path to source file (for resolving relative includes)
;; Returns: (listof s-expr)
(define (read-all-exprs input [source-file #f])
  (read-all-exprs-internal input source-file '()))

;; Internal helper with circular include detection. Reverse before
;; splicing includes -- if we did it after, the splice's content would
;; come out in the order it was read, but the surrounding forms would
;; have just been reversed, so the included definitions would end up
;; before earlier source lines.
(define (read-all-exprs-internal input source-file included-files)
  (define in (if (string? input)
                 (open-input-string input)
                 input))
  (let loop ([exprs '()])
    (define expr (read in))
    (if (eof-object? expr)
        (process-includes (reverse exprs) source-file included-files)
        (loop (cons expr exprs)))))

;; Heads whose sub-forms are data, not expressions, and so must not be
;; descended into when scanning for nested (include …) forms. quote and
;; quasiquote protect literal data; the syntax-binding forms hold
;; pattern/template lists that look like expressions but are matched, not
;; evaluated. (Quasiquote with embedded unquote-include is a corner case
;; we don't attempt to support.)
(define (skip-include-walk? head)
  (and (symbol? head)
       (memq head '(quote quasiquote
                          define-syntax let-syntax letrec-syntax
                          syntax-rules))))

;; Walk the children of a form via cons-based traversal so the walker
;; copes with improper lists (e.g. the (FUNC . REST) formals in
;; (define (FUNC . REST) BODY ...)). Splices any include directly under
;; this form and recurses into nested pairs.
(define (process-form-children children source-file included-files)
  (cond
    [(null? children) '()]
    [(pair? children)
     (let ([head (car children)]
           [tail (cdr children)])
       (cond
         [(include-form? head)
          (append (expand-include head source-file included-files)
                  (process-form-children tail source-file included-files))]
         [(pair? head)
          (cons (walk-form-includes head source-file included-files)
                (process-form-children tail source-file included-files))]
         [else
          (cons head (process-form-children tail source-file included-files))]))]
    [else children]))  ; improper tail: leave as-is

;; Walk a single form. Atoms pass through; quoted / syntax-binding heads
;; protect their data; everything else recurses via process-form-children.
(define (walk-form-includes form source-file included-files)
  (cond
    [(not (pair? form)) form]
    [(skip-include-walk? (car form)) form]
    [else
     (process-form-children form source-file included-files)]))

;; Process include directives at any depth. Top-level includes splice
;; into the program; nested includes splice into their containing form.
;; exprs: list of s-expressions
;; source-file: current source file path
;; included-files: list of already included files (prevents circular includes)
;; Returns: expanded list of s-expressions
(define (process-includes exprs source-file included-files)
  (apply append
    (for/list ([expr exprs])
      (cond
        [(include-form? expr)
         (expand-include expr source-file included-files)]
        [(pair? expr)
         (list (walk-form-includes expr source-file included-files))]
        [else (list expr)]))))

;; Check if expression is an R7RS include form: (include "filename")
(define (include-form? expr)
  (and (list? expr)
       (= (length expr) 2)
       (eq? (car expr) 'include)
       (string? (cadr expr))))

;; Expand an include form by reading and splicing the included file
(define (expand-include expr source-file included-files)
  (let* ([include-path (cadr expr)]
         [resolved-path (resolve-include-path include-path source-file)]
         [canonical-path (simplify-path (path->complete-path resolved-path))])

    ;; Check for circular includes
    (when (member canonical-path included-files)
      (error 'include "Circular include detected: ~a" canonical-path))

    ;; Read and process the included file
    (unless (file-exists? resolved-path)
      (error 'include "File not found: ~a (resolved to: ~a)"
             include-path resolved-path))

    (let* ([included-code (file->string resolved-path)]
           [new-included (cons canonical-path included-files)]
           [included-exprs (read-all-exprs-internal included-code
                                                    resolved-path
                                                    new-included)])
      included-exprs)))

;; Resolve include path. Absolute paths are used verbatim; relative
;; paths resolve against the including file's directory, falling back
;; to the current directory if the including source has no known path.
(define (resolve-include-path include-path source-file)
  (cond
    [(absolute-path? include-path) include-path]
    [source-file
     (let* ([source-dir (or (path-only source-file) (current-directory))])
       (build-path source-dir include-path))]
    [else include-path]))

;; Read single expression from string or port
;; Returns: s-expr or eof-object
(define (read-expr input)
  (if (string? input)
      (read (open-input-string input))
      (read input)))

;; Examples:
;; (read-all-exprs "(+ 1 2) (* 3 4)") => '((+ 1 2) (* 3 4))
;; (read-expr "(lambda (x) (+ x 1))") => '(lambda (x) (+ x 1))
;; (read-expr "`(a ,b ,@c)") => '(quasiquote (a (unquote b) (unquote-splicing c)))
;;
;; Include example:
;; Given file "lib.scm" containing: (define x 1) (define y 2)
;; (read-all-exprs "(include \"lib.scm\") (+ x y)" "test.scm")
;;   => '((define x 1) (define y 2) (+ x y))

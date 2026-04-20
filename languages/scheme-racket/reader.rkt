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

;; Internal helper with circular include detection
(define (read-all-exprs-internal input source-file included-files)
  (define in (if (string? input)
                 (open-input-string input)
                 input))
  (let loop ([exprs '()])
    (define expr (read in))
    (if (eof-object? expr)
        (reverse (process-includes exprs source-file included-files))
        (loop (cons expr exprs)))))

;; Process include directives recursively
;; exprs: list of s-expressions
;; source-file: current source file path
;; included-files: list of already included files (prevents circular includes)
;; Returns: expanded list of s-expressions
(define (process-includes exprs source-file included-files)
  (apply append
    (for/list ([expr exprs])
      (if (include-form? expr)
          (expand-include expr source-file included-files)
          (list expr)))))

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

;; Resolve include path relative to source file directory
;; If no source file is provided, use current directory
(define (resolve-include-path include-path source-file)
  (if source-file
      (let* ([source-dir (or (path-only source-file) (current-directory))])
        (build-path source-dir include-path))
      include-path))

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

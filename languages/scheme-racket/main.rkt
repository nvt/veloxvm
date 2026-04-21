#lang racket

;; VeloxVM Racket Compiler - Main Entry Point
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require "reader.rkt"
         "expander.rkt"  ; Macro expansion
         "rewriter.rkt"
         "optimizer.rkt"  ; Optimizations
         "errors.rkt"     ; Error handling
         "compiler.rkt"
         "bytecode.rkt")

(provide compile-file
         compile-string
         compile-expr)

;; Compile Scheme file to VeloxVM bytecode
(define (compile-file source-file [dest-file #f])
  (with-error-context source-file #f
    (let* ([dest (or dest-file
                     (current-output-file)
                     (path-replace-extension source-file ".vm"))]
           [source-code (file->string source-file)]
           [result (compile-string source-code source-file)])
      (write-bytecode-file dest result)
      (printf "Compiled ~a -> ~a\n" source-file dest)
      dest)))

;; Compile Scheme source string
;; Uses CL-style pre-allocation: expression 0 is pre-allocated as entry point
;; source-file: optional path for resolving include directives
(define (compile-string source-code [source-file #f])
  (let* ([exprs (read-all-exprs source-code source-file)]
         ;; Expand macros first (handles define-syntax)
         ;; Filter out *FILTERED* sentinel values (from define-syntax)
         [expanded (filter (lambda (e) (not (eq? e *FILTERED*)))
                           (map expand-macros exprs))]
         ;; Rewrite derived forms
         [rewritten (map rewrite-expr expanded)]
         ;; Finalize guard: convert vm-guard back to guard
         [finalized (map finalize-guard rewritten)]
         ;; Optimize expressions (constant folding, etc.)
         [optimized (map optimize-expr finalized)]
         ;; Create main bytecode with pre-allocated expression 0
         [bc (make-bytecode)]
         ;; Pre-allocate expression 0 as empty placeholder (will be replaced)
         [_ (add-expr bc (expr-encoding 'atom '()))])

    ;; CL-Style: Accumulate bytes from each top-level expression into expression 0
    ;; Don't wrap in begin - compile each expression and concatenate their bytes
    ;; Compile directly into bc (not temp-bc) so nested expressions get correct IDs!
    (let* ([accumulated-bytes
            (apply append
              (for/list ([expr optimized])
                (let ([enc (compile-expr expr bc)])  ; Compile into bc, not temp-bc!
                  (expr-encoding-data enc))))])

      ;; Replace expression 0 with all accumulated bytes
      ;; This matches CL's approach: expression 0 contains ALL top-level code sequentially
      (replace-expr bc 0 (expr-encoding 'form accumulated-bytes))

      ;; DEBUG
      (when (debug-mode)
        (printf "DEBUG: Compiled ~a top-level expressions\n" (length optimized))
        (printf "DEBUG: Accumulated ~a bytes total\n" (length accumulated-bytes))
        (printf "DEBUG: bc has ~a expressions\n" (bytecode-expression-count bc))
        (for ([i (in-naturals)]
              [expr (bytecode-expressions bc)])
          (printf "DEBUG: Expression ~a: ~a bytes\n" i (length (expr-encoding-data expr))))))

    ;; Return the bytecode - no reversal, no form reference fixing!
    bc))

;; Command-line interface
(module+ main
  (command-line
   #:program "veloxvm-compile"
   #:once-each
   [("-o" "--output") dest "Output file" (current-output-file dest)]
   [("-v" "--verbose") "Verbose output" (verbose-mode #t)]
   [("--debug") "Debug mode" (debug-mode #t)]
   [("--opt-level") level "Optimization level (0-2, default 1)" (optimization-level (string->number level))]
   [("--no-optimize") "Disable all optimizations" (enable-optimizations #f)]
   #:args (source-file)
   (compile-file source-file)))

;; Parameters for configuration
(define current-output-file (make-parameter #f))
(define verbose-mode (make-parameter #f))
(define debug-mode (make-parameter #f))

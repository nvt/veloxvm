#lang racket

;; VeloxVM Racket Compiler - Main Entry Point
;; Copyright (c) 2025, RISE Research Institutes of Sweden AB

(require "reader.rkt"
         "expander.rkt"  ; Macro expansion
         "rewriter.rkt"
         "optimizer.rkt"  ; Optimizations
         "dead-define.rkt"    ; Strip unreferenced top-level defines
         "errors.rkt"     ; Error handling
         "compiler.rkt"
         "bytecode.rkt")

(provide compile-file
         compile-string
         compile-expr
         compile-batch)

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
  (opt-stats-reset!)
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
         ;; Strip top-level defines whose names are never referenced.
         [pruned (eliminate-dead-defines optimized)]
         ;; Collect top-level user (define name ...) names that also
         ;; happen to be VM primitives. encode-symbol will route call
         ;; sites of these names to the user binding rather than the
         ;; primitive ID.
         [shadowed (collect-shadowed-primitives pruned)]
         ;; Create main bytecode with pre-allocated expression 0
         [bc (make-bytecode)]
         ;; Pre-allocate expression 0 as empty placeholder (will be replaced)
         [_ (add-expr bc (expr-encoding 'atom #""))])

    ;; CL-Style: Accumulate bytes from each top-level expression into expression 0
    ;; Don't wrap in begin - compile each expression and concatenate their bytes
    ;; Compile directly into bc (not temp-bc) so nested expressions get correct IDs!
    (parameterize ([current-shadowed-primitives shadowed])
    (let* ([accumulated-bytes
            (apply bytes-append
              (for/list ([expr pruned])
                (let ([enc (compile-expr expr bc)])  ; Compile into bc, not temp-bc!
                  (expr-encoding-data enc))))])

      ;; Replace expression 0 with all accumulated bytes
      ;; This matches CL's approach: expression 0 contains ALL top-level code sequentially
      (replace-expr bc 0 (expr-encoding 'form accumulated-bytes))

      ;; DEBUG
      (when (debug-mode)
        (printf "DEBUG: Compiled ~a top-level expressions\n" (length pruned))
        (printf "DEBUG: Accumulated ~a bytes total\n" (bytes-length accumulated-bytes))
        (printf "DEBUG: bc has ~a expressions\n" (bytecode-expression-count bc))
        (for ([i (in-naturals)]
              [expr (bytecode-expressions bc)])
          (printf "DEBUG: Expression ~a: ~a bytes\n" i (bytes-length (expr-encoding-data expr))))))

    ) ; close parameterize

    ;; Optimisation stats summary, after all rules have fired.
    (when (opt-stats?)
      (opt-stats-print))

    ;; Return the bytecode - no reversal, no form reference fixing!
    bc))

;; Walk top-level expressions collecting names that are both
;; user-defined via (define name ...) or (define (name . _) ...) AND
;; are VM primitives. encode-symbol uses this set to skip primitive
;; resolution when the user has provided their own binding.
(define (collect-shadowed-primitives exprs)
  (define seen (make-hash))
  (for ([e (in-list exprs)])
    (when (and (pair? e) (eq? (car e) 'define) (pair? (cdr e)))
      (let* ([target (cadr e)]
             [name (cond
                     [(symbol? target) target]
                     [(pair? target) (car target)]
                     [else #f])])
        (when (and (symbol? name) (get-primitive-id name))
          (hash-set! seen name #t)))))
  (hash-keys seen))

;; Compile every entry in a manifest file inside one Racket process so the
;; module-load cost is paid once. Each manifest line is either
;;   SRC                        (dest is SRC with .vm extension)
;;   SRC<TAB>DEST
;; Blank lines and lines starting with '#' are ignored. Per-file failures
;; are reported to stderr but don't abort the batch; returns the count of
;; failed entries so callers can exit non-zero.
(define (compile-batch path)
  (define failures 0)
  (with-input-from-file path
    (lambda ()
      (for ([line (in-lines)])
        (define trimmed (string-trim line))
        (unless (or (string=? trimmed "")
                    (regexp-match? #rx"^#" trimmed))
          (define fields (string-split trimmed "\t"))
          (with-handlers
            ([exn:fail?
              (lambda (e)
                (set! failures (+ failures 1))
                (eprintf "BATCH-FAIL ~a: ~a~n"
                         (car fields) (exn-message e)))])
            (case (length fields)
              [(1) (compile-file (car fields))]
              [(2) (compile-file (car fields) (cadr fields))]
              [else
               (set! failures (+ failures 1))
               (eprintf "BATCH-FAIL ~a: malformed manifest line~n" trimmed)]))))))
  failures)

;; Command-line interface
(module+ main
  (define batch-file #f)
  (command-line
   #:program "veloxvm-compile"
   #:once-each
   [("-o" "--output") dest "Output file" (current-output-file dest)]
   [("-v" "--verbose") "Verbose output" (verbose-mode #t)]
   [("--debug") "Debug mode" (debug-mode #t)]
   [("--opt-level") level "Optimization level (0-2, default 1)" (optimization-level (string->number level))]
   [("--no-optimize") "Disable all optimizations" (enable-optimizations #f)]
   [("--opt-stats") "Print per-bucket optimization fire counts to stderr" (opt-stats? #t)]
   [("--trace-opts") "Trace each optimization rewrite to stderr" (opt-trace? #t)]
   [("--batch") manifest
                "Compile every SRC[<tab>DEST] line in MANIFEST in one process"
                (set! batch-file manifest)]
   #:args sources
   (cond
     [batch-file
      (unless (null? sources)
        (error 'veloxvm-compile
               "--batch is mutually exclusive with positional source files"))
      (let ([failed (compile-batch batch-file)])
        (when (> failed 0) (exit 1)))]
     [(= (length sources) 1)
      (compile-file (car sources))]
     [else
      (error 'veloxvm-compile
             "expected one source file (or --batch MANIFEST); got ~a"
             (length sources))])))

;; Parameters for configuration
(define current-output-file (make-parameter #f))
(define verbose-mode (make-parameter #f))
(define debug-mode (make-parameter #f))

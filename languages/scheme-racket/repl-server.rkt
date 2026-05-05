#lang racket

;; VeloxVM Racket Compiler - REPL Server
;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;
;; Long-running compiler service for the VeloxVM REPL. Speaks the
;; s-expression line protocol from doc/repl-design.md and emits
;; deltas in the wire format described there (magic 0x5E 0xB6).
;;
;; Run as:  racket languages/scheme-racket/repl-server.rkt
;;
;; Holds session state across calls so that strings, symbols,
;; expressions, and macros defined in one REPL turn are visible in the
;; next. A failed compile is rolled back so the session remains
;; consistent with the VM's view of the program.

(require "reader.rkt"
         "expander.rkt"
         "rewriter.rkt"
         "optimizer.rkt"
         "compiler.rkt"
         "bytecode.rkt"
         net/base64)

(provide main)

;; ============================================================================
;; Session State
;; ============================================================================

;; Sync watermarks track how many items in each bytecode table have
;; been delivered to the VM via APPLY. Each delta covers the items
;; with index >= corresponding watermark.
;;
;; bytecode is mutable; watermarks advance on successful compile-form.
;; A no-op placeholder expression is inserted at index 0 at session
;; creation so that VMs that try to "load" the REPL program before any
;; user input have something to point at.

(struct repl-session
  (bytecode             ; the live bytecode object
   [sent-strings #:mutable]
   [sent-symbols #:mutable]
   [sent-exprs #:mutable]
   [sent-captures #:mutable]))

(define (make-session)
  (define bc (make-bytecode))
  ;; Reserve expression 0 as a no-op placeholder. The real REPL entry
  ;; expressions start at index 1 and are appended one per turn.
  (add-expr bc (expr-encoding 'atom (list)))
  (reset-macro-table!)
  ;; Watermark "sent-exprs = 0" means "expression 0 has not been
  ;; shipped to the VM yet"; the first delta will include it.
  (repl-session bc 0 0 0 0))

;; ============================================================================
;; Snapshot / Rollback
;; ============================================================================

;; A failed compile-form must not change session state. We snapshot
;; the bytecode object's mutable fields and the macro-scope chain
;; before each compile attempt and restore on exception. Lists are
;; persistent so saving them is O(1); hashes are deep-copied.

(struct bc-snap
  (strings-rev strings-index
   symbols-rev symbols-index
   exprs-rev expr-count
   captures-list
   macro-scopes))

(define (snapshot session)
  (define bc (repl-session-bytecode session))
  (bc-snap
    (bytecode-strings-rev bc)
    (hash-copy (bytecode-strings-index bc))
    (bytecode-symbols-rev bc)
    (hash-copy (bytecode-symbols-index bc))
    (bytecode-exprs-rev bc)
    (bytecode-expr-count bc)
    (bytecode-captures-list bc)
    (snapshot-macro-state)))

(define (restore! session snap)
  (define bc (repl-session-bytecode session))
  (set-bytecode-strings-rev!   bc (bc-snap-strings-rev snap))
  (set-bytecode-strings-index! bc (bc-snap-strings-index snap))
  (set-bytecode-symbols-rev!   bc (bc-snap-symbols-rev snap))
  (set-bytecode-symbols-index! bc (bc-snap-symbols-index snap))
  (set-bytecode-exprs-rev!     bc (bc-snap-exprs-rev snap))
  (set-bytecode-expr-count!    bc (bc-snap-expr-count snap))
  (set-bytecode-captures-list! bc (bc-snap-captures-list snap))
  (restore-macro-state! (bc-snap-macro-scopes snap)))

;; ============================================================================
;; S-expression Line Protocol
;; ============================================================================

;; Read one top-level s-expression from stdin or return eof.
(define (read-request)
  (read (current-input-port)))

(define (write-response value)
  (writeln value (current-output-port))
  (flush-output (current-output-port)))

(define (respond-error msg [line 0] [col 0])
  (write-response `(error ,msg (,line ,col))))

(define (respond-incomplete)
  (write-response '(incomplete)))

(define (respond-ok delta-bytes entry kind [name #f])
  (define b64 (bytes->string/latin-1
                (regexp-replace* #rx"\n" (base64-encode delta-bytes #"") "")))
  (define base `(ok ,b64 ,entry ,kind))
  (write-response (if name (append base (list name)) base)))

;; ============================================================================
;; Main Loop
;; ============================================================================

(define (main)
  (define session (make-session))
  (let loop ()
    (define request (read-request))
    (cond
      [(eof-object? request) (void)]
      [else
       (handle-request session request)
       (loop)])))

(define (handle-request session request)
  (cond
    [(not (and (pair? request) (symbol? (car request))))
     (respond-error "malformed request")]
    [(eq? (car request) 'shutdown)
     (exit 0)]
    [(eq? (car request) 'reset)
     (do-reset session)]
    [(eq? (car request) 'compile-form)
     (cond
       [(and (pair? (cdr request)) (string? (cadr request)))
        (do-compile-form session (cadr request))]
       [else
        (respond-error "compile-form expects one string argument")])]
    [else
     (respond-error (format "unknown request: ~a" (car request)))]))

(define (do-reset session)
  (define bc (make-bytecode))
  (add-expr bc (expr-encoding 'atom (list)))
  (reset-macro-table!)
  ;; In Racket we can't replace the immutable bytecode field, so update
  ;; the mutable fields in place instead.
  (define old (repl-session-bytecode session))
  (set-bytecode-strings-rev!   old (bytecode-strings-rev bc))
  (set-bytecode-strings-index! old (bytecode-strings-index bc))
  (set-bytecode-symbols-rev!   old (bytecode-symbols-rev bc))
  (set-bytecode-symbols-index! old (bytecode-symbols-index bc))
  (set-bytecode-exprs-rev!     old (bytecode-exprs-rev bc))
  (set-bytecode-expr-count!    old (bytecode-expr-count bc))
  (set-bytecode-captures-list! old '())
  (set-repl-session-sent-strings!  session 0)
  (set-repl-session-sent-symbols!  session 0)
  (set-repl-session-sent-exprs!    session 0)
  (set-repl-session-sent-captures! session 0)
  (write-response '(ok)))

;; ============================================================================
;; Compile-Form
;; ============================================================================

(define (do-compile-form session source-text)
  (define forms (try-read-all-forms source-text))
  (cond
    [(eq? forms 'incomplete) (respond-incomplete)]
    [(read-error? forms) (respond-error (read-error-msg forms)
                                        (read-error-line forms)
                                        (read-error-col forms))]
    [(null? forms) (respond-incomplete)]
    [else
     (define snap (snapshot session))
     (with-handlers
       ([exn:fail?
         (lambda (e)
           (restore! session snap)
           (respond-error (exn-message e)))])
       (compile-and-respond session forms))]))

(define (compile-and-respond session forms)
  (define bc (repl-session-bytecode session))
  ;; Wrap multi-form input in (begin ...) so we always have a single
  ;; top-level form; preserve a singleton form unwrapped for accurate
  ;; kind detection on (define ...) etc.
  (define wrapped
    (if (= (length forms) 1) (car forms) `(begin ,@forms)))

  ;; Determine kind by inspecting the syntactic head before expansion.
  (define-values (kind name)
    (cond
      [(and (pair? wrapped) (eq? (car wrapped) 'define)
            (pair? (cdr wrapped)))
       (values 'define
               (let ([first (cadr wrapped)])
                 (cond
                   [(symbol? first) first]                   ; (define x ...)
                   [(and (pair? first) (symbol? (car first)))
                    (car first)]                              ; (define (f x) ...)
                   [else #f])))]
      [(and (pair? wrapped) (eq? (car wrapped) 'define-syntax))
       (values 'stmt #f)]
      [else
       (values 'expr #f)]))

  ;; Macro expansion + rewrite + optimization, mirroring main.rkt.
  (define expanded-list
    (filter (lambda (e) (not (eq? e *FILTERED*)))
            (map expand-macros (list wrapped))))

  ;; If define-syntax filtered everything, emit a no-op entry.
  (define-values (entry-expr-id final-kind)
    (cond
      [(null? expanded-list)
       (define id (add-expr bc (expr-encoding 'atom (list))))
       (values id 'stmt)]
      [else
       (define rewritten (map rewrite-expr expanded-list))
       (define finalized (map finalize-guard rewritten))
       (define optimized (map optimize-expr finalized))
       ;; For kind=expr, wrap the outermost form in (write <form>) so that
       ;; the VM emits the value as an IO_OUT byte stream the driver can
       ;; render. Defines and statements are emitted verbatim.
       (define to-compile
         (cond
           [(and (eq? kind 'expr) (= (length optimized) 1))
            (list `(write ,(car optimized)))]
           [else optimized]))
       (define-values (entry-id _) (compile-forms-as-entry bc to-compile))
       (values entry-id kind)]))

  ;; Emit delta covering everything new since last call.
  (define delta-bytes (build-delta session entry-expr-id))

  ;; Update watermarks.
  (set-repl-session-sent-strings!  session (length (bytecode-strings bc)))
  (set-repl-session-sent-symbols!  session (length (bytecode-symbols bc)))
  (set-repl-session-sent-exprs!    session (bytecode-expr-count bc))
  (set-repl-session-sent-captures! session (length (bytecode-captures-list bc)))

  (respond-ok delta-bytes entry-expr-id final-kind
              (and name (symbol->string name))))

;; Compile a list of top-level forms, concatenating their byte
;; encodings into a single new top-level expression. Returns the new
;; expression's id.
(define (compile-forms-as-entry bc forms)
  (define accumulated
    (apply append
           (for/list ([f forms])
             (expr-encoding-data (compile-expr f bc)))))
  (define id (add-expr bc (expr-encoding 'form accumulated)))
  (values id #f))

;; ============================================================================
;; Reader Wrapper (incomplete vs error vs ok)
;; ============================================================================

(struct read-error (msg line col))

(define (try-read-all-forms text)
  (with-handlers
    ([exn:fail:read:eof? (lambda (e) 'incomplete)]
     [exn:fail:read?
      (lambda (e)
        (define srclocs (exn:fail:read-srclocs e))
        (define line (if (and (pair? srclocs) (srcloc-line (car srclocs)))
                         (srcloc-line (car srclocs)) 0))
        (define col (if (and (pair? srclocs) (srcloc-column (car srclocs)))
                        (srcloc-column (car srclocs)) 0))
        (read-error (exn-message e) line col))])
    (read-all-exprs text)))

;; ============================================================================
;; Delta Wire-Format Emission
;; ============================================================================

;; Tags from doc/repl-design.md
(define DELTA-MAGIC-1 #x5E)
(define DELTA-MAGIC-2 #xB6)
(define DELTA-VERSION 1)

(define TAG-STRINGS-APPEND  #x01)
(define TAG-SYMBOLS-APPEND  #x02)
(define TAG-EXPRS-APPEND    #x03)
(define TAG-CAPTURES-APPEND #x04)
(define TAG-ENTRY-EXPR      #x05)
(define TAG-END             #xFF)

(define (build-delta session entry-expr-id)
  (define out (open-output-bytes))
  (write-byte DELTA-MAGIC-1 out)
  (write-byte DELTA-MAGIC-2 out)
  (write-byte DELTA-VERSION out)

  (define bc (repl-session-bytecode session))
  (define sent-strings (repl-session-sent-strings session))
  (define sent-symbols (repl-session-sent-symbols session))
  (define sent-exprs (repl-session-sent-exprs session))
  (define sent-captures (repl-session-sent-captures session))

  ;; STRINGS_APPEND
  (define new-strings (drop (bytecode-strings bc) sent-strings))
  (when (not (null? new-strings))
    (write-section out TAG-STRINGS-APPEND
                   (encode-string-section sent-strings new-strings)))

  ;; SYMBOLS_APPEND
  (define new-symbols (drop (bytecode-symbols bc) sent-symbols))
  (when (not (null? new-symbols))
    (write-section out TAG-SYMBOLS-APPEND
                   (encode-symbol-section sent-symbols new-symbols)))

  ;; EXPRS_APPEND
  (define new-exprs (drop (bytecode-expressions bc) sent-exprs))
  (when (not (null? new-exprs))
    (write-section out TAG-EXPRS-APPEND
                   (encode-expr-section sent-exprs new-exprs)))

  ;; CAPTURES_APPEND
  (define captures (bytecode-captures-list bc))
  (define new-captures (take captures (- (length captures) sent-captures)))
  (when (not (null? new-captures))
    (write-section out TAG-CAPTURES-APPEND
                   (encode-captures-section new-captures)))

  ;; ENTRY_EXPR
  (write-section out TAG-ENTRY-EXPR
                 (u16->bytes entry-expr-id))

  ;; END
  (write-byte TAG-END out)
  (write-bytes (u16->bytes 0) out)

  (get-output-bytes out))

(define (write-section out tag payload-bytes)
  (write-byte tag out)
  (write-bytes (u16->bytes (bytes-length payload-bytes)) out)
  (write-bytes payload-bytes out))

(define (encode-string-section start-id items)
  (define out (open-output-bytes))
  (write-bytes (u16->bytes start-id) out)
  (write-bytes (u16->bytes (length items)) out)
  (for ([s items])
    (define b (string->bytes/utf-8 s))
    (write-bytes (u16->bytes (bytes-length b)) out)
    (write-bytes b out))
  (get-output-bytes out))

(define (encode-symbol-section start-id items)
  (encode-string-section start-id (map symbol->string items)))

(define (encode-expr-section start-id items)
  (define out (open-output-bytes))
  (write-bytes (u16->bytes start-id) out)
  (write-bytes (u16->bytes (length items)) out)
  (for ([e items])
    (define data (expr-encoding-data e))
    (write-bytes (u16->bytes (length data)) out)
    (for ([b data])
      (write-byte b out)))
  (get-output-bytes out))

(define (encode-captures-section items)
  (define out (open-output-bytes))
  (write-bytes (u16->bytes (length items)) out)
  (for ([entry items])
    (define expr-id (car entry))
    (define sym-ids (cdr entry))
    (define entry-bytes (+ 2 (* 2 (length sym-ids))))
    (write-bytes (u16->bytes entry-bytes) out)
    (write-bytes (u16->bytes expr-id) out)
    (for ([sid sym-ids])
      (write-bytes (u16->bytes sid) out)))
  (get-output-bytes out))

(define (u16->bytes n)
  (integer->integer-bytes n 2 #f #t))   ; unsigned, big-endian

;; ============================================================================
;; Entry Point
;; ============================================================================

(module+ main
  (main))

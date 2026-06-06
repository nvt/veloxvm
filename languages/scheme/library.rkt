#lang racket

;; VeloxVM Scheme Compiler - R7RS Library System
;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;
;; Structural lowering. `define-library` / `import` / `export` /
;; `cond-expand` are lowered to ordinary top-level forms in the single flat
;; namespace the rest of the pipeline already uses. See
;; doc/r7rs-library-system-design.md.
;;
;; Scope and deliberate limitations:
;;   - Flat namespace, no isolation: two libraries that define the same
;;     internal name collide, exactly as two hand-written top-level defines
;;     would. Name-mangling for real isolation is layered on separately.
;;   - Permissive: imports validate the library name but do not hide
;;     un-imported names; every visible binding stays visible.
;;   - Import sets: `only` / `except` are accepted and ignored (harmless
;;     under permissive resolution); `prefix` / `rename` are rejected
;;     (ignoring them would silently miscompile references).
;;   - Macros exported by a library become globally visible (the expander
;;     registers them globally); per-library macro hygiene is handled separately.

(require "reader.rkt")

(provide lower-libraries)

;; ---------------------------------------------------------------------------
;; Feature set and known standard libraries
;; ---------------------------------------------------------------------------

;; cond-expand feature identifiers. MUST stay in sync with the (features)
;; procedure in runtime/r7rs-features.scm -- the compile-time selector here
;; and the runtime list there are two views of the same capability set.
(define library-features
  '(veloxvm r5rs r7rs-subset exact-closed ratios))

;; R7RS standard library names the compiler recognises. These
;; are no-ops on import: their bindings already exist as VM primitives or
;; as the auto-prepended runtime prelude.
(define standard-libraries
  (list '(scheme base) '(scheme write) '(scheme char) '(scheme inexact)
        '(scheme complex) '(scheme cxr) '(scheme file) '(scheme read)
        '(scheme time) '(scheme process-context) '(scheme lazy)
        '(scheme load) '(scheme repl) '(scheme eval) '(scheme r5rs)))

;; ---------------------------------------------------------------------------
;; A registered library
;; ---------------------------------------------------------------------------

(struct lib (name exports imports body) #:transparent)

;; Library names are lists of symbols and/or exact integers, e.g.
;; (scheme base) or (srfi 1). Used directly as equal?-based hash keys.
(define (library-name? x)
  (and (list? x)
       (pair? x)
       (andmap (lambda (e) (or (symbol? e) (exact-integer? e))) x)))

;; ---------------------------------------------------------------------------
;; Form predicates
;; ---------------------------------------------------------------------------

(define (tagged? form sym)
  (and (pair? form) (eq? (car form) sym)))

(define (define-library-form? f) (tagged? f 'define-library))
(define (cond-expand-form? f)    (tagged? f 'cond-expand))
(define (import-form? f)         (tagged? f 'import))
(define (include-form? f)
  (and (pair? f) (memq (car f) '(include include-ci)) #t))

;; ---------------------------------------------------------------------------
;; cond-expand
;; ---------------------------------------------------------------------------

;; A feature requirement is: a feature id (symbol); (library <name>);
;; (and req ...); (or req ...); (not req). `else` is handled at clause
;; level, not here.
(define (feature-present? req known-libs)
  (cond
    [(symbol? req) (and (memq req library-features) #t)]
    [(pair? req)
     (case (car req)
       [(library) (library-known? (cadr req) known-libs)]
       [(and) (andmap (lambda (r) (feature-present? r known-libs)) (cdr req))]
       [(or)  (ormap  (lambda (r) (feature-present? r known-libs)) (cdr req))]
       [(not) (not (feature-present? (cadr req) known-libs))]
       [else (error 'cond-expand "unknown feature requirement: ~a" req)])]
    [else (error 'cond-expand "malformed feature requirement: ~a" req)]))

;; Select the body (list of forms/declarations) of the first matching
;; clause, or '() if none match and there is no else clause.
(define (select-cond-expand-clause clauses known-libs)
  (let loop ([cs clauses])
    (cond
      [(null? cs) '()]
      [(not (and (pair? (car cs)) (pair? (cdar cs))))
       (error 'cond-expand "malformed clause: ~a" (car cs))]
      [(eq? (caar cs) 'else) (cdar cs)]
      [(feature-present? (caar cs) known-libs) (cdar cs)]
      [else (loop (cdr cs))])))

;; ---------------------------------------------------------------------------
;; Import-set parsing
;; ---------------------------------------------------------------------------

;; Reduce an import set to the underlying library name. `only`/`except`
;; wrap a set and are accepted (their filtering is a no-op under permissive
;; resolution). `prefix`/`rename` change the spelling the program uses, so
;; ignoring them would miscompile -- rejected.
(define (import-set->name iset)
  (cond
    [(and (pair? iset) (memq (car iset) '(only except)))
     (import-set->name (cadr iset))]
    [(and (pair? iset) (memq (car iset) '(prefix rename)))
     (error 'import
            "import set '~a' is not supported: ~a"
            (car iset) iset)]
    [(library-name? iset) iset]
    [else (error 'import "malformed import set: ~a" iset)]))

(define (library-known? name known-libs)
  (or (and (member name standard-libraries) #t)
      (and (member name known-libs) #t)))

;; ---------------------------------------------------------------------------
;; Library registration
;; ---------------------------------------------------------------------------

;; Process a list of library declarations, accumulating into mutable boxes.
;; Declarations: export, import, begin, include, include-ci,
;; include-library-declarations, cond-expand.
(define (process-declarations decls source-file known-libs
                              exports-box imports-box body-box)
  (define (add-exports! specs) (set-box! exports-box (append (unbox exports-box) specs)))
  (define (add-imports! sets)  (set-box! imports-box (append (unbox imports-box) sets)))
  (define (add-body! forms)    (set-box! body-box   (append (unbox body-box) forms)))
  (for ([d decls])
    (cond
      [(tagged? d 'export) (add-exports! (cdr d))]
      [(tagged? d 'import) (add-imports! (cdr d))]
      [(tagged? d 'begin)  (add-body! (cdr d))]
      [(include-form? d)
       ;; (include "f" ...) / (include-ci "f" ...): splice each file's
       ;; expressions into the body. include-ci is treated as include
       ;; (no case folding -- a documented limitation).
       (for ([path (cdr d)])
         (add-body! (read-included-exprs path source-file)))]
      [(tagged? d 'include-library-declarations)
       ;; Splice declarations (not expressions) from each file, recursively.
       (for ([path (cdr d)])
         (process-declarations (read-included-exprs path source-file)
                               source-file known-libs
                               exports-box imports-box body-box))]
      [(cond-expand-form? d)
       (process-declarations (select-cond-expand-clause (cdr d) known-libs)
                             source-file known-libs
                             exports-box imports-box body-box)]
      [else (error 'define-library "unknown library declaration: ~a" d)])))

(define (register-library! form registry source-file known-libs)
  (define name (cadr form))
  (unless (library-name? name)
    (error 'define-library "invalid library name: ~a" name))
  (define exports-box (box '()))
  (define imports-box (box '()))
  (define body-box (box '()))
  (process-declarations (cddr form) source-file known-libs
                        exports-box imports-box body-box)
  (hash-set! registry name
             (lib name (unbox exports-box) (unbox imports-box) (unbox body-box))))

;; ---------------------------------------------------------------------------
;; Dependency ordering
;; ---------------------------------------------------------------------------

;; Emit libraries so each is defined before any (registered) library that
;; imports it. Standard-library imports are ignored as edges. Cyclic user
;; imports are an error.
(define (topo-sort-libraries registry order)
  (define visited (make-hash))   ; name -> 'done
  (define in-progress (make-hash))
  (define result '())
  (define (visit name)
    (cond
      [(hash-has-key? visited name) (void)]
      [(hash-has-key? in-progress name)
       (error 'define-library "circular library import involving ~a" name)]
      [else
       (hash-set! in-progress name #t)
       (define l (hash-ref registry name))
       (for ([iset (lib-imports l)])
         (define dep (import-set->name iset))
         (when (hash-has-key? registry dep)
           (visit dep)))
       (hash-remove! in-progress name)
       (hash-set! visited name 'done)
       (set! result (cons name result))]))
  (for ([name order]) (visit name))
  (reverse result))

;; ---------------------------------------------------------------------------
;; Top-level lowering
;; ---------------------------------------------------------------------------

;; Lower all library/import/cond-expand forms in a top-level expression
;; list to a flat list of ordinary forms. `source-file` is used to resolve
;; includes inside libraries and cond-expand clauses.
(define (lower-libraries exprs [source-file #f])
  ;; Shallow pre-scan of library names so (library X) cond-expand tests and
  ;; import validation are independent of source order.
  (define known-libs
    (for/list ([f exprs] #:when (define-library-form? f)) (cadr f)))

  (define registry (make-hash))
  (define lib-order '())     ; registered names, definition order (reversed)
  (define program-forms '()) ; non-library top-level forms (reversed)

  (define (handle f)
    (cond
      [(include-form? f)
       ;; Top-level includes are normally expanded by the reader already;
       ;; this covers includes revealed by selecting a cond-expand clause.
       (for ([path (cdr f)])
         (for-each handle (read-included-exprs path source-file)))]
      [(cond-expand-form? f)
       (for-each handle (select-cond-expand-clause (cdr f) known-libs))]
      [(define-library-form? f)
       (register-library! f registry source-file known-libs)
       (set! lib-order (cons (cadr f) lib-order))]
      [(import-form? f)
       ;; Top-level program import: validate names, then drop.
       (for ([iset (cdr f)])
         (define name (import-set->name iset))
         (unless (library-known? name known-libs)
           (error 'import "unknown library: ~a" name)))]
      [else (set! program-forms (cons f program-forms))]))

  (for-each handle exprs)

  ;; Validate each library's own imports too (catches typos in deps).
  (for ([(name l) (in-hash registry)])
    (for ([iset (lib-imports l)])
      (define dep (import-set->name iset))
      (unless (library-known? dep known-libs)
        (error 'import "library ~a imports unknown library: ~a" name dep))))

  (define ordered (topo-sort-libraries registry (reverse lib-order)))
  (append (append-map (lambda (name) (lib-body (hash-ref registry name))) ordered)
          (reverse program-forms)))

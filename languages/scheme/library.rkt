#lang racket

;; VeloxVM Scheme Compiler - R7RS Library System
;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;
;; Lowers R7RS define-library / import / export / cond-expand to ordinary
;; top-level forms in the single flat namespace the rest of the pipeline
;; uses. A pure source-to-source pass: no VM-core or bytecode changes.
;; See doc/r7rs-library-system-design.md.
;;
;; Structural lowering (flatten libraries, resolve cond-expand,
;; validate imports).
;;
;; Name isolation by mangling. Each library's top-level
;; definitions are alpha-renamed to unique symbols (e.g. helper$L0), and
;; references are rewritten per a per-library substitution map. This is
;; sound without local-scope analysis: consistently renaming every
;; occurrence of an identifier within a library's forms to a fresh,
;; unique spelling is whole-identifier alpha-conversion, which preserves
;; binding/shadowing structure (shadowing is positional). The only
;; positions that must be left alone are quoted data. With the
;; substitution-map model the import sets only / except / prefix / rename
;; fall out naturally and are supported for user libraries.
;;
;; Macros participate in mangling. A library's define-syntax
;; names are treated as top-level definitions and mangled like values, and
;; the substitution walk no longer skips macro forms, so a macro template's
;; references to its library's own (possibly non-exported) bindings are
;; rewritten consistently. Because the expander is global and keyed by
;; name, mangling macro names to unique symbols yields per-library macro
;; isolation for free (a non-importer never names m$L<index>) and lets
;; export / import sets (only/except/prefix/rename) apply to macros exactly
;; as they do to values -- with no expander changes.
;;
;; Deliberate limitations:
;;   - Macro hygiene edges: syntax-rules literal identifiers are matched by
;;     spelling, so a library binding (or import) whose name coincides with
;;     a macro literal can interact badly. This is the existing expander's
;;     hygiene gap, not introduced here.
;;   - Import sets on *standard* libraries (only/except/prefix/rename)
;;     need a per-standard-library export list we do not yet carry, so
;;     they are rejected; plain (import (scheme base)) is a no-op.

(require racket/runtime-path
         "reader.rkt")

(provide lower-libraries library-search-paths)

;; ---------------------------------------------------------------------------
;; Feature set and known standard libraries
;; ---------------------------------------------------------------------------

;; cond-expand feature identifiers. MUST stay in sync with the (features)
;; procedure in runtime/r7rs-features.scm -- the compile-time selector here
;; and the runtime list there are two views of the same capability set.
(define library-features
  '(veloxvm r5rs r7rs-subset exact-closed ratios))

;; R7RS standard library names the compiler recognises. On import these
;; are no-ops: their bindings already exist as VM primitives or as the
;; auto-prepended runtime prelude, so nothing is mangled or substituted.
(define standard-libraries
  (list '(scheme base) '(scheme write) '(scheme char) '(scheme inexact)
        '(scheme complex) '(scheme cxr) '(scheme file) '(scheme read)
        '(scheme time) '(scheme process-context) '(scheme lazy)
        '(scheme load) '(scheme repl) '(scheme eval) '(scheme r5rs)))

;; ---------------------------------------------------------------------------
;; A registered library
;; ---------------------------------------------------------------------------

;; index    : unique integer, used to build mangled names
;; exports  : list of export specs (symbol | (rename internal external))
;; imports  : list of import sets
;; body     : flattened top-level forms (post cond-expand + include)
;; own      : list of names this library defines at top level (to mangle)
;; mangle   : hash own-name -> mangled symbol
;; exp-map  : list of (public-name . mangled-symbol) for exported names
(struct lib (name index exports imports body own mangle exp-map) #:mutable #:transparent)

;; Library names are lists of symbols and/or exact integers, e.g.
;; (scheme base) or (srfi 1). Used directly as equal?-based hash keys.
(define (library-name? x)
  (and (list? x)
       (pair? x)
       (andmap (lambda (e) (or (symbol? e) (exact-integer? e))) x)))

(define (standard-library? name) (and (member name standard-libraries) #t))

;; ---------------------------------------------------------------------------
;; Form predicates
;; ---------------------------------------------------------------------------

(define (tagged? form sym) (and (pair? form) (eq? (car form) sym)))
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

(define (library-known? name known-libs)
  (or (standard-library? name) (and (member name known-libs) #t)))

;; ---------------------------------------------------------------------------
;; Collecting a body's top-level definition names
;; ---------------------------------------------------------------------------

;; The name a (define ...) target introduces, peeling curried defines.
(define (define-target-name target)
  (cond [(symbol? target) target]
        [(pair? target) (define-target-name (car target))]
        [else #f]))

;; Names in a formals list (proper, improper, or a bare rest symbol).
(define (formals->names formals)
  (cond [(symbol? formals) (list formals)]
        [(pair? formals) (cons (car formals) (formals->names (cdr formals)))]
        [else '()]))

;; The top-level names introduced by a define-record-type form:
;; the type name, constructor, predicate, and each accessor/mutator.
(define (record-type-names f)
  ;; (define-record-type NAME (CTOR field ...) PRED (field ACC [MUT]) ...)
  (define names '())
  (define (add! x) (when (symbol? x) (set! names (cons x names))))
  (when (>= (length f) 4)
    (add! (define-target-name (list-ref f 1)))       ; type name
    (let ([ctor (list-ref f 2)])                      ; constructor
      (when (pair? ctor) (add! (car ctor))))
    (add! (list-ref f 3))                             ; predicate
    (for ([spec (drop f 4)])                          ; field specs
      (when (pair? spec)
        (when (>= (length spec) 2) (add! (list-ref spec 1)))  ; accessor
        (when (>= (length spec) 3) (add! (list-ref spec 2)))))) ; mutator
  (reverse names))

(define (defined-names-of f)
  (cond
    [(not (pair? f)) '()]
    [(eq? (car f) 'define)
     (let ([n (define-target-name (and (pair? (cdr f)) (cadr f)))])
       (if n (list n) '()))]
    [(eq? (car f) 'define-values)
     (if (pair? (cdr f)) (formals->names (cadr f)) '())]
    [(eq? (car f) 'define-record-type) (record-type-names f)]
    [(eq? (car f) 'begin) (append-map defined-names-of (cdr f))]
    ;; define-syntax: the macro keyword is a top-level definition; mangling
    ;; it isolates the macro per library and lets export/import sets apply.
    [(eq? (car f) 'define-syntax)
     (if (and (pair? (cdr f)) (symbol? (cadr f))) (list (cadr f)) '())]
    [else '()]))

(define (collect-defined-names forms) (append-map defined-names-of forms))

;; ---------------------------------------------------------------------------
;; Substitution (alpha-renaming) walk
;; ---------------------------------------------------------------------------

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

;; Rewrite every symbol occurrence found in `subst`, except inside quoted
;; data. Macro forms (define-syntax / let-syntax / letrec-syntax) ARE
;; walked: a template's free references to library bindings, and macro
;; keywords that are library names, get rewritten. syntax-rules pattern
;; variables, literals, and the ellipsis are left alone because they do not
;; appear in `subst` (a library name colliding with a literal is the known
;; expander hygiene edge, not handled here).
(define (subst-walk form subst)
  (cond
    [(symbol? form) (hash-ref subst form form)]
    [(not (pair? form)) form]
    [(eq? (car form) 'quote) form]
    [(eq? (car form) 'quasiquote) (list 'quasiquote (qq-walk (cadr form) subst 1))]
    [else (cons (subst-walk (car form) subst)
                (subst-walk (cdr form) subst))]))

;; Quasiquote-aware walk: substitute only inside unquote / unquote-splicing
;; at the matching nesting level; leave literal template data alone.
(define (qq-walk form subst level)
  (cond
    [(not (pair? form)) form]
    [(eq? (car form) 'unquote)
     (if (= level 1)
         (list 'unquote (subst-walk (cadr form) subst))
         (list 'unquote (qq-walk (cadr form) subst (sub1 level))))]
    [(eq? (car form) 'unquote-splicing)
     (if (= level 1)
         (list 'unquote-splicing (subst-walk (cadr form) subst))
         (list 'unquote-splicing (qq-walk (cadr form) subst (sub1 level))))]
    [(eq? (car form) 'quasiquote)
     (list 'quasiquote (qq-walk (cadr form) subst (add1 level)))]
    [else (cons (qq-walk (car form) subst level)
                (qq-walk (cdr form) subst level))]))

;; ---------------------------------------------------------------------------
;; Library registration
;; ---------------------------------------------------------------------------

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
       ;; (include "f" ...) / (include-ci "f" ...). include-ci is
       ;; treated as include (no case folding -- a documented limitation).
       (for ([path (cdr d)])
         (add-body! (read-included-exprs path source-file)))]
      [(tagged? d 'include-library-declarations)
       (for ([path (cdr d)])
         (process-declarations (read-included-exprs path source-file)
                               source-file known-libs
                               exports-box imports-box body-box))]
      [(cond-expand-form? d)
       (process-declarations (select-cond-expand-clause (cdr d) known-libs)
                             source-file known-libs
                             exports-box imports-box body-box)]
      [else (error 'define-library "unknown library declaration: ~a" d)])))

(define (register-library! form registry source-file known-libs index)
  (define name (cadr form))
  (unless (library-name? name)
    (error 'define-library "invalid library name: ~a" name))
  (define exports-box (box '()))
  (define imports-box (box '()))
  (define body-box (box '()))
  (process-declarations (cddr form) source-file known-libs
                        exports-box imports-box body-box)
  (define body (unbox body-box))
  (define own (remove-duplicates (collect-defined-names body)))
  ;; Mangle map: own-name -> own-name$L<index>.
  (define mangle (make-hash))
  (define suffix (string->symbol (format "$L~a" index)))
  (for ([n own]) (hash-set! mangle n (symbol-append n suffix)))
  ;; Export map: public-name -> mangled. Supports (rename internal external).
  (define exp-map
    (for/list ([spec (unbox exports-box)])
      (define-values (internal external)
        (cond
          [(symbol? spec) (values spec spec)]
          [(and (tagged? spec 'rename) (= (length spec) 3)) (values (cadr spec) (caddr spec))]
          [else (error 'export "malformed export spec: ~a" spec)]))
      (unless (hash-has-key? mangle internal)
        (error 'export "library ~a exports undefined name: ~a" name internal))
      (cons external (hash-ref mangle internal))))
  (hash-set! registry name
             (lib name index (unbox exports-box) (unbox imports-box)
                  body own mangle exp-map)))

;; ---------------------------------------------------------------------------
;; Import-set resolution -> substitution entries (public-name . target)
;; ---------------------------------------------------------------------------

;; Underlying library name of an import set (peeling only/except/prefix/rename).
(define (import-base iset)
  (if (and (pair? iset) (memq (car iset) '(only except prefix rename)))
      (import-base (cadr iset))
      iset))

;; Resolve an import set to a list of (public-name . target-symbol) pairs.
;; Standard libraries contribute no entries (their names resolve directly);
;; an import-set operator on a standard library is rejected.
(define (resolve-import-set iset registry)
  (define base (import-base iset))
  (define is-standard (standard-library? base))
  (cond
    [(library-name? iset)
     (cond
       [is-standard '()]
       [(hash-ref registry iset #f) => lib-exp-map]
       [else (error 'import "unknown library: ~a" iset)])]
    [(and (pair? iset) is-standard)
     (error 'import
            "import set '~a' on standard library ~a needs an export list: ~a"
            (car iset) base iset)]
    [(tagged? iset 'only)
     (let ([inner (resolve-import-set (cadr iset) registry)]
           [ids (cddr iset)])
       (for ([id ids])
         (unless (assq id inner)
           (error 'import "(only ...) names ~a not exported by ~a" id base)))
       (filter (lambda (p) (memq (car p) ids)) inner))]
    [(tagged? iset 'except)
     (let ([inner (resolve-import-set (cadr iset) registry)]
           [ids (cddr iset)])
       (filter (lambda (p) (not (memq (car p) ids))) inner))]
    [(tagged? iset 'prefix)
     (let ([inner (resolve-import-set (cadr iset) registry)]
           [p (caddr iset)])
       (map (lambda (pr) (cons (symbol-append p (car pr)) (cdr pr))) inner))]
    [(tagged? iset 'rename)
     (let* ([inner (resolve-import-set (cadr iset) registry)]
            [renames (cddr iset)])  ; each (from to)
       (map (lambda (pr)
              (let ([r (assq (car pr) (map (lambda (x) (cons (car x) (cadr x))) renames))])
                (if r (cons (cdr r) (cdr pr)) pr)))
            inner))]
    [else (error 'import "malformed import set: ~a" iset)]))

;; Merge import entries into a substitution hash, flagging conflicts where
;; one name would resolve to two different bindings.
(define (merge-import-entries! subst entries context)
  (for ([pr entries])
    (define name (car pr))
    (define target (cdr pr))
    (cond
      [(and (hash-has-key? subst name) (not (eq? (hash-ref subst name) target)))
       (error 'import "~a: ~a imported with conflicting bindings" context name)]
      [else (hash-set! subst name target)])))

;; ---------------------------------------------------------------------------
;; Dependency ordering
;; ---------------------------------------------------------------------------

(define (topo-sort-libraries registry order)
  (define visited (make-hash))
  (define in-progress (make-hash))
  (define result '())
  (define (visit name)
    (cond
      [(hash-has-key? visited name) (void)]
      [(hash-has-key? in-progress name)
       (error 'define-library "circular library import involving ~a" name)]
      [else
       (hash-set! in-progress name #t)
       (for ([iset (lib-imports (hash-ref registry name))])
         (define dep (import-base iset))
         (when (hash-has-key? registry dep) (visit dep)))
       (hash-remove! in-progress name)
       (hash-set! visited name 'done)
       (set! result (cons name result))]))
  (for ([name order]) (visit name))
  (reverse result))

;; ---------------------------------------------------------------------------
;; Library search path: loading file-based libraries by name
;; ---------------------------------------------------------------------------

;; Root directories to search for library files, beyond the source file's
;; own directory and the current directory. Extendable from the compiler
;; CLI (-I / --lib-dir). A library named (foo bar) is sought as the file
;; foo/bar.sld (then foo/bar.scm) under each root. Like static linking
;; generally, a found library is compiled into the program, not shared.
;; The default root is the compiler's own runtime/ directory, which the
;; reader already searches for (include ...), so a library shipped with
;; the compiler such as (velox sort) imports by name without -I.
(define-runtime-path library-runtime-dir "runtime")
(define library-search-paths (make-parameter (list library-runtime-dir)))

(define (source-dir-or-cwd source-file)
  (if source-file
      (or (path-only source-file) (current-directory))
      (current-directory)))

;; Locate the file defining library NAME, or #f. (foo bar) -> foo/bar.<ext>.
(define (find-library-file name source-file)
  (define comps (map (lambda (c) (format "~a" c)) name))
  (define roots (append (list (source-dir-or-cwd source-file))
                        (library-search-paths)
                        (list (current-directory))))
  (for*/or ([root (in-list roots)]
            [ext (in-list '(".sld" ".scm"))])
    (define file-comps
      (append (drop-right comps 1)
              (list (string-append (last comps) ext))))
    (define p (apply build-path root file-comps))
    (and (file-exists? p) p)))

;; Load and register the library NAME (and any sibling define-library forms
;; in the same file) from the search path. Includes inside the file resolve
;; relative to the file itself.
(define (load-library-file! name registry in-file-names source-file index-box)
  (define file (find-library-file name source-file))
  (unless file
    (error 'import "cannot find library ~s on the search path" name))
  (define forms (read-all-exprs (file->string file) (path->string file)))
  (define defs (filter define-library-form? forms))
  (unless (findf (lambda (f) (equal? (cadr f) name)) defs)
    (error 'import "file ~a does not define library ~s" file name))
  (for ([f defs])
    (unless (hash-has-key? registry (cadr f))
      (register-library! f registry file
                         (append in-file-names (hash-keys registry))
                         (unbox index-box))
      (set-box! index-box (add1 (unbox index-box))))))

;; Iteratively load every imported library that is neither standard nor
;; already registered, following transitive imports to a fixpoint. Each
;; load adds at least the requested library, so this terminates.
(define (resolve-file-libraries! registry program-imports in-file-names
                                 source-file index-box)
  (let loop ()
    (define all-imports
      (append program-imports (append-map lib-imports (hash-values registry))))
    (define missing
      (remove-duplicates
       (for/list ([iset (in-list all-imports)]
                  #:when (let ([d (import-base iset)])
                           (and (not (standard-library? d))
                                (not (hash-has-key? registry d)))))
         (import-base iset))))
    (unless (null? missing)
      (for ([name (in-list missing)])
        (load-library-file! name registry in-file-names source-file index-box))
      (loop))))

;; ---------------------------------------------------------------------------
;; Top-level lowering
;; ---------------------------------------------------------------------------

(define (lower-libraries exprs [source-file #f])
  ;; Shallow pre-scan of in-file library names so (library X) cond-expand
  ;; tests and import validation are independent of source order.
  (define in-file-names
    (for/list ([f exprs] #:when (define-library-form? f)) (cadr f)))

  (define registry (make-hash))
  (define program-forms '())   ; non-library top-level forms (reversed)
  (define program-imports '()) ; top-level import sets (reversed)
  (define index-box (box 0))   ; unique index source for mangling

  (define (handle f)
    (cond
      [(include-form? f)
       ;; Covers includes revealed by selecting a cond-expand clause;
       ;; ordinary top-level includes are expanded by the reader already.
       (for ([path (cdr f)])
         (for-each handle (read-included-exprs path source-file)))]
      [(cond-expand-form? f)
       (for-each handle (select-cond-expand-clause (cdr f) in-file-names))]
      [(define-library-form? f)
       (register-library! f registry source-file
                          (append in-file-names (hash-keys registry))
                          (unbox index-box))
       (set-box! index-box (add1 (unbox index-box)))]
      [(import-form? f)
       (set! program-imports (append (reverse (cdr f)) program-imports))]
      [else (set! program-forms (cons f program-forms))]))

  (for-each handle exprs)
  (set! program-imports (reverse program-imports))
  (set! program-forms (reverse program-forms))

  ;; Load any imported library not defined in this file from the search path.
  (resolve-file-libraries! registry program-imports in-file-names
                           source-file index-box)

  ;; Validate every import now that in-file and file-loaded libraries are
  ;; all registered.
  (define known-libs (append in-file-names (hash-keys registry)))
  (for ([iset (in-list program-imports)])
    (define dep (import-base iset))
    (unless (library-known? dep known-libs)
      (error 'import "unknown library: ~a" dep)))
  (for ([(name l) (in-hash registry)])
    (for ([iset (lib-imports l)])
      (define dep (import-base iset))
      (unless (library-known? dep known-libs)
        (error 'import "library ~a imports unknown library: ~a" name dep))))

  ;; Build each library's substitution: imports first, own names override.
  (define (library-subst l)
    (define subst (make-hash))
    (for ([iset (lib-imports l)])
      (merge-import-entries! subst (resolve-import-set iset registry) (lib-name l)))
    (for ([(name mangled) (in-hash (lib-mangle l))]) (hash-set! subst name mangled))
    subst)

  ;; Build the program's substitution from top-level imports, then drop any
  ;; name the program defines itself (a program-level define shadows it).
  (define program-subst (make-hash))
  (for ([iset program-imports])
    (merge-import-entries! program-subst (resolve-import-set iset registry) "program"))
  (for ([n (collect-defined-names program-forms)])
    (hash-remove! program-subst n))

  ;; Emit mangled library bodies (dependency order) then mangled program.
  ;; Seed the sort with all registered libraries (in-file + file-loaded) in
  ;; index order for a deterministic, dependency-correct emission.
  (define seed (map lib-name (sort (hash-values registry) < #:key lib-index)))
  (define ordered (topo-sort-libraries registry seed))
  (define lib-out
    (append-map (lambda (name)
                  (define l (hash-ref registry name))
                  (define s (library-subst l))
                  (map (lambda (form) (subst-walk form s)) (lib-body l)))
                ordered))
  (define prog-out
    (map (lambda (form) (subst-walk form program-subst)) program-forms))
  (append lib-out prog-out))

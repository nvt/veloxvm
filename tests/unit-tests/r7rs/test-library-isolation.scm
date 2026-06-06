;;; VeloxVM Unit Tests - R7RS library isolation + import sets
;;; Mangling renames each library's top-level definitions to unique names,
;;; so same-named internal bindings in different libraries no longer
;;; collide, and the only/except/prefix/rename import sets resolve over a
;;; user library's exports. See doc/r7rs-library-system-design.md.

(include "../unit-test-framework.scm")

;; --- Isolation: two libraries with an identically named private helper.
;; Under a flat namespace these would collide (the second helper would win
;; for both); mangling keeps them distinct.
(define-library (iso-a)
  (export a-val)
  (begin
    (define (helper) 100)
    (define (a-val) (helper))))

(define-library (iso-b)
  (export b-val)
  (begin
    (define (helper) 200)
    (define (b-val) (helper))))

;; --- Import sets over a user library.
(define-library (sets)
  (export one two three)
  (begin (define one 1) (define two 2) (define three 3)))

;; --- Quote / quasiquote protection: quoted occurrences of a library's own
;; (mangled) names must not be rewritten.
(define-library (q)
  (export valsym qqbuild val)
  (begin
    (define (val) 99)
    (define (valsym) 'val)            ; 'val is data; stays 'val, not 'val$Lk
    (define (qqbuild) `(val ,(val))))) ; literal val preserved; ,(val) substituted

(import (iso-a))
(import (iso-b))
(import (only (sets) one))
(import (rename (sets) (three iii)))
(import (prefix (sets) s:))
(import (q))

(test-suite "R7RS library isolation + import sets")

(assert-equal 100 (a-val) "private helper isolated (iso-a)")
(assert-equal 200 (b-val) "private helper isolated (iso-b)")
(assert-equal 1   one     "(only ...) imports the named binding")
(assert-equal 3   iii     "(rename ...) imports under the new name")
(assert-equal 2   s:two   "(prefix ...) imports under the prefixed name")
(assert-equal 'val (valsym)    "quoted datum not mangled")
(assert-equal '(val 99) (qqbuild) "quasiquote literal preserved, unquote substituted")

(test-summary)

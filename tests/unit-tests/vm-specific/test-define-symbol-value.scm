;; Regression: (define b a) where the value is a bare symbol referring
;; to another binding used to bind b to the SYMBOL a rather than to its
;; value. The fix lets the VM's define primitive evaluate symbol-typed
;; arguments, not just non-lambda forms.

(include "../unit-test-framework.scm")

(test-suite "(define b a) with symbol value")

;; Integer alias.
(define src-int 42)
(define ali-int src-int)
(assert-equal 42 ali-int "(define ali-int src-int) propagates the integer")

;; String alias (also exercises post-resolve string handling).
(define src-str "hello")
(define ali-str src-str)
(assert-equal "hello" ali-str "(define ali-str src-str) propagates the string")

;; List alias.
(define src-lst '(1 2 3))
(define ali-lst src-lst)
(assert-equal '(1 2 3) ali-lst "(define ali-lst src-lst) propagates the list")

;; Quoted symbol still binds to the symbol itself, not whatever foo is.
(define foo 99)
(define quoted-foo 'foo)
(assert-equal 99 foo "Sanity: quoted-foo doesn't shadow foo")
(assert-true (symbol? quoted-foo) "Quoted symbol stored as a symbol")

;; Form value still works.
(define result-of-form (+ 1 2))
(assert-equal 3 result-of-form "Form value evaluates as before")

(test-summary)

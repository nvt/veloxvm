#lang racket

;; Test the new scope infrastructure for let-syntax/letrec-syntax support

(require rackunit
         "../expander.rkt")

(printf "Testing scope infrastructure...\n\n")

;; Test 1: Basic scope push/pop
(printf "Test 1: Push and pop scopes...\n")
(reset-macro-table!)

;; Define a global macro
(process-define-syntax 'global-macro
  '(syntax-rules () [(global-macro) 'global]))

;; Should find it in global scope
(check-not-false (lookup-macro 'global-macro))
(printf "   Global macro found\n")

;; Push a new scope with a local macro
(define local-scope (make-hash))
(hash-set! local-scope 'local-macro
           (make-syntax-rules-transformer '() '([(local-macro) 'local])))
(push-macro-scope! local-scope)

;; Should find both global and local macros
(check-not-false (lookup-macro 'global-macro))
(check-not-false (lookup-macro 'local-macro))
(printf "   Both global and local macros found\n")

;; Pop the local scope
(pop-macro-scope!)

;; Should find global but not local anymore
(check-not-false (lookup-macro 'global-macro))
(check-false (lookup-macro 'local-macro))
(printf "   Local macro no longer visible after pop\n")

;; Test 2: Shadowing
(printf "\nTest 2: Macro shadowing...\n")
(reset-macro-table!)

;; Define global version
(process-define-syntax 'foo
  '(syntax-rules () [(foo) 'global-foo]))

;; A syntax-rules transformer returns the expanded *form*, not the
;; evaluated value. The template 'global-foo is read as (quote global-foo),
;; so the expansion is also (quote global-foo) — hence the extra quote
;; level in the expected values below.
(define global-transformer (lookup-macro 'foo))
(check-equal? (global-transformer '(foo)) ''global-foo)
(printf "   Global 'foo' returns 'global-foo\n")

;; Push local scope with shadowing definition
(define shadow-scope (make-hash))
(hash-set! shadow-scope 'foo
           (make-syntax-rules-transformer '() '([(foo) 'local-foo])))
(push-macro-scope! shadow-scope)

;; Should get local version (shadows global)
(define local-transformer (lookup-macro 'foo))
(check-equal? (local-transformer '(foo)) ''local-foo)
(printf "   Local 'foo' shadows global, returns 'local-foo\n")

;; Pop scope
(pop-macro-scope!)

;; Should get global version again
(define restored-transformer (lookup-macro 'foo))
(check-equal? (restored-transformer '(foo)) ''global-foo)
(printf "   After pop, 'foo' returns 'global-foo again\n")

;; Test 3: Nested scopes
(printf "\nTest 3: Nested scopes...\n")
(reset-macro-table!)

;; Global scope
(process-define-syntax 'a '(syntax-rules () [(a) 'global-a]))

;; Push first local scope
(define scope1 (make-hash))
(hash-set! scope1 'b (make-syntax-rules-transformer '() '([(b) 'local-b])))
(push-macro-scope! scope1)

(check-not-false (lookup-macro 'a))
(check-not-false (lookup-macro 'b))
(check-false (lookup-macro 'c))
(printf "   First level: 'a' and 'b' visible, 'c' not\n")

;; Push second local scope
(define scope2 (make-hash))
(hash-set! scope2 'c (make-syntax-rules-transformer '() '([(c) 'local-c])))
(push-macro-scope! scope2)

(check-not-false (lookup-macro 'a))
(check-not-false (lookup-macro 'b))
(check-not-false (lookup-macro 'c))
(printf "   Second level: 'a', 'b', and 'c' all visible\n")

;; Pop second scope
(pop-macro-scope!)

(check-not-false (lookup-macro 'a))
(check-not-false (lookup-macro 'b))
(check-false (lookup-macro 'c))
(printf "   After first pop: 'a' and 'b' visible, 'c' not\n")

;; Pop first scope
(pop-macro-scope!)

(check-not-false (lookup-macro 'a))
(check-false (lookup-macro 'b))
(check-false (lookup-macro 'c))
(printf "   After second pop: only 'a' visible\n")

;; Test 4: add-macro! adds to current scope
(printf "\nTest 4: add-macro! adds to current scope...\n")
(reset-macro-table!)

;; Add to global scope
(add-macro! 'global-added
            (make-syntax-rules-transformer '() '([(global-added) 'global])))
(check-not-false (lookup-macro 'global-added))
(printf "   Macro added to global scope\n")

;; Push new scope
(push-macro-scope! (make-hash))

;; Add to local scope
(add-macro! 'local-added
            (make-syntax-rules-transformer '() '([(local-added) 'local])))

;; Both should be visible
(check-not-false (lookup-macro 'global-added))
(check-not-false (lookup-macro 'local-added))
(printf "   Both global and local macros visible\n")

;; Pop scope
(pop-macro-scope!)

;; Only global should be visible
(check-not-false (lookup-macro 'global-added))
(check-false (lookup-macro 'local-added))
(printf "   After pop, only global macro visible\n")

;; Test 5: Cannot pop global scope
(printf "\nTest 5: Cannot pop global scope...\n")
(reset-macro-table!)

(check-exn
 exn:fail?
 (lambda () (pop-macro-scope!))
 "Should error when trying to pop global scope")
(printf "   Error raised when trying to pop global scope\n")

(printf "\n All scope infrastructure tests passed!\n")
(printf "  - Push/pop scopes\n")
(printf "  - Macro shadowing\n")
(printf "  - Nested scopes\n")
(printf "  - add-macro! to current scope\n")
(printf "  - Global scope protection\n")

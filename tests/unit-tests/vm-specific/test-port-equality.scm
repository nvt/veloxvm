;; Regression: vm_objects_equal in core/vm-lang.c handled VM_TYPE_PORT
;; via memcmp(&obj1->value.port, &obj2->value.port, sizeof(vm_port_t)).
;; obj->value.port is an 8-byte pointer inside the union; memcmp reading
;; sizeof(vm_port_t) (~40 bytes) overran into the obj's type field and
;; whatever lay beyond, returning non-deterministic answers for eq? /
;; eqv? / equal? on ports.
;;
;; The fix replaces the memcmp with a pointer comparison, which is the
;; correct semantic: two ports are equal iff they refer to the same
;; underlying port object.

(include "../unit-test-framework.scm")

(test-suite "Port equality")

(define p (current-output-port))
(define q (current-output-port))
(define i (current-input-port))

(assert-true  (eq?    p q) "(eq? p p) where p is the same port via two refs")
(assert-true  (eqv?   p q) "(eqv? p p)")
(assert-true  (equal? p q) "(equal? p p)")

(assert-false (eq?    p i) "(eq? <output-port> <input-port>) is #f")
(assert-false (eqv?   p i) "(eqv? <output-port> <input-port>) is #f")
(assert-false (equal? p i) "(equal? <output-port> <input-port>) is #f")

(test-summary)

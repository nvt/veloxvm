;;; VeloxVM Unit Tests - R7RS comparator-aware assoc and member
;;; Tests the optional third-argument forms of assoc and member,
;;; relies on the compiler's top-level shadowing pass to route call
;;; sites of the redefined names to the user binding.

(include "../unit-test-framework.scm")

;; Inline definitions from
;; languages/scheme-racket/runtime/r7rs-lists.scm. These shadow the
;; primitives 76 (assoc) and 73 (member) at top level.
(define (assoc obj alist . rest)
  (let ((cmp (if (null? rest) equal? (car rest))))
    (let loop ((l alist))
      (cond ((null? l) #f)
            ((cmp obj (car (car l))) (car l))
            (else (loop (cdr l)))))))

(define (member obj lst . rest)
  (let ((cmp (if (null? rest) equal? (car rest))))
    (let loop ((l lst))
      (cond ((null? l) #f)
            ((cmp obj (car l)) l)
            (else (loop (cdr l)))))))

(test-suite "R7RS comparator-aware assoc and member")

;; assoc 2-arg (default equal?)
(define al (list (cons 1 'a) (cons 5 'b) (cons 5 'e)))

(assert-equal (cons 1 'a) (assoc 1 al)  "assoc default finds first")
(assert-equal (cons 5 'b) (assoc 5 al)  "assoc default finds first matching key")
(assert-equal #f       (assoc 99 al) "assoc default returns #f when missing")

;; assoc with explicit = comparator
(assert-equal (cons 5 'b) (assoc 5 al =) "assoc with = comparator")
(assert-equal #f       (assoc 99 al =) "assoc with = returns #f when missing")

;; assoc with custom comparator (case-insensitive numeric)
(define string-al (list (cons "hi" 1) (cons "bye" 2)))
(assert-equal (cons "hi" 1)
              (assoc "hi" string-al)
              "assoc default uses equal? for strings")

;; member 2-arg (default equal?)
(assert-equal '(2 3) (member 2 '(1 2 3))   "member default finds element")
(assert-equal '(3)   (member 3 '(1 2 3))   "member default at end")
(assert-equal #f     (member 99 '(1 2 3))  "member default returns #f")

;; member with explicit = comparator
(assert-equal '(2 3) (member 2 '(1 2 3) =) "member with = comparator")

;; member with custom comparator: matches when first arg fits second
(define (divides? d n) (= 0 (remainder n d)))
(assert-equal '(6 7 8 9 10)
              (member 3 '(4 5 6 7 8 9 10) divides?)
              "member with custom: 3 divides 6")
(assert-equal #f
              (member 11 '(4 5 6 7 8 9 10) divides?)
              "member with custom: no element divisible by 11")

(test-summary)

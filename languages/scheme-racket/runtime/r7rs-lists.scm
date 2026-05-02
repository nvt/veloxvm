;;; ============================================================================
;;; VeloxVM R7RS List Helpers Runtime Library
;;; Copyright (c) 2026, RISE Research Institutes of Sweden AB
;;;
;;; R7RS-small list helpers and the comparator-aware variants of
;;; assoc and member. The latter two redefine names that exist as VM
;;; primitives; the compiler's top-level shadowing pass routes call
;;; sites to the user binding so the optional third-argument form
;;; works.
;;;
;;; The `include` directive currently has issues in nested scopes; copy
;;; these definitions directly into your program.
;;; ============================================================================

;; list-copy: shallow copy of a list (R7RS §6.4).
(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

;; list-tabulate: build a list of length n by calling proc on 0..n-1.
;; SRFI 1 style; not strictly R7RS but commonly bundled.
(define (list-tabulate n proc)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons (proc i) acc)))))

;; assoc with optional comparator (R7RS §6.4). Default is equal?.
;; Shadows the 2-arg primitive at top-level scope.
(define (assoc obj alist . rest)
  (let ((cmp (if (null? rest) equal? (car rest))))
    (let loop ((l alist))
      (cond ((null? l) #f)
            ((cmp obj (car (car l))) (car l))
            (else (loop (cdr l)))))))

;; member with optional comparator (R7RS §6.4). Default is equal?.
;; Shadows the 2-arg primitive at top-level scope.
(define (member obj lst . rest)
  (let ((cmp (if (null? rest) equal? (car rest))))
    (let loop ((l lst))
      (cond ((null? l) #f)
            ((cmp obj (car l)) l)
            (else (loop (cdr l)))))))

;;; End of r7rs-lists.scm

;; List Operations Demonstration
;; Shows: list construction, manipulation, predicates, and algorithms

(print "=== List Operations ===" #\Newline #\Newline)

;; 1. List construction
(print "1. List construction:" #\Newline)
(define list1 (list 1 2 3 4 5))
(define list2 (cons 'a (cons 'b (cons 'c '()))))
(print "   list1: " list1 #\Newline)
(print "   list2: " list2 #\Newline #\Newline)

;; 2. List predicates
(print "2. List predicates:" #\Newline)
(print "   (list? list1): " (list? list1) #\Newline)
(print "   (pair? list1): " (pair? list1) #\Newline)
(print "   (null? list1): " (null? list1) #\Newline)
(print "   (null? '()): " (null? '()) #\Newline #\Newline)

;; 3. List access and modification
(print "3. List access:" #\Newline)
(print "   (car list1): " (car list1) #\Newline)
(print "   (cdr list1): " (cdr list1) #\Newline)
(print "   (list-ref list1 2): " (list-ref list1 2) #\Newline)
(print "   (list-tail list1 3): " (list-tail list1 3) #\Newline #\Newline)

;; 4. List modification (mutation)
(print "4. List mutation:" #\Newline)
(define mutable-list (list 100 200 300))
(print "   Original: " mutable-list #\Newline)
(set-car! mutable-list 50)
(print "   After set-car!: " mutable-list #\Newline)
(set-cdr! mutable-list (list 150 250))
(print "   After set-cdr!: " mutable-list #\Newline #\Newline)

;; 5. List length
(print "5. List length:" #\Newline)
(define (my-length lst)
  (if (null? lst)
      0
      (+ 1 (my-length (cdr lst)))))

(print "   Length of " list1 ": " (my-length list1) #\Newline #\Newline)

;; 6. List reversal
(print "6. List reversal:" #\Newline)
(define (my-reverse lst)
  (let loop ((remaining lst)
             (result '()))
    (if (null? remaining)
        result
        (loop (cdr remaining)
              (cons (car remaining) result)))))

(print "   Original: " list1 #\Newline)
(print "   Reversed: " (my-reverse list1) #\Newline #\Newline)

;; 7. List filtering
(print "7. Filtering lists:" #\Newline)
(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (even? n) (= (modulo n 2) 0))
(define numbers (list 1 2 3 4 5 6 7 8 9 10))
(print "   Numbers: " numbers #\Newline)
(print "   Even numbers: " (filter even? numbers) #\Newline #\Newline)

;; 8. List mapping
(print "8. Mapping over lists:" #\Newline)
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))

(define (square x) (* x x))
(print "   Squares: " (my-map square (list 1 2 3 4 5)) #\Newline)

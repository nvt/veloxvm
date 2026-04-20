;; Closures and Higher-Order Functions Demonstration
;; Shows: lambda, lexical scoping, closures, letrec, higher-order functions

(print "=== Closures and Functions ===" #\Newline #\Newline)

;; 1. Simple closures - counter factory
(print "1. Counter factory (closures):" #\Newline)
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter1 (make-counter))
(define counter2 (make-counter))
(print "   Counter1: " (counter1) #\Newline)
(print "   Counter1: " (counter1) #\Newline)
(print "   Counter2: " (counter2) #\Newline)
(print "   Counter1: " (counter1) #\Newline #\Newline)

;; 2. Closures with parameters - make adder
(print "2. Make-adder (closures with parameters):" #\Newline)
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))
(print "   add5(3) = " (add5 3) #\Newline)
(print "   add10(3) = " (add10 3) #\Newline #\Newline)

;; 3. LETREC - Mutual recursion
(print "3. Mutual recursion with letrec (even?/odd?):" #\Newline)
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (print "   Is 10 even? " (even? 10) #\Newline)
  (print "   Is 89 even? " (even? 89) #\Newline))
(print "" #\Newline)

;; 4. Higher-order functions - compose
(print "4. Function composition:" #\Newline)
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (add1 x) (+ x 1))
(define square-then-add1 (compose add1 square))
(define add1-then-square (compose square add1))

(print "   square-then-add1(5) = " (square-then-add1 5) #\Newline)
(print "   add1-then-square(5) = " (add1-then-square 5) #\Newline #\Newline)

;; 5. Currying
(print "5. Currying example:" #\Newline)
(define (curried-add x)
  (lambda (y)
    (lambda (z)
      (+ x y z))))

(define result ((( curried-add 1) 2) 3))
(print "   (((curried-add 1) 2) 3) = " result #\Newline)

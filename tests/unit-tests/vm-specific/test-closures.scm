;; VeloxVM Unit Tests - Closures
;;
;; Covers both immutable capture (make-adder, currying) and mutable
;; capture via the box rewrite (counters, shared state through cons).

(include "../unit-test-framework.scm")

(test-suite "Closures")

;; make-adder: classic capture-by-value pattern.
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add5 (make-adder 5))
(define add10 (make-adder 10))

(assert-equal 8 (add5 3) "make-adder: 5 + 3 = 8")
(assert-equal 12 (add5 7) "make-adder: 5 + 7 = 12")
(assert-equal 13 (add10 3) "make-adder: 10 + 3 = 13")
(assert-equal 0 (add10 -10) "make-adder with negative arg")

;; Two distinct closures over different captured values don't interfere.
(assert-equal 8 (add5 3) "add5 still works after add10 calls")

;; Multiple captures in one closure.
(define (make-mixer a b)
  (lambda (x) (+ (* a x) b)))

(define line2x+3 (make-mixer 2 3))
(define line5x-1 (make-mixer 5 -1))

(assert-equal 7 (line2x+3 2) "2*2 + 3 = 7")
(assert-equal 13 (line2x+3 5) "2*5 + 3 = 13")
(assert-equal 14 (line5x-1 3) "5*3 - 1 = 14")

;; Closure captures a list (heap value).
(define (make-prepender prefix)
  (lambda (lst) (cons prefix lst)))

(define add-zero (make-prepender 0))
(assert-equal '(0 1 2) (add-zero '(1 2)) "closure captures list value")

;; Triple-nested lambda: outer captures from outermost.
(define (make-curried-add a)
  (lambda (b)
    (lambda (c) (+ a b c))))

(define add-from-1 (make-curried-add 1))
(define add-1-2 (add-from-1 2))
(assert-equal 6 (add-1-2 3) "curried 1 + 2 + 3 = 6")
(assert-equal 13 (add-1-2 10) "curried 1 + 2 + 10 = 13")

;; Mutable capture: counter state survives across calls because the
;; captured variable lives in a heap box.
(define (make-counter)
  (let ((c 0))
    (lambda ()
      (set! c (+ c 1))
      c)))

(define ctr (make-counter))
(assert-equal 1 (ctr) "counter first call returns 1")
(assert-equal 2 (ctr) "counter second call returns 2")
(assert-equal 3 (ctr) "counter third call returns 3")

;; Two counters from the same factory hold independent state.
(define ctr2 (make-counter))
(assert-equal 1 (ctr2) "second counter starts at 1")
(assert-equal 4 (ctr) "first counter unaffected")
(assert-equal 2 (ctr2) "second counter increments independently")

;; Two closures sharing the same captured-and-mutated binding see each
;; other's writes -- this is the case that requires shared box storage,
;; not just copy-on-capture.
(define (make-pair)
  (let ((shared 0))
    (cons (lambda () (set! shared (+ shared 1)) shared)
          (lambda () shared))))

(define p (make-pair))
(define inc (car p))
(define rd (cdr p))

(assert-equal 1 (inc) "shared incrementer returns 1")
(assert-equal 2 (inc) "shared incrementer returns 2")
(assert-equal 2 (rd) "reader sees the same shared value")
(assert-equal 3 (inc) "shared incrementer returns 3 after reader call")
(assert-equal 3 (rd) "reader sees latest shared value")

;; A primitive's name (count is a VM primitive) is a perfectly valid
;; local binding name; the box rewrite must capture the local, not the
;; primitive.
(define (make-named-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define nc (make-named-counter))
(assert-equal 1 (nc) "primitive-named local: first call")
(assert-equal 2 (nc) "primitive-named local: second call")

;; Operator position is a sub-expression that returns a callable.
;; The compiler lifts a compound operator into a form-ref; the runtime
;; recognizes a sub-expression operator and invokes the resulting
;; lambda/closure.
(define (get-doubler) (lambda (x) (* x 2)))
(assert-equal 14 ((get-doubler) 7) "call result of a 0-arg getter")

;; Named let -- rewrites to letrec which rewrites to a captured-and-mutated
;; outer parameter holding the recursive function. The recursive call site
;; ((box-ref loop) ...) hits the same operator-as-sub-expression path.
(define (count-down n)
  (let loop ((i n) (acc 0))
    (if (= i 0) acc (loop (- i 1) (+ acc 1)))))
(assert-equal 5 (count-down 5) "named let counting down")
(assert-equal 0 (count-down 0) "named let zero iterations")

(test-summary)

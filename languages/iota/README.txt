Example translations.

Iota source 1.

x = 10 * 9;
if (x > 8 * 7) {
  print("x is high\n");
} else {
  print("x is low\n");
}

Translated Scheme source 1.

(define x (* 10 9))
(if (> x (* 8 7))
  (print "x is high" #\Newline)
  (print "x is log" #\Newline)

Iota source 2.

x = array(100);
x[15] = 1904;

Translated Scheme source 2.

(define x (make-vector 100))
(vector-set! x 15 1904)

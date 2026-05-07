;;; VeloxVM Unit Tests - R7RS input-port-open? / output-port-open?
;;; R7RS §6.13.1: returns #t iff the argument is an open port of the
;;; right direction; otherwise #f (including non-port arguments and
;;; closed ports of the right direction).

(include "../unit-test-framework.scm")

(test-suite "R7RS port-open predicates")

;; stdin / stdout are open and have the expected directions.
(assert-equal #t (input-port-open?  (current-input-port))
              "input-port-open? on open stdin")
(assert-equal #t (output-port-open? (current-output-port))
              "output-port-open? on open stdout")

;; Wrong direction: stdout is not an input port; stdin is not an output port.
(assert-equal #f (input-port-open?  (current-output-port))
              "input-port-open? rejects an output port")
(assert-equal #f (output-port-open? (current-input-port))
              "output-port-open? rejects an input port")

;; Non-ports are #f, never errors.
(assert-equal #f (input-port-open?  42)        "input-port-open? on integer")
(assert-equal #f (input-port-open?  "x")       "input-port-open? on string")
(assert-equal #f (input-port-open?  '())       "input-port-open? on empty list")
(assert-equal #f (output-port-open? 42)        "output-port-open? on integer")
(assert-equal #f (output-port-open? #t)        "output-port-open? on boolean")

;; Open then close a file port; the predicate flips.
(define out (open-output-file "/tmp/vm-test-open-pred.tmp"))
(assert-equal #t (output-port-open? out) "fresh output port is open")
(close-output-port out)
(assert-equal #f (output-port-open? out) "closed output port is not open")

(define in (open-input-file "/tmp/vm-test-open-pred.tmp"))
(assert-equal #t (input-port-open? in) "fresh input port is open")
(close-input-port in)
(assert-equal #f (input-port-open? in) "closed input port is not open")

(test-summary)

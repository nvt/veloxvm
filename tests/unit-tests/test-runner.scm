;; Test Framework - Test Runner
;; Manages test execution, state, and result collection
;; Coordinates between assertions and reporting

;; NOTE: Requires test-assertions.scm and test-reporter.scm to be loaded

;; Test state - counters and suite tracking
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *tests-skipped* 0)
(define *current-suite* "")

;; Per-suite statistics: ((suite-name passed failed skipped) ...)
(define *suite-stats* '())

;; Record a test result - dispatches to reporter and updates state
(define (record-result result)
  (let ((type (car result)))
    (cond
      ((eq? type 'pass)
       (set! *tests-passed* (+ *tests-passed* 1))
       (report-pass (cadr result)))
      ((eq? type 'fail)
       (set! *tests-failed* (+ *tests-failed* 1))
       (report-fail (cadr result) (caddr result)))
      ((eq? type 'skip)
       (set! *tests-skipped* (+ *tests-skipped* 1))
       (report-skip (cadr result) (caddr result)))
      (else
       (error "Unknown test result type" type)))))

;; Backward-compatible assertion API - wraps pure assertions with recording
(define (assert-equal expected actual description)
  (record-result (check-equal expected actual description)))

(define (assert-not-equal not-expected actual description)
  (record-result (check-not-equal not-expected actual description)))

(define (assert-true value description)
  (record-result (check-true value description)))

(define (assert-false value description)
  (record-result (check-false value description)))

(define (assert-truthy value description)
  (record-result (check-truthy value description)))

(define (assert-skip description reason)
  (record-result (make-skip description reason)))

;; NOTE: assert-error is defined by platform adapters (exception handling is platform-specific)

;; Test suite management
(define (test-suite name)
  ;; Save stats from previous suite if any
  (if (not (equal? *current-suite* ""))
      (set! *suite-stats*
            (cons (list *current-suite* *tests-passed* *tests-failed* *tests-skipped*)
                  *suite-stats*))
      #f)
  ;; Start new suite
  (set! *current-suite* name)
  (set! *tests-passed* 0)
  (set! *tests-failed* 0)
  (set! *tests-skipped* 0)
  (report-suite-header name))

;; Display test summary
(define (test-summary)
  ;; Save final suite stats
  (if (not (equal? *current-suite* ""))
      (set! *suite-stats*
            (cons (list *current-suite* *tests-passed* *tests-failed* *tests-skipped*)
                  *suite-stats*))
      #f)

  ;; Calculate totals using recursive helper (for-each is broken in VeloxVM)
  (define (sum-stats stats passed failed skipped)
    (if (null? stats)
        (report-summary *suite-stats* passed failed skipped)
        (let ((suite-stat (car stats)))
          (sum-stats (cdr stats)
                     (+ passed (cadr suite-stat))
                     (+ failed (caddr suite-stat))
                     (+ skipped (cadddr suite-stat))))))

  (sum-stats *suite-stats* 0 0 0))

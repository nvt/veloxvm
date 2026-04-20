;; Test Framework - Test Reporter
;; Handles all output formatting and display
;; Separates presentation from logic

;; NOTE: Requires test-assertions.scm for value->string
;; NOTE: Platform adapters must define 'print' procedure

;; Helper: Format suite context for failure messages
(define (format-suite-context)
  (if (equal? *current-suite* "")
      ""
      (string-append "(" *current-suite* ") ")))

;; Report individual test results
(define (report-pass description)
  (print "  PASS: " description #\newline))

(define (report-fail description detail-lines)
  (print "  FAIL: " (format-suite-context) description #\newline)
  (for-each (lambda (line) (print "    " line #\newline))
            detail-lines))

(define (report-skip description reason)
  (print "  SKIP: " description " - " reason #\newline))

;; Report suite header
(define (report-suite-header name)
  (print #\newline "=== Test Suite: " name " ===" #\newline))

;; Report overall summary
(define (report-summary suite-stats total-passed total-failed total-skipped)
  (print #\newline)
  (print "==============================" #\newline)
  (print "Test Summary" #\newline)
  (print "==============================" #\newline)
  (print #\newline)

  ;; Per-suite results (in reverse order - chronological)
  ;; Using recursive helper since for-each is broken in VeloxVM
  (define (print-suite-results stats)
    (if (not (null? stats))
        (let* ((suite-stat (car stats))
               (name (car suite-stat))
               (passed (cadr suite-stat))
               (failed (caddr suite-stat))
               (skipped (cadddr suite-stat))
               (total (+ passed failed skipped)))
          (print "  " name ": ")
          (if (= total 0)
              (print "0/0 ok")
              (print passed "/" total
                     (if (= failed 0) " ok" " fail")))
          (if (> skipped 0)
              (print " [" skipped " skipped]")
              #f)
          (print #\newline)
          (print-suite-results (cdr stats)))
        #f))

  (print "Suite Results:" #\newline)
  (print-suite-results (reverse suite-stats))

  (print #\newline)
  (print "Total:   " (+ total-passed total-failed total-skipped) #\newline)
  (print "Passed:  " total-passed #\newline)
  (print "Failed:  " total-failed #\newline)
  (if (> total-skipped 0)
      (print "Skipped: " total-skipped #\newline)
      #f)

  (if (= total-failed 0)
      (print "ALL TESTS PASSED" #\newline)
      (print "SOME TESTS FAILED" #\newline))

  (print "==============================" #\newline))

#lang racket

;; Copyright (c) 2025, RISE Research Institutes of Sweden AB
;;
;; Runs every tests/test-*.rkt file as a subprocess, counts FAILURE
;; markers in their output, and exits non-zero if any check failed or
;; any file crashed.

(require racket/runtime-path
         racket/port)

(define-runtime-path here ".")

(define (find-test-files)
  (sort
   (for/list ([p (in-list (directory-list here))]
              #:when (let ([s (path->string p)])
                       (and (regexp-match? #rx"^test-.*\\.rkt$" s)
                            (not (equal? s "run-all-tests.rkt")))))
     (build-path here p))
   path<?))

(define failure-rx #px"(?m:^FAILURE$)")

(define (count-failures output)
  (length (regexp-match* failure-rx output)))

(define (run-test-file path)
  (define ports (process* (find-executable-path "racket") (path->string path)))
  (match-define (list stdout stdin _pid stderr ctl) ports)
  (close-output-port stdin)
  (define stdout-str (port->string stdout))
  (define stderr-str (port->string stderr))
  (ctl 'wait)
  (define exit-code (ctl 'exit-code))
  (close-input-port stdout)
  (close-input-port stderr)
  (values exit-code (string-append stdout-str stderr-str)))

(displayln "")
(displayln "========================================")
(displayln "VeloxVM Racket Compiler - Test Suite")
(displayln "========================================")
(displayln "")

(define results
  (for/list ([file (in-list (find-test-files))])
    (define-values (exit-code output) (run-test-file file))
    (define fails (count-failures output))
    (define crashed? (not (zero? exit-code)))
    (define status (if (or crashed? (positive? fails)) 'fail 'pass))
    (define name (path->string (file-name-from-path file)))
    (printf "~a  ~a~a~a~n"
            (if (eq? status 'pass) "PASS" "FAIL")
            name
            (if (positive? fails) (format "  (~a failure(s))" fails) "")
            (if crashed? (format "  (exit ~a)" exit-code) ""))
    ;; Surface details for failing files so the output is actionable.
    (when (eq? status 'fail)
      (for ([line (in-list (string-split output "\n"))]
            #:when (regexp-match? #px"^(FAILURE|name:|location:|message:|actual:|expected:|params:)" line))
        (printf "    ~a~n" line)))
    (hash 'file file 'status status 'fails fails 'crashed? crashed?)))

(define total-fails (apply + (map (lambda (r) (hash-ref r 'fails)) results)))
(define failed-files (filter (lambda (r) (eq? (hash-ref r 'status) 'fail)) results))

(displayln "")
(displayln "========================================")
(cond
  [(null? failed-files)
   (printf "All tests passed  (~a files, 0 failures)~n" (length results))]
  [else
   (printf "~a failing check(s) across ~a file(s)~n"
           total-fails (length failed-files))])
(displayln "========================================")
(displayln "")

(exit (if (null? failed-files) 0 1))

(define si (system-info))

(println "VM version: " (vector-ref si 0))
(println "OS version: " (vector-ref si 1))
(println "Loaded programs: " (get-programs))
(println "Available devices: " (get-devices))

(program-info)

;; (load-program "tests/math.vm");

(define stats-call
  (lambda (num stats my-list)
    (println "List: " my-list)
    (println "=== Thread statistics ===")
    (println "Schedulings: " (vector-ref stats 0))
    (println "Function calls: " (vector-ref stats 1))
    (println "Allocated total: " (vector-ref stats 2))
    (when (< num 100)
      (stats-call (+ num 1) (thread-stats) (cons (random 100) my-list)))))

(stats-call 0 (thread-stats) (list (random 100)))

;;; sort.sld -- stable sorting for lists and vectors, exported as
;;; (velox sort). The library sits under the compiler's runtime/ search
;;; root, so (import (velox sort)) resolves without -I.
;;;
;;; The R6RS / SRFI-132 procedures, comparator first:
;;;
;;;   (list-sort    less? list)  -> a new sorted list
;;;   (vector-sort  less? vec)   -> a new sorted vector (vec is unchanged)
;;;   (vector-sort! less? vec)   -> sorts vec in place; returns unspecified
;;;
;;; `less?` must be a strict ordering: true iff its first argument comes
;;; before its second. Elements that compare equal keep their input order.
;;;
;;; The sort is a binary-stable insertion sort over a vector. It is
;;; quadratic, but it needs no scratch vector and stays within two flat
;;; loops. Both matter more here than the asymptotics: the arrays sorted
;;; on a 32 kB-class device are small, and the nested loop composition a
;;; bottom-up merge sort needs does not keep its tail calls flat on the
;;; VM's shallow context stack. Sorting large arrays calls for a native
;;; vector-sort! primitive rather than a bigger library.

(define-library (velox sort)
  (import (scheme base))
  (export list-sort vector-sort vector-sort!)
  (begin

    (define (vector-sort! less? vec)
      (let ((n (vector-length vec)))
        ;; Grow the sorted prefix vec[0,i) one element at a time.
        (let outer ((i 1))
          (when (< i n)
            (let ((key (vector-ref vec i)))
              ;; Shift elements greater than key one slot right and drop
              ;; key into the gap. Stopping once the left neighbour is no
              ;; longer greater keeps equal elements in their input order.
              (let inner ((j i))
                (cond ((and (> j 0) (less? key (vector-ref vec (- j 1))))
                       (vector-set! vec j (vector-ref vec (- j 1)))
                       (inner (- j 1)))
                      (else
                       (vector-set! vec j key)))))
            (outer (+ i 1))))))

    (define (vector-sort less? vec)
      (let ((n (vector-length vec)))
        (if (< n 1)
            (make-vector 0 0)
            (let ((copy (make-vector n (vector-ref vec 0))))
              (let loop ((i 0))
                (when (< i n)
                  (vector-set! copy i (vector-ref vec i))
                  (loop (+ i 1))))
              (vector-sort! less? copy)
              copy))))

    (define (list-sort less? lst)
      ;; Sorting through the vector core costs one vector, where a
      ;; list-native sort would cons its way through the input.
      (let ((v (list->vector lst)))
        (vector-sort! less? v)
        (vector->list v)))))

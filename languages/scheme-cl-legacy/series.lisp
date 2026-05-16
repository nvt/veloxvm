; Copyright (c) 2012-2017, RISE SICS AB
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. Neither the name of the Institute nor the names of its contributors
;    may be used to endorse or promote products derived from this software
;    without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE INSTITUTE AND CONTRIBUTORS ``AS
; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
; INSTITUTE OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Author: Nicolas Tsiftes <nvt@acm.org>

(provide :series)

(defmacro series->hash (series-name)
  `(get ,series-name 'series-hash))

(defmacro series->vector (series-name)
  `(get ,series-name 'series-vector))

(defmacro series-counter (series-name)
  `(get ,series-name 'series-counter))

(defun series-capacity (series-name)
  (car (array-dimensions (series->vector series-name))))

(defun create-series (name &key
			     (comparator 'equal)
			     (element-type 'string)
			     (max-size 16384))
  (setf (series->hash name)
	(make-hash-table :test comparator))
  (setf (series->vector name)
	(make-array max-size :fill-pointer 0 :element-type element-type))
  (setf (series-counter name) 0))

(defun insert-series (series item)
  (let ((id (series-counter series))
	(max_id (1- (series-capacity series))))
    (when (> id max_id)
      (error "Series ~a cannot exceed ~a items." series max_id))
    (setf (gethash item (series->hash series)) id)
    (vector-push item (series->vector series))
    (incf (series-counter series))
    id))

(defun get-series-id (series item)
  (multiple-value-bind (value)
      (gethash item (series->hash series)) value))

(defun commit-string (item file)
  "commit a string in a format where the first byte specifies the length."
  (write-byte (length item) file)
  (loop for ch across item
       do (write-byte (char-code ch) file)))

(defun commit-series (series file &optional (output-function #'commit-string))
  (let ((amount (series-counter series)))
    (write-byte amount file)
    (loop for item across (series->vector series)
       do (funcall output-function item file))))

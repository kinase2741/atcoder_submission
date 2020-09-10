(defmacro read-to-list ()
  `(read-from-string
    (concatenate 'string "(" (read-line) ")")))

;; test


(defun count-happy-people (s)
  (labels ((inner (s &optional (cnt 0))
             (cond
               ((null (cdr s)) cnt)
               ((char-equal
                 (first s)
                 (second s))
                (inner (rest s) (1+ cnt)))
               (t
                (inner (rest s) cnt)))))
    (inner s)))

(defun solve (n k s)
  (let ((start (count-happy-people s)))
    (min (+ start (* k 2))
         (1- n))))


(defun main ()
  (let* ((n (read))
         (k (read))
         (s (concatenate 'list (read-line))))
    (format t "~a~%" (solve n k s))))

(main)

(defmacro read-to-list ()
  `(read-from-string
    (concatenate 'string "(" (read-line) ")")))

(defun solve (n)
  n)


(defun main ()
  (let ((n (read)))
    (format t "~a~%" (solve n))))

(main)

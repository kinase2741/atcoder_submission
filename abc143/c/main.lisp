(defmacro read-to-list ()
  `(read-from-string
    (concatenate 'string "(" (read-line) ")")))

(defun solve (s &optional (res 0) (prev #\0))
  (if (null s)
      res
      (if (char-equal (first s)
                      prev)
          (solve (rest s) res (first s))
          (solve (rest s) (1+ res) (first s)))))


(defun main ()
  (let ((n (read))
        (s (concatenate 'list (read-line))))
    (format t "~a~%" (solve s))))


(main)







;;; Write code here

(defun solve (d time s)
  (if (<= (/ d s) time)
      "Yes"
      "No"))


(defun main ()
  (let ((d (read))
        (time (read))
        (s (read)))
    (format t "~a~%" (solve d time s))))

(main)

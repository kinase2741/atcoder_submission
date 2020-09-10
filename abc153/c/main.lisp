(defun solve (n k h)
  (let ((h-sorted (sort (copy-seq h) #'>)))
    (if (> n k)
        (apply #'+ (subseq h-sorted k))
        0)))


(defun main ()
  (let ((n (read))
        (k (read))
        (h (read-from-string (concatenate 'string "(" (read-line)")"))))
    (format t "~a~%" (solve n k h))))

(main)

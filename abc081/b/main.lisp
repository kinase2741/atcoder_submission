#|
------------------------------------
|               Body               |
------------------------------------
|#


(defun solve (a)
  (if (some #'oddp a)
      0
      (1+ (solve (mapcar (lambda (x)
                           (floor x 2))
                         a)))))


(defun main ()
  (let ((n (read)))
    (let ((a (loop repeat n collect (read))))
      (format t "~a~&" (solve a)))))

#-swank (main)

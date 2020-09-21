;;; body

(defconstant +inf+ 10000000)

(defun main ()
  (let ((n (read)))
    (declare ((unsigned-byte 8) n))
    (let ((a (loop repeat n collect (read))))
      (declare (list a))
      (princ
       (reduce #'min
               (mapcar (lambda (y)
                         (reduce #'+
                                 (mapcar (lambda (x)
                                           (expt (- x y) 2))
                                         a)))
                       (loop for y of-type (signed-byte 16) from -100 to 100 collect y))))
      (fresh-line))))

#-swank (main)

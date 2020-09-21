;;; body

(defun solve (n k x)
  (flet ((calc-dist (i-left i-right)
           (let ((left (aref x i-left))
               (right (aref x (1- i-right))))
             (min (+ (abs left) (abs (- right left)))
                  (+ (abs right) (abs (- right left)))))))
    (if (= n 1)
        (abs (aref x 0))
        (reduce #'min 
                (let ((acc nil))
                  (loop with i = 0  while (<= (+ i k) n) do
                       (push (calc-dist i
                                        (+ i k))
                             acc)
                       (incf i))
                  acc)))))

(defun main ()
  (let ((n (read))
        (k (read)))
    (let ((x (make-array n
                         :initial-contents (loop repeat n collect (read)))))
      (princ (solve n k x))
      (fresh-line))))

#-swank (main)

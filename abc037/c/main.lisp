;;; body

(defun solve (n k arr)
  (declare ((array fixnum 1) arr)
           (fixnum n k))
  (let ((cum (make-array (1+ n)
                         :element-type 'fixnum
                         :initial-element 0)))
    (declare ((array fixnum 1) cum))
    (dotimes (i n)
      (setf (aref cum (1+ i))  (+ (aref cum i)
                                  (aref arr i))))
    (let ((res 0)
          (start 0))
      (loop while (<= (+ start k) n) do
           (incf res (- (aref cum (+ start k))
                        (aref cum start)))
           (incf start)
         finally
           (return res)))))

(defun main ()
  (let ((n (read))
        (k (read)))
    (declare (fixnum n k))
    (let ((arr (make-array n
                           :element-type 'fixnum
                           :initial-contents (loop repeat n collect (read)))))
      (declare ((array fixnum 1) arr))
      (princ (solve n k arr))
      (fresh-line))))

#-swank (main)

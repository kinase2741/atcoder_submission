

(defun solve (n x y)
  (labels ((calc-min-distance (start end x y)
             (assert (< start end))
             (min
              (abs (- end start))
              (+ (abs (- start y))
                 (abs (- end x))
                 1)
              (+ (abs (- start x))
                 (abs (- end y))
                 1))))
    (let ((res (make-array n)))
      (loop for i from 1 to (1- n)
         do (loop for j from (1+ i) to n
               do (incf (aref res (calc-min-distance i j x y)))))
      res)))


(defun main ()
  (let ((n (read))
        (x (read))
        (y (read)))
    (let ((ans (solve n x y)))
      (dotimes (i (1- n))
        (fresh-line)
        (princ (aref ans (1+ i))))
      (fresh-line))))

(main)

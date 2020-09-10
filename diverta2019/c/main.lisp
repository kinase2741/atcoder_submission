(defun count-str-containing-ab (str)
  (let ((counter 0))
    (loop for i from 0 below (1- (length str)) do
         (when (and (char= (elt str i) #\A)
                    (char= (elt str (1+ i)) #\B))
           (incf counter)))
    counter))

(defun a-last-p (str)
  (char= (elt str (1- (length str))) #\A))

(defun b-first-p (str)
  (char= (elt str 0) #\B))

(defun solve (xs)
  (let ((x (count-if #'b-first-p xs))
        (y (count-if #'a-last-p xs))
        (w (count-if (lambda (x)
                       (and
                        (b-first-p x)
                        (a-last-p x)))
                     xs))
        (z (apply #'+ (mapcar #'count-str-containing-ab xs))))
    (cond
      ((> (+ x y) 0) (+ w
                        (min x y)
                        z))
      (t             (+ (max (1- w)
                             0)
                        z)))))




(defun main ()
  (let* ((n (read))
         (xs nil))
    (dotimes (i n)
      (push (read-line) xs))
    (format t "~a~%" (solve (reverse xs)))))

(main)

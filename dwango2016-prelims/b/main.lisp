;;; Utils

(defmacro read-numbers-to-list ()
  `(read-from-string
    (concatenate 'string "(" (read-line) ")")))


(defmacro read-line-to-array (dimension)
  (if (< dimension 0)
      (error "invalid arguments for dimension.")
      `(make-array ,dimension :initial-contents (read-numbers-to-list))))

(defmacro read-line-to--char-list ()
  `(concatenate 'list (read-line)))

(defmethod make-cumlative-sum ((sequence list))
  (labels ((inner (sequence &optional (acc '(0)))
             (if (null sequence)
                 (reverse acc)
                 (inner (rest sequence) (cons (+ (first sequence)
                                                 (first acc))
                                              acc)))))
    (inner sequence)))





(defmethod princ-for-each-line ((sequence list))
  (labels ((inner (sequence)
             (if (null sequence)
                 nil
                 (progn
                   (fresh-line)
                   (princ (first sequence))
                   (inner (rest sequence))))))
    (inner sequence)))

(defmethod princ-for-each-line ((sequence array))
  (dotimes (i (length sequence))
    (fresh-line)
    (princ (aref sequence i))))



;;; Write code here

(defparameter *inf* (1+ (expt 10 9)))

(defun solve (n xs)
  (assert (= (length xs) (1+ n)))
  (let ((ans (make-array n)))
    (dotimes (i n)
      (setf (aref ans i) (min (first xs)
                              (second xs)))
      (pop xs))
    (assert (every #'plusp ans))
    ans))



(defun main ()
  (let* ((n (read))
         (xs nil))
    (push *inf* xs)
    (loop repeat (1- n) do
         (push (read) xs))
    (push *inf* xs)
    (let ((ans (solve n (reverse xs))))
      (assert (= (length ans) n))
      (dotimes (i (1- n))
        (format t "~a " (aref ans i)))
      (princ (aref ans (1- n)))
      (fresh-line))))


(main)

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

(defun solve (n x weights)
  (let ((min-weight (reduce #'min weights))
        (counter n)
        (weight-rest (- x (reduce #'+ weights))))
    (assert (>= weight-rest 0))
    (+ counter
       (floor weight-rest min-weight))))


(defun main ()
  (let* ((n (read))
         (x (read))
         (weights (coerce (loop repeat n collect (read)) 'vector)))
    (assert (and
             (every #'numberp weights)))
    (format t "~a~%" (solve n x weights))))

(main)

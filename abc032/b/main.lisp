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

(defun make-new-string (xs start size)
  (subseq xs start (+ start size)))

(defun solve (xs k)
  (if (< (length xs) k)
      0
      (length
       (remove-duplicates
        (mapcar (lambda (start)
                  (make-new-string xs start k))
                (loop for i from 0 to (- (length xs) k) collect i))
        :test #'equal))))


(defun main ()
  (let ((xs (read-line))
        (k (read)))
    (format t "~a~%" (solve xs k))))

(main)

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

(defun solve (n xs)
  (labels ((dp (i y)
             ;; 部分わをyにできるか？
             (cond
               ((zerop i)
                (if (zerop y)
                    t
                    nil))
               ((minusp y) nil)
               (t
                (or (dp (1- i) y)
                    (dp (1- i) (- y (nth (1- i) xs))))))))
    (if (= n 1)
        (first xs)
        (let ((res-max (apply #'+ xs)))
          (loop for res from (ceiling res-max 2) to res-max do
               (when (dp n res)
                 (return res))
             finally
               (return -1))))))



(defun main ()
  (let* ((n (read))
         (xs nil))
    (loop for i below n do
         (push (read) xs))
    (format t "~a~%" (solve n (reverse xs)))))

(main)

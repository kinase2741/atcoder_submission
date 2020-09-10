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

(defun solve (n m queries)
  (declare (optimize speed))
  (declare (type fixnum n)
           (type fixnum m))
  (let ((parents (make-array n :initial-element -1)))
    (labels ((find-parents (x)
               (if (< (aref parents x) 0)
                   x
                   (progn
                     ;; memoize
                     (setf (aref parents x) (find-parents (aref parents x)))
                     (aref parents x))))
             (get-size-of-tree (x)
               (abs (aref parents (find-parents x)))))
      (loop repeat m
         do
           (progn
             (let ((x (find-parents (first (first queries))))
                   (y (find-parents (rest (first queries)))))
               (if (> x y)
                   (rotatef x y))
               (unless (= x y)
                 (incf (aref parents x) (aref parents y))
                 (setf (aref parents y) x)))
             (pop queries)))
      (reduce #'max
              (mapcar #'get-size-of-tree (loop for i from 0 below n collect i))))))


(defun main ()
  (let ((n (read))
        (m (read))
        (queries nil))
    (loop repeat m
       do (progn
            (push (cons
                   (1- (read))
                   (1- (read)))
                  queries)))
    (format t "~a~%" (solve n m queries))))

(main)

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


(defparameter *table*
  '((#\a . #\b)
    (#\b . #\c)
    (#\c . #\a)))

(defun orderd-p (xs)
  (assert (every #'alphanumericp xs))
  (cond
    ((null (rest xs)) t)
    ((char-equal (rest (assoc (first xs) *table*)) (second xs)) (orderd-p (rest xs)))
    (t nil)))

;;; Write code here

(defun solve (n s) 
  (let* ((proc-count (truncate (1- n) 2))
         (k (mod proc-count 3)))
    (if
     (and
      (oddp n)
      (case k
        (0 (char-equal (first s) #\b))
        (1 (char-equal (first s) #\a))
        (2 (char-equal (first s) #\c))
        (otherwise nil))
      (orderd-p s))
     proc-count
     
     -1)))


(defun main ()
  (let ((n (read))
        (s (concatenate 'list (read-line))))
    (assert (and (numberp n)
                 (every #'alphanumericp s)))
    (format t "~a~%" (solve n s))))

(main)

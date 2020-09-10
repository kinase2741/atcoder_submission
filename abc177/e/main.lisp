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

(defun factorize-number-to-lst (x)
  (assert (and (numberp x)
               (plusp x)))
  (labels
      ((inner (x)
         (if (= x 1)
             1)
         (let ((res nil)
               (f 2))
           (loop while (<= (* f f) x) do
                (if (zerop (mod x f))
                    (progn
                      (setf x (truncate x f))
                      (push f res))
                    (incf f))
              finally
                (progn
                  (when (/= x 1)
                    (push x res))
                  (return (reverse res)))))))
    (inner x)))


(defun setwise-p (xs)
  (=
   (reduce #'gcd xs)
   1))


(defun step-sum (sequence start end step)
  (assert (arrayp sequence))
  (labels ((inner (start end step &optional (acc 0))
             (if (< start end)
                 (inner (+ start step) end step (+ acc
                                                   (aref sequence start)))
                 acc)))
    (inner start end step)))

(defun pairwise-p (n xs)
  (assert (and (plusp n)
               (every #'numberp xs)))
  (let ((memo (make-array (1+ (expt 10 6))::initial-element 0))
        (flag t))
    (dotimes (i n)
      (incf (aref memo (first xs)))
      (pop xs))
    (let ((f 2))
      (loop while (<= f (expt 10 6)) do
           (when (> (step-sum memo f (1+ (expt 10 6)) f) 1)
             (setf flag nil))
           (incf f)
         finally
           (return flag)))))



(defun solve (n a)
  (cond
    ((pairwise-p n a) "pairwise coprime")
    ((setwise-p a)  "setwise coprime")
    (t "not coprime")))


(defun main ()
  (let ((n (read))
        (a (sort (read-numbers-to-list) #'<)))
    (format t "~a~%" (solve n a))))

(main)

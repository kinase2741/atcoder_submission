;;; Utils

(defmacro read-numbers-to-list ()
  `(read-from-string
    (concatenate 'string "(" (read-line) ")")))


(defmacro read-line-to-array (dimension)
  (if (< dimension 0)
      (error "invalid arguments for dimension.")
      `(make-array ,dimension :initial-contents (read-numbers-to-list))))

(defmacro read-line-to-char-list ()
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

(defun count-difference (str1 str2)
  (assert (= (length str1) (length str2)))
  (labels ((inner (s1 s2 &optional (cnt 0))
             (cond
               ((null s1) cnt)
               ((char-equal (first s1) (first s2))
                (inner (rest s1) (rest s2) cnt))
               (t
                (inner (rest s1) (rest s2) (1+ cnt))))))
    (inner str1 str2)))



(defun solve (bigger smaller)
  (let ((b-len (length bigger))
        (s-len (length smaller))
        (k 0)
        (ans 10001))
    (loop while (< (+ k s-len) b-len)
       do
         (progn
           (setq ans (min ans (count-difference (subseq bigger 0 s-len)
                                                smaller)))
           (pop bigger)
           (incf k))
       finally
         (return ans))))




(defun main ()
  (let ((str1 (read-line-to-char-list))
        (str2 (read-line-to-char-list)))
    (format t "~a~%" (solve str1 str2))))

(main)

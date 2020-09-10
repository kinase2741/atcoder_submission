;;; Utils

(defmacro read-numbers-to-list (size)
  `(loop repeat ,size collect (read)))



(defmacro read-numbers-to-array (size)
  `(if (< ,size 0)
       (error "invalid size")
       (make-array ,size :initial-contents (read-numbers-to-list ,size))))


(defmethod make-cumlative-sum ((sequence list))
  (labels ((inner (sequence &optional (acc '(0)))
             (if (null sequence)
                 (reverse acc)
                 (inner (rest sequence) (cons (+ (first sequence)
                                                 (first acc))
                                              acc)))))
    (inner sequence)))


(defmethod make-cumlative-sum ((sequence array))
  (declare (type (simple-array fixnum) sequence))
  (the array
       (let* ((n (length sequence))
              (acc (make-array (1+ n) :element-type 'integer :initial-element 0)))
         (loop for i below n do
              (setf (aref acc (1+ i)) (+ (aref sequence i)
                                         (aref acc i)))
            finally
              (return acc)))))




(defmethod princ-for-each-line ((sequence list))
  (labels ((inner (sequence)
             (if (null sequence)
                 (fresh-line)
                 (progn
                   (fresh-line)
                   (princ (first sequence))
                   (inner (rest sequence))))))
    (inner sequence)))

(defmethod princ-for-each-line ((sequence array))
  (dotimes (i (length sequence))
    (fresh-line)
    (princ (aref sequence i)))
  (fresh-line))


(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))) body...)




;;; Write code here

(defun solve (a b k l)
  (min
   (* b (ceiling k l))
   (+ (* a (mod k l))
      (* b (truncate k l)))))


(defun main ()
  (let ((a (read))
        (b (read))
        (k (read))
        (l (read))
        )
    (format t "~a~%" (solve a b k l))))

(main)

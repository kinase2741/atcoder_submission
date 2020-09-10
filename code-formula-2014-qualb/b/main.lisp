#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (terpri stream))))


(defmethod fast-sort ((sequence list) &optional (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))


(defmethod fast-sort ((sequence array) &optional (test #'<))
  (declare (inline sort))
  (sort sequence (lambda (x y)
                   (funcall test x y))))

(defmacro read-numbers-to-list (size)
  `(progn
     (when (not (integerp ,size))
       (error "Size must be integer."))
     (when (< ,size 0)
       (error "Size must be plus or zero."))
     (loop repeat ,size collect (read))))



(defmacro read-numbers-to-array (size)
  `(progn
     (when (not (integerp ,size))
       (error "Size must be integer."))
     (when (< ,size 0)
       (error "Size must be plus or zero."))
     (make-array ,size :initial-contents (read-numbers-to-list ,size))))

(defmacro read-numbers-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym)))
    `(let ((,board (make-array '(,row-size ,column-size))))
       (dotimes (,r ,row-size)
         (dotimes (,c ,column-size)
           (setf (aref ,board ,r ,c) (read))))
       ,board)))


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
       (write-string (get-output-stream-string ,out)))))




;;; Write code here

(defun solve (n)
  (labels ((inner (n &optional (even-sum 0) (odd-sum 0) (cnt 0))
             (cond
               ((zerop n) (format nil "~a ~a" even-sum odd-sum))
               ((evenp cnt) (inner (truncate n 10)
                                   even-sum
                                   (+ odd-sum
                                      (rem n 10))
                                   (1+ cnt)))
               (t           (inner (truncate n 10)
                                   (+ even-sum
                                      (rem n 10))
                                   odd-sum
                                   (1+ cnt))))))
    (inner n)))


(defun main ()
  (let ((n (read)))
    (format t "~a~%" (solve n))))

(main)

#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (fresh-line stream))))

(declaim (inline fast-sort))
(defmethod fast-sort ((sequence list) &key (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))

(declaim (inline quick-sort))
(defmethod quick-sort ((sequence array))
  (labels ((swap (arr x y)
             (rotatef (aref arr x)
                      (aref arr y)))
           (qsort-sub (arr left right)
             (let ((l left)
                   (r right)
                   (pivot (aref arr (+ left
                                       (random (- right left))))))
               (loop while (<= l r) do
                    (loop while (< (aref arr l) pivot) do
                         (incf l))
                    (loop while (> (aref arr r) pivot) do
                         (decf r))
                    (when (<= l r)
                      (swap arr l r)
                      (incf l)
                      (decf r)))
               (when (< left r)
                 (qsort-sub arr left r))
               (when (< l right)
                 (qsort-sub arr l right)))))
    (qsort-sub sequence 0 (1- (length sequence)))
    sequence))


(defmacro read-numbers-to-list (size)
  `(loop repeat ,size collect (read)))



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


(defun princ-for-each-line (list)
  (format t "~{~a~&~}" list))

(defun unwrap (list)
  (format nil "~{~a~^ ~}" list))



(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))




;;; Write code here

(defun solve (n k)
  (declare (fixnum n k))
  (the integer
       (let ((cnt 0))
         (declare (fixnum cnt))
         (cond
           ((oddp k)
            (loop for i from 1 to n do
                 (when (zerop (mod i k))
                   (incf cnt))
               finally
                 (return (expt cnt 3))))
           (t
            (let ((full-count 0)
                  (half-count 0))
              (declare (fixnum full-count half-count))
              (loop for i from 1 to n do
                   (when (zerop (mod i k))
                     (incf full-count))
                   (when (and (not (zerop (mod i k)))
                              (zerop (mod i (truncate k 2))))
                     (incf half-count))
                 finally
                   (return (+ (expt full-count 3)
                              (expt half-count 3))))))))))



       
(defun main ()
  (let ((n (read))
        (k (read)))
    (declare (fixnum n k))
    (format t "~a~&" (solve n k))))

#-swank (main)

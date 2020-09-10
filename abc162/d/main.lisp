#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (fresh-line stream))) body...)


(defmethod fast-sort ((sequence list) &key (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))


(defmethod fast-sort ((sequence array) &key (test #'<))
  (declare (inline sort))
  (sort sequence (lambda (x y)
                   (funcall test x y))))

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

(defun solve (n s)
  (declare (fixnum n)
           (string s))
  (the fixnum
       (let ((ans (*
                   (count #\R s :test #'char-equal)
                   (count #\G s :test #'char-equal)
                   (count #\B s :test #'char-equal)
                   )))
         (declare (fixnum ans))
         (loop for i from 0 below (- n 2) do
              (loop for j from (1+ i) below (- n 1) do
                   (let ((k (- (* j 2)
                               i
                               )))
                     (unless (or
                              (>= k n)
                              (char-equal (elt s k)
                                          (elt s i))
                              (char-equal (elt s k)
                                          (elt s j))
                              (char-equal (elt s i)
                                          (elt s j)))
                       (decf ans))))
            finally
              (return ans)))))


(defun main ()
  (let ((n (read))
        (s (read-line)))
    (declare (fixnum n))
    (format t "~a~&" (solve n s))))

#-swank (main)

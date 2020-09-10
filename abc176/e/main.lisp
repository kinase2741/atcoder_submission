#-swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#+swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

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


(defun main ()
  (let ((h (read))
        (w (read))
        (mines (loop repeat (read) collect (cons (1- (read))
                                                 (1- (read))))))
    (let ((h-count (make-array h))
          (w-count (make-array w)))
      (loop for m in mines do
           (incf (aref h-count (first m)))
           (incf (aref w-count (rest m))))
      (let* ((h-max (reduce #'max h-count))
             (w-max (reduce #'max w-count))
             (h-max-list (remove-if-not
                          (lambda (y)
                            (= (aref h-count y)
                               h-max))
                          (loop for i below h collect i)))
             (w-max-list (remove-if-not
                          (lambda (x)
                            (= (aref w-count x)
                               w-max))
                          (loop for i below w collect i))))
        (let (ans)
          (if (some (lambda (m)
                        (and
                         (find (first m) h-max-list)
                         (find (rest m) w-max-list)))
                     mines)
              (setf ans (+ h-max w-max))
              (setf ans (1- (+ h-max w-max))))
          (format t "~a~&" ans))))))
#-swank (main)

#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (terpri stream))))


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

(let (parents)
  (defun uf-print ()
    (princ parents))

  (defun uf-init (size)
    (setf parents (make-array size :initial-element -1)))

  
  (defun uf-find (x)
    (if (minusp (aref parents x))
        x
        (setf (aref parents x) (uf-find (aref parents x)))))

  (defun uf-unite (x y)
    (let ((x (uf-find x))
          (y (uf-find y)))
      (when (> x y)
        (rotatef x y))
      (unless (= x y)
        (incf (aref parents x) (aref parents y))
        (setf (aref parents y) x))))

  (defun uf-count-trees ()
    (length
     (remove-duplicates
      (mapcar (lambda (x)
                (uf-find x))
              (loop for i below (length parents) collect i)))))
  )




(defun main ()
  (let* ((n (read))
         (m (read))
         (ans 0)
         (edges (loop repeat m collect (cons (1- (read))
                                             (1- (read))))))
    (assert (= (length edges) m))
    (loop for edge in edges do
         (let ((edges-1 (remove edge edges
                                :test #'equalp)))
           (uf-init n)
           (loop for e in edges-1 do
                (uf-unite (first e)
                          (rest e)))
           (if (/= (uf-count-trees)
                   1)
               (incf ans)))
       finally
         (format t "~a~&" ans))))



(main)

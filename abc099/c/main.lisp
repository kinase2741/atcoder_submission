#|
------------------------------------
|               Utils               |
------------------------------------
|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (progn (ql:quickload '(:cl-debug-print :fiveam))
                 (shadow :run)
                 (use-package :fiveam)))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

#-swank
(unless (member :child-sbcl *features*)
  (quit
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "128MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
                 :output t :error t :input t))))


(defconstant +mod+ 1000000007)


(defmacro define-int-types (&rest bits)
  `(progn
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "INT~a" b)) () '(signed-byte ,b))) bits)
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "UINT~a" b)) () '(unsigned-byte ,b))) bits)))

(define-int-types 2 4 8 16 32 64)


(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (fresh-line stream))))


(defmacro safe-sort (list test &key (key #'identity))
  `(progn
    (declaim (inline sort sb-impl::stable-sort-list))
    (sort (copy-seq ,list) ,test :key ,key)))


(defmacro read-numbers-to-list (size)
  `(loop repeat ,size collect (read)))


(defmacro read-numbers-to-array (size)
  (let ((i (gensym))
        (arr (gensym)))
    `(let ((,arr (make-array ,size
                             :element-type 'fixnum)))
       (declare ((array fixnum 1) ,arr))
       (loop for ,i of-type fixnum below ,size do
            (setf (aref ,arr ,i) (read))
          finally
            (return ,arr)))))

(defmacro read-characters-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym))
        (tmp (gensym)))
    `(let ((,board (make-array '(,row-size ,column-size) :element-type 'character :adjustable nil)))
       (dotimes (,r ,row-size ,board)
         (let ((,tmp (read-line)))
           (dotimes (,c ,column-size)
             (setf (aref ,board ,r ,c) (char ,tmp ,c))))))))

(defmethod princ-for-each-line ((sequence list))
  (format t "~{~a~&~}" sequence))

(defmethod princ-for-each-line ((sequence vector))
  (loop for i below (length sequence) do
       (princ (aref sequence i))
       (fresh-line)))

(defun unwrap (list)
  (format nil "~{~a~^ ~}" list))



(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))





#|
------------------------------------
|               Body               |
------------------------------------
|#

(defparameter *inf* 100000000)



(defun solve (n)
  (let ((memo (make-array (+ n 10)
                          :initial-element *inf*)))
    (labels ((dp (x)
               (cond
                 ((zerop x) 0)
                 ((minusp x) *inf*)
                 ((< (aref memo x) *inf*) (aref memo x))
                 (t
                  (setf (aref memo x)
                        (let ((res *inf*))
                          (loop with k = 9 while (<= k x) do
                               (setf res (min res
                                              (1+ (dp (- x k)))))
                               (setf k (* k 9)))
                          (loop with k = 6 while (<= k x) do
                               (setf res (min res
                                              (1+ (dp (- x k)))))
                               (setf k (* k 6)))
                          (setf res (min res
                                         (1+ (dp (1- x)))))
                          res))))))
      (dp n))))


(defun main ()
  (declare #.OPT)
  (let ((n (read)))
    (format t "~a~&" (solve n))))

#-swank (main)

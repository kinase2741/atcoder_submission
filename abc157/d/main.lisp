#|
------------------------------------
|               Utils               |
------------------------------------
|#


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #-swank '(optimize (speed 0) (safety 3) (debug 3))
    #+swank '(optimize (speed 3) (safety 0) (debug 0)))
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

(defstruct uf-tree parents)

(defun uf-create (size)
  (make-uf-tree
   :parents (make-array size
                        :initial-element -1)))

(defmethod uf-find ((uf uf-tree) x)
  (declare (uf-tree uf)
           (fixnum x))
  (if (minusp (aref (uf-tree-parents uf)
                    x))
      x
      (setf (aref (uf-tree-parents uf) x)
            (uf-find (aref (uf-tree-parents uf)
                           x)))))

(defmethod uf-unite ((uf uf-tree) x y)
  (declare (fixnum x y))
  (when (> x y)
    (rotatef x y))
  (unless (= x y)
    (incf (aref (uf-tree-parents uf) x) y)
    (setf (aref (uf-tree-parents uf) y) x)))

(defmethod uf-family-p ((uf uf-tree) (x fixnum) (y fixnum))
  (= (uf-find uf x)
     (uf-find uf y)))


(defmethod uf-get-size ((uf uf-tree) (x fixnum))
  (let ((parent (uf-find uf x)))
    (- (aref (uf-tree uf) parent))))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (m (read))
        (k (read)))
    (declare (fixnum n m k))
    (let ((uf (uf-create n))
          (ng-list (make-array n
                               :initial-element nil)))
      (declare (uf-tree uf)
               (array ng-list))
      (dotimes (i m)
        (let ((a (1- (read)))
              (b (1- (read))))
          (push a (aref ng-list b))
          (push b (aref ng-list a))
          (uf-unite uf a b)))
      (dotimes (i k)
        (let ((c (1- (read)))
              (d (1- (read))))
          (declare (fixnum c d))
          (unless (uf-family-p uf c d)
            (push d (aref ng-list c))
            (push c (aref ng-list d)))))
      (princ-for-each-line (mapcar
                            (lambda (x)
                              (the fixnum
                                   (- (uf-get-size uf x)
                                      (length (aref ng-list x))
                                      1)))
                            (loop for i below n collect i))))))

#-swank (main)

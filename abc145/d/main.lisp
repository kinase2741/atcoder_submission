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




(declaim (ftype (function (finxum &optional fixnum) fixnum) modint))
(defun modint (x &optional (m +mod+))
  (declare (integer x))
  (cond
    ((and (>= x 0) (< x m)) x)
    ((minusp x) (modint (+ x m)))
    (t (mod x m))))

(declaim (ftype (function (&rest fixnum) fixnum) mod+))
(defun mod+ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'+ args))))


(declaim (ftype (function (&rest list) fixnum) mod-))
(defun mod- (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'- args))))

(declaim (ftype (function (&rest list) fixnum) mod*))
(defun mod* (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'* args))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
  (declare (integer a m))
  (let ((b m)
        (u 1)
        (v 0))
    (loop until (zerop b) do
         (let ((w (truncate a b)))
           (decf a (* w b))
           (rotatef a b)
           (decf u (* w v))
           (rotatef u v))
       finally
         (loop while (minusp u) do
              (incf u m))
         (return (mod u m)))))

(declaim (ftype (function (&rest list) fixnum) mod/))
(defun mod/ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (* x (mod-inv y))))
              args
              :initial-value 1)))

(declaim (ftype (function (finxum fixnum &optional fixnum) fixnum) mod-pow))
(defun mod-pow (a n &optional (m +mod+))
  (declare (integer a)
           ((integer 0) n))
  (labels ((sub (a n &optional (res 1))
             (if (zerop n)
                 res
                 (sub (mod (* a a) m)
                      (truncate n 2)
                      (if (oddp n)
                          (mod (* res a) m)
                          res)))))
    (sub a n)))


(declaim (ftype (function (fixnum fixnum &optional fixnum) fixnum) mod-binomial))
(defun mod-binomial (n k &optional (m +mod+))
  (declare ((integer 0) m))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k) do
             (setq num (mod (* num x) m)))
        (loop for x from 1 to k do
             (setq denom (mod (* denom x) m)))
        (mod (* num (mod-inv denom m)) m))))


(defun solve (x y)
  (declare (fixnum x y))
  (let ((w (/ (- (* 2 x) y) 3))
        (h (/ (- (* 2 y) x) 3)))
    (if (not (or (integerp w)
                 (integerp h)))
        0
        (mod-binomial (+ h w) w))))


(defun main ()
  (declare #.OPT)
  (let ((x (read))
        (y (read)))
    (format t "~a~&" (solve x y))))

#-swank (main)

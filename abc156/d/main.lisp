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
  `(make-array ,size :initial-contents :element-type 'fixnum (loop repeat ,size collect (read))))

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








(defconstant +mod+ 1000000007)

(defun modint (x &optional (m +mod+))
  (cond
    ((and (>= x 0) (< x +mod+)) x)
    ((minusp x) (modint (+ x +mod+)))
    (t (mod x +mod+))))



(defun mod+ (&rest args)
  (if (null args)
      0
      (modint (reduce #'+ args))))

(defun mod- (&rest args)
  (if (null args)
      0
      (modint (reduce #'- args))))

(defun mod* (&rest args)
  (if (null args)
      0
      (modint (reduce #'* args))))

(defun mod-inv (a &optional (m +mod+))
  (declare (integer a))
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

(defun mod/ (&rest args)
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (* x (mod-inv y))))
              args
              :initial-value 1)))

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


(defun mod-binomial (n k m)
  (declare ((integer 0 #.most-positive-fixnum) m))
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


(defun factinv (n)
  (mod-inv (fact n)))

(defun solve (n a b)
  (declare (fixnum n)
           (fixnum a b))
  (mod- (1- (mod-pow 2 n))
        (mod-binomial n a +mod+)
        (mod-binomial n b +mod+)))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (a (read))
        (b (read)))
    (format t "~a~&" (solve n a b))))

#-swank (main)

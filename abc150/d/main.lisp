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


#|
------------------------------------
|               Body               |
------------------------------------
|#

(defun divide-count (k)
  (declare (fixnum k))
  (loop until (oddp k) with cnt fixnum = 0 do
       (setf k (floor k 2))
       (incf cnt)
     finally
       (return cnt)))


(defun divide-judge (xs)
  (declare (list xs))
  (declare (inline divide-count))
  (loop for x in xs with tmp = 40 do
       (let ((c (divide-count x)))
         (when (and (/= tmp 40)
                    (/= c tmp))
           (return nil))
         (setq tmp c))
     finally
       (return t)))

(defun solve (m a)
  (let ((xb (mapcar (lambda (x) (floor x 2)) a)))
    (if (not (divide-judge xb))
        0
        (let ((p (reduce #'lcm xb)))
          (floor (+ m p) (* p 2))))))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (m (read)))
    (let ((a (sort (read-numbers-to-list n) #'<)))
      (format t "~a~&" (solve m a)))))

#-swank (main)

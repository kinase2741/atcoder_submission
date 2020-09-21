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

(declaim (inline fast-sort))
(defmethod fast-sort ((sequence list) &key (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))


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


(defun calc-happiness (groups board &optional (res 0))
  (loop for group in groups
     do
       ;; group ex: (1 2 4)
       (unless (<= (length group) 1)
         (loop for x below (1- (length group))
            do
              (loop for y from (1+ x) below (length group)
                 do
                   (incf res (aref board
                                   (nth x group)
                                   (nth y group))))))
     finally
       (return res)))


(defun nrt (k &optional (div 2) (cnt 0))
  (if (= cnt 0)
      k
      (nrt (truncate k div) div (1- cnt))))


(defparameter *inf* 10000000)

(defun solve (n board)
  (loop
     for c
     below (expt 3 n)
     with res = (- *inf*)
       with groups = nil
     do
       (loop
          for person
          below n
          initially
            (setq groups (make-list 3))
          do
            (let ((num (mod (nrt c 3 person) 3)))
              (push person (nth num groups))))
       (setq res (max res (calc-happiness groups board)))
     finally
       (return res)))
         
         



(defun main ()
  (declare #.OPT)
  (let* ((n (read))
         (board (make-array `(,n ,n)
                            :element-type 'fixnum
                            :adjustable nil
                            :initial-element 0)))
    (declare ((array fixnum 2) board))
    (labels ((set-value (x y val)
               (setf (aref board x y) val)
               (setf (aref board y x) val)))
      (dotimes (i n)
        (dotimes (j (- n i 1))
          (set-value i (+ i j 1) (read))))
      (format t "~a~&" (solve n board)))))

#-swank (main)

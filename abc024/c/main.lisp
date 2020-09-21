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


(defun solve (k limits positions goals)
  (declare (fixnum k)
           (list limits)
           (array positions goals))
  (loop for limit in limits with res = (make-array k :initial-element 0) with cnt = 1 do
       (loop for i below k do
            (let ((pos (aref positions i)))
              (declare (fixnum pos))
              (when (and
                     (/= pos (aref goals i))
                     (<= (first limit) pos (rest limit)))
                (if (<= (aref positions i) (aref goals i))
                    (setf (aref positions i) (min (rest limit) (aref goals i)))
                    (setf (aref positions i) (max (first limit) (aref goals i))))
                (when (= (aref positions i)
                         (aref goals i))
                  (setf (aref res i) cnt)))))
       (incf cnt)
     finally
       (return res)))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (d (read))
        (k (read)))
    (declare (fixnum n d k))
    (let ((limits (loop repeat d collect (cons (read) (read))))
          (starts (make-array k :initial-element 0))
          (goals (make-array k :initial-element 0)))
      (declare (list limits)
               (array starts goals))
      (loop for i below k do
           (setf (aref starts i) (read))
           (setf (aref goals i) (read)))
      (princ-for-each-line (solve k limits starts goals)))))
  

#-swank (main)

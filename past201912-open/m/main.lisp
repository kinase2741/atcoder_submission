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

(defparameter *inf* 10000000)

(defun calc-estimated-strength (a b x)
  (- b (* x a)))


(defun solve (a-b c-d)
  (declare (list a-b c-d))
  (labels ((judge (monsters x)
             (declare (list monsters)
                      (rational x))
             (>= (reduce #'+
                         (mapcar (lambda (m)
                                   (- (rest m)
                                      (* (first m)
                                         x)))
                                 monsters))
                 0)))
    (loop with ok rational = 0 with ng rational = *inf* with mid rational = 0 until (< (abs (- ok ng)) 1.0e-7) do
         (setq mid (/ (abs (+ ng ok)) 2))
         (let (
               (main-m (sort (copy-seq a-b) #'>
                             :key (lambda (m)
                                    (calc-estimated-strength (first m) (rest m) mid))))
               (sub-m (sort (copy-seq c-d) #'>
                            :key (lambda (m)
                                   (calc-estimated-strength (first m) (rest m) mid)))))
           (declare (list main-m sub-m))
           (if (or (judge (subseq main-m 0 5) mid)
                   (judge (cons (first sub-m) (subseq main-m 0 4)) mid))
               (setq ok mid)
               (setq ng mid)))
       finally
         (return (coerce mid 'double-float)))))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (m (read)))
    (declare (fixnum n m))
    (let ((a-b (loop repeat n collect (cons (read) (read))))
          (c-d (loop repeat m collect (cons (read) (read)))))
      (declare (list a-b c-d))
      (println (solve a-b c-d)))))
      

#-swank (main)

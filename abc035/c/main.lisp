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

;;; Utils

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
             (setf (aref ,board ,r ,c) (elt ,tmp ,c))))))))


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




;;; Body

(defun solve (n queries)
  (declare (fixnum n)
           (list queries))
  (let ((memo (make-array (1+ n)
                          :element-type 'fixnum
                          :initial-element 0)))
    (declare ((array fixnum 1) memo))
    (loop for query in queries do
         (incf (aref memo (first query)))
         (incf (aref memo (1+ (rest query))))
       finally
         (let ((cum (make-array (1+ n)
                                :element-type 'fixnum
                                :initial-element 0)))
           (declare ((array fixnum 1) cum))
           (dotimes (i n)
             (setf (aref cum (1+ i))
                   (+ (aref cum i)
                      (aref memo i))))
           (return (loop for i below n collect
                        (rem (aref cum (1+ i)) 2)))))))


(defun main ()
  (declare #.OPT)
  (let ((n (read))
        (q (read)))
    (let ((queries (loop repeat q collect
                        (cons (1- (read))
                              (1- (read))))))
      (let ((ans (solve n queries)))
        (dotimes (i n)
          (princ (pop ans)))
        (fresh-line)))))

#-swank (main)

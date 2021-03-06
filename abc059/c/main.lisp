(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (progn (ql:quickload '(:cl-debug-print :fiveam))
                 (shadow :run)
                 (use-package :fiveam)))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)

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


(defun cumlative-sum (xs &optional (acc '(0)))
  (if (null xs)
      (reverse acc)
      (cumlative-sum (rest xs) (cons (+ (first xs)
                                        (first acc))
                                     acc))))

(defun calc (a first-plus-p &optional (s 0) (res 0))
  (loop for x in a do
       (when (and (>= (+ s x) 0)
                  (not first-plus-p))
         (incf res (1+ (abs (+ s x))))
         (decf s (1+ (abs (+ s x)))))
       (when (and (<= (+ s x) 0)
                  first-plus-p)
         (incf res (1+ (abs (+ s x))))
         (incf s (1+ (abs (+ s x)))))
       (setf first-plus-p (not first-plus-p))
     finally
       (return res)))

(defun solve (a)
  (declare (list a))
  (let ((cum (cumlative-sum a)))
    (declare (list a))
    (the fixnum
         (min (calc (rest cum) t)
              (calc (rest cum) nil)))))
 

(defun main ()
  (declare #.OPT)
  (let* ((n (read))
         (a (read-numbers-to-list n)))
    (declare (fixnum n)
             ((array fixnum 1) a))
    (format t "~a~&" (solve a))))

#-swank (main)

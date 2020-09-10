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

(declaim (inline quick-sort))
(defmethod quick-sort ((sequence array))
  (labels ((swap (arr x y)
             (rotatef (aref arr x)
                      (aref arr y)))
           (qsort-sub (arr left right)
             (let ((l left)
                   (r right)
                   (pivot (aref arr (+ left
                                       (random (- right left))))))
               (loop while (<= l r) do
                    (loop while (< (aref arr l) pivot) do
                         (incf l))
                    (loop while (> (aref arr r) pivot) do
                         (decf r))
                    (when (<= l r)
                      (swap arr l r)
                      (incf l)
                      (decf r)))
               (when (< left r)
                 (qsort-sub arr left r))
               (when (< l right)
                 (qsort-sub arr l right)))))
    (qsort-sub sequence 0 (1- (length sequence)))
    sequence))


(defmacro read-numbers-to-list (size)
  `(loop repeat ,size collect (read)))



(defmacro read-numbers-to-array (size)
  `(make-array ,size :initial-contents (loop repeat ,size collect (read))))

(defmacro read-numbers-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym)))
    `(let ((,board (make-array '(,row-size ,column-size))))
       (dotimes (,r ,row-size)
         (dotimes (,c ,column-size)
           (setf (aref ,board ,r ,c) (read))))
       ,board)))


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

(defun int->lst (n)
  (declare (fixnum n))
  (labels ((inner (n &optional (acc nil))
             (if (zerop n)
                 acc
                 (inner (floor n 10) (cons (rem n 10)
                                           acc)))))
    (inner n)))


(defun calc-value (lst)
  (declare (list lst))
  (the fixnum
       (labels ((inner (lst &optional (acc 0) (res 0))
                  (if (null lst)
                      (+ acc res)
                      (if (integerp (first lst))
                          (inner (rest lst)
                                 (+ (* acc 10)
                                    (first lst))
                                 res)
                          (inner (rest lst)
                                 0
                                 (+ acc res))))))
         (inner lst))))


(declaim (ftype (function (fixnum) fixnum)))
(defun solve (n)
  (declare (fixnum n))
  (the fixnum
       (let ((ln (int->lst n))
             (res 0))
         (declare (list ln)
                  (fixnum res))
         (loop for i below (ash 1 (1- (length ln))) do
              (let ((ln-sub (copy-seq ln))
                    (acc nil))
                (declare (list ln-sub)
                         (list acc))
                (loop for j below (1- (length ln)) do
                     (push (pop ln-sub) acc)
                     (when (oddp (ash i (- j)))
                       (push "+" acc)))
                (push (pop ln-sub) acc)
                (incf res (calc-value (reverse acc)))))
         res)))


(defun main ()
  (declare #.OPT)
  (let ((n (read)))
    (format t "~a~&" (solve n))))

#-swank (main)

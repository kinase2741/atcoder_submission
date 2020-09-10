(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0)))
  #+swank (progn (ql:quickload '(:cl-debug-print :fiveam))
                 (shadow :run)
                 (use-package :fiveam)))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)


;;; Utils

#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))


(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 (princ obj stream) (terpri stream))))


(defmethod fast-sort ((sequence list) &optional (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))


(defmethod fast-sort ((sequence array) &optional (test #'<))
  (declare (inline sort))
  (sort sequence (lambda (x y)
                   (funcall test x y))))

(defmacro read-numbers-to-list (size)
  `(progn
     (when (not (integerp ,size))
       (error "Size must be integer."))
     (when (< ,size 0)
       (error "Size must be plus or zero."))
     (loop repeat ,size collect (read))))



(defmacro read-numbers-to-array (size)
  `(progn
     (when (not (integerp ,size))
       (error "Size must be integer."))
     (when (< ,size 0)
       (error "Size must be plus or zero."))
     (make-array ,size :initial-contents (read-numbers-to-list ,size))))

(defmacro read-numbers-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym)))
    `(let ((,board (make-array '(,row-size ,column-size))))
       (dotimes (,r ,row-size)
         (dotimes (,c ,column-size)
           (setf (aref ,board ,r ,c) (read))))
       ,board)))


(defmethod make-cumlative-sum ((sequence list))
  (labels ((inner (sequence &optional (acc '(0)))
             (if (null sequence)
                 (reverse acc)
                 (inner (rest sequence) (cons (+ (first sequence)
                                                 (first acc))
                                              acc)))))
    (inner sequence)))


(defmethod make-cumlative-sum ((sequence array))
  (declare (type (simple-array fixnum) sequence))
  (the array
       (let* ((n (length sequence))
              (acc (make-array (1+ n) :element-type 'integer :initial-element 0)))
         (loop for i below n do
              (setf (aref acc (1+ i)) (+ (aref sequence i)
                                         (aref acc i)))
            finally
              (return acc)))))




(defmethod princ-for-each-line ((sequence list))
  (labels ((inner (sequence)
             (if (null sequence)
                 (fresh-line)
                 (progn
                   (fresh-line)
                   (princ (first sequence))
                   (inner (rest sequence))))))
    (inner sequence)))

(defmethod princ-for-each-line ((sequence array))
  (dotimes (i (length sequence))
    (fresh-line)
    (princ (aref sequence i)))
  (fresh-line))



(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))



;;; Write code here

;;; deque

(defparameter *default-deque-size* 100)

(defstruct deque
  (data nil)
  (size *default-deque-size*)
  (head 0)
  (tail 0)
  (count 0))


(defun deque-create (size)
  (make-deque
   :size size
   :data (make-array size)))



(defmethod deque-clear ((d deque))
  (fill (deque-data d) 0)
  (setf (deque-head d) 0)
  (setf (deque-tail d) 0)
  (setf (deque-count d) 0))

;; Subcommand


(defmethod deque-empty-p ((d deque))
  (zerop (deque-count d)))



(defmethod deque-full-p ((d deque))
  (= (deque-count d) (deque-size d)))

(defmethod deque-get-prev-index ((d deque) idx)
  (declare (inline deque-get-next-index))
  (if (zerop idx)
      (1- (deque-size d))
      (1- idx)))

(defmethod deque-get-next-index ((d deque) idx)
  (rem (1+ idx) (deque-size d)))




(defmethod deque-pushfront ((d deque) item)
  (when (deque-full-p d)
    (error "deque is full"))

  (setf (deque-head d) (deque-get-prev-index d (deque-head d)))
  (setf (aref (deque-data d) (deque-head d)) item)
  (when (deque-empty-p d) ; first insersion
    (setf (deque-tail d) (deque-head d)))
  (incf (deque-count d)))



(defmethod deque-pushback ((d deque) item)
  (when (deque-full-p d)
    (error "deque is full"))
  
  (setf (deque-tail d) (deque-get-next-index d (deque-tail d)))
  (setf (aref (deque-data d) (deque-tail d)) item)
  (when (deque-empty-p d) ; first insersion
    (setf (deque-head d) (deque-tail d)))
  (incf (deque-count d)))


(defmethod deque-popfront ((d deque))
  (when (deque-empty-p d)
    (error "deque is empty,"))
  
  (let ((value (aref (deque-data d) (deque-head d))))
    (setf (deque-head d) (deque-get-next-index d (deque-head d)))
    (decf (deque-count d))
    value))



(defmethod deque-popback ((d deque))
  (when (deque-empty-p d)
    (error "deque is empty,"))
  
  (let ((value (aref (deque-data d) (deque-tail d))))
    (setf (deque-tail d) (deque-get-prev-index d (deque-tail d)))
    (decf (deque-count d))
    value))








(defun main ()
  (let* ((n (read)))
    (assert (numberp n))
    (let ((d (deque-create n)))
      (loop for i below n do
           (deque-pushback d (read)))
      (assert (deque-full-p d))

      (let ((x 0)
            (y 0))
        (flet ((increment-score (idx point)
                 (if (evenp idx) 
                     (incf x point)    ;太郎の番のとき
                     (incf y point)))) ;次郎の番のとき
          (loop for i below (1- n) do ; 残り一個になるまで繰り返す
               (let ((left (deque-popfront d))
                     (right (deque-popback d)))
                 (if (< left right) 
                     (progn
                       (increment-score i right)
                       (deque-pushfront d left)) ;戻す
                     (progn
                       (increment-score i left)
                       (deque-pushback d right)))))
          (increment-score n (deque-popfront d))

          (assert (deque-empty-p d))
          (format t "~a~%" (- x y)))))))

(main)

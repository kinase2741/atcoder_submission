#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)



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




;;; Write code here

;; from library

(defun product (xs &optional (repeat 1))
  (let ((res nil))
    (labels ((product-sub (xs &optional (acc nil) (cnt 0))
               (if (= cnt repeat)
                   (push (reverse acc) res)
                   (loop for x in xs do
                        (product-sub xs (cons x acc) (1+ cnt))))))
      (product-sub xs)
      (reverse res))))

(defparameter *inf* 10000000)


(defun calc-cost (la lb lc a b c)
  (declare (list la lb lc)
           (fixnum a b c))
  (labels ((calc-sub (lx x &optional (res 0) (len 0))
             (declare (list lx)
                      (fixnum res x len))
             (incf len (reduce #'+ lx))
             (incf res (* (1- (length lx)) 10))
             (incf res (abs (- len x)))
             res))
    (+ (calc-sub la a)
       (calc-sub lb b)
       (calc-sub lc c))))

(defun solve (n a b c l)
  (let ((cand-lst (remove-if-not (lambda (lst)
                                   (and (plusp (count 1 lst))
                                        (plusp (count 2 lst))
                                        (plusp (count 3 lst))))
                                 (product '(1 2 3 x) n))))
    (let ((ans *inf*))
      (loop for lst of-type list in cand-lst do
           (let ((la nil)
                 (lb nil)
                 (lc nil))
             (loop for i below n do
                  (case (first lst)
                    (1 (push (nth i l) la))
                    (2 (push (nth i l) lb))
                    (3 (push (nth i l) lc)))
                  (pop lst))
             (setf ans (min ans
                            (calc-cost la lb lc a b c))))
         finally
           (return ans)))))


(defun main ()
  (let* ((n (read))
         (a (read))
         (b (read))
         (c (read))
         (l (loop repeat n collect (read))))
    (format t "~a~&" (solve n a b c l))))

#-swank (main)

#+swank (declaim (optimize (speed 0) (safety 3) (debug 3)))
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +mod+ 1000000007)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1
        (write obj :stream stream)
      (fresh-line stream))) body...)


(defmethod fast-sort ((sequence list) &key (test #'<))
  (declare (inline sort)
           (inline sb-impl::stable-sort-list))
  (sort sequence (lambda (x y)
                   (funcall test x y))))


(defmethod fast-sort ((sequence array) &key (test #'<))
  (declare (inline sort))
  (sort sequence (lambda (x y)
                   (funcall test x y))))

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


(defparameter *default-deque-size* 100)

(defstruct deque
  (data nil)
  (size nil)
  (head 0)
  (tail 0)
  (count 0))


(defun deque-create (&optional (size *default-deque-size*))
  (make-deque
   :size size
   :data (make-array size)))



(defmethod deque-clear ((d deque))
  (fill (deque-data d) 0)
  (setf (deque-size d) 0)
  (setf (deque-head d) 0)
  (setf (deque-tail d) 0)
  (setf (deque-count d) 0))

;; Subcommand

(declaim (inline deque-empty-p
                 deque-full-p
                 deque-get-prev-index
                 deque-get-next-index))


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




;;; Main command

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


(defmethod dref ((d deque) idx)
  (let ((arr (deque-data d))
        (size (deque-size d))
        (head (deque-head d)))
    (aref arr (mod (+ idx
                      head)
                   size))))

(defparameter *inf* 10000000)

(defparameter *dy-dx*
  '((1 . 0)
    (0 . 1)
    (-1 . 0)
    (0 . -1)))

(defparameter *dy-dx2*
  (let ((acc))
    (loop for i from -2 to 2 do
         (loop for j from -2 to 2 do
              (unless (= i j 0)
                (push (cons i j) acc)))
       finally
         (return (reverse acc)))))

(defun bfs (r c sy sx gy gx board)
  (declare (fixnum r c sy sx gy gx)
           ((array character 2) board))
  (let ((memo (make-array `(,r ,c) :element-type 'fixnum :adjustable nil :initial-element *inf*))
        (q (deque-create 10000)))
    (declare ((array fixnum 2) memo)
             (deque q))
    (deque-pushback q (cons sy sx))
    (setf (aref memo sy sx) 0)
    (loop until (deque-empty-p q) do
         (let* ((pos (deque-popfront q))
                (y (first pos))
                (x (rest pos)))
           (declare (fixnum y x))
           (when (and (= gy y)
                      (= gx x))
             (return (aref memo gy gx)))
           (loop for d in *dy-dx*
              with flag = nil
              do
                (let ((ny (+ y
                             (first d)))
                      (nx (+ x
                             (rest d))))
                  (when (and (<= 0 ny (1- r))
                             (<= 0 nx (1- c))
                             (char-equal (aref board ny nx) #\.)
                             (= (aref memo ny nx)
                                *inf*))
                    (setq flag t)
                    (deque-pushback q (cons ny nx))
                    (setf (aref memo ny nx)
                          (aref memo y x))))
                (when (not flag)
                  (loop for d in *dy-dx2*
                     do
                       (let ((ny (+ y
                                    (first d)))
                             (nx (+ x
                                    (rest d))))
                         (when (and (<= 0 ny (1- r))
                                    (<= 0 nx (1- c))
                                    (char-equal (aref board ny nx) #\.)
                                    (= (aref memo ny nx)
                                       *inf*))
                           (deque-pushback q (cons ny nx))
                           (setf (aref memo ny nx)
                                 (+ (aref memo y x)))))))
              finally
                (return -1))))))

(defun solve (h w sy sx gy gx board)
  (bfs h w sy sx gy gx board))


(defun main ()
  (let ((h (read))
        (w (read))
        (sy (read))
        (sx (read))
        (gy (read))
        (gx (read)))
    (let ((board (make-array `(,h ,w)
                             :element-type 'character
                             :initial-element #\1)))
      (dotimes (y h)
        (let ((tmp (read-line)))
          (dotimes (x w)
            (setf (aref board y x) (char tmp x)))))
      (format t "~a~&" (solve h w sy sx gy gx board)))))

#-swank (main)

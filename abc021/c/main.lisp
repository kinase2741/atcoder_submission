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

(defmacro buffered-read-line (&optional (buffer-size 30) (in '*standard-input*) (term-char #\Space))
  (let ((buffer (gensym))
        (character (gensym))
        (idx (gensym)))
    `(let* ((,buffer (load-time-value (make-string ,buffer-size :element-type 'base-char))))
       (declare (simple-base-string ,buffer)
                (inline read-byte))
       (loop for ,character of-type base-char =
                ,(if (member :swank *features*)
                     `(read-char ,in nil #\Newline) ; on SLIME
                     `(code-char (read-byte ,in nil #.(char-code #\Newline))))
             for ,idx from 0
             until (char= ,character #\Newline)
             do (setf (schar ,buffer ,idx) ,character)
             finally (when (< ,idx ,buffer-size)
                       (setf (schar ,buffer ,idx) ,term-char))
                     (return (values ,buffer ,idx))))))

(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  (declare (inline read-byte)
           #-swank (sb-kernel:ansi-stream in))
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (read-byte in nil 0))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setq minus t)))))))
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48) (the (integer 0 #.(floor most-positive-fixnum 10)) (* result 10))))
              (return (if minus (- result) result))))))))


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
  `(loop repeat ,size collect (read-fixnum)))

(defmacro read-numbers-to-array (size)
  (let ((i (gensym))
        (arr (gensym)))
    `(let ((,arr (make-array ,size
                             :element-type 'fixnum)))
       (declare ((array fixnum 1) ,arr))
       (loop for ,i of-type fixnum below ,size do
            (setf (aref ,arr ,i) (read))
          finally
            (return ,arr)))))

(defmacro read-characters-to-board (row-size column-size)
  (let ((board (gensym))
        (r (gensym))
        (c (gensym))
        (tmp (gensym)))
    `(let ((,board (make-array '(,row-size ,column-size) :element-type 'character :adjustable nil)))
       (dotimes (,r ,row-size ,board)
         (let ((,tmp (buffered-read-line)))
           (dotimes (,c ,column-size)
             (setf (aref ,board ,r ,c) (char ,tmp ,c))))))))

(defmethod princ-for-each-line ((sequence list))
  (format t "~{~a~&~}" sequence))

(defmethod princ-for-each-line ((sequence vector))
  (loop for i below (length sequence) do
       (princ (aref sequence i))
       (fresh-line)))

(declaim (inline unwrap))
(defun unwrap (list)
  (the string
       (format nil "~{~a~^ ~}" list)))

(defmacro with-buffered-stdout (&body body)
  (let ((out (gensym)))
    `(let ((,out (make-string-output-stream :element-type 'base-char)))
       (let ((*standard-output* ,out))
         ,@body)
       (write-string (get-output-stream-string ,out)))))

(defmacro maxf (place cand)
  `(setf ,place (max ,place ,cand)))

(defmacro minf (place cand)
  `(setf ,place (min ,place ,cand)))


(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defun iota (count &optional (start 0) (step 1))
  (loop for i from 0 below count collect (+ start (* i step))))


#|
------------------------------------
|               Body               |
------------------------------------
|#




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


(defmethod dref ((d deque) subscripts)
  (let ((arr (deque-data d))
        (size (deque-size d))
        (head (deque-head d)))
    (aref arr (mod (+ subscripts
                      head)
                   size))))


(defmacro modf (place)
  `(setf ,place (mod ,place +mod+)))

(defparameter *inf* 100000)

(defun solve (n a b edges)
  (let ((d (deque-create 1000)))
    (declare (fixnum n a b)
             (array edges))
    (deque-pushback d (cons a 0))
    (loop
       until (deque-empty-p d)
       with cnt_memo = (make-array n
                                    :initial-element 0)
       with distance_memo = (make-array n :initial-element *inf*)
       initially
         (setf (aref cnt_memo a) 1)
         (setf (aref distance_memo a) 0)
       do
         (let* ((tmp (deque-popfront d))
                (pos (first tmp))
                (dist (rest tmp)))
           (when (/= pos b)
             (mapc (lambda (next)
                     (cond
                       ((= (aref distance_memo next) *inf*) ;first time to visit
                        (setf (aref distance_memo next) (1+ dist))
                        (incf (aref cnt_memo next) (aref cnt_memo pos))
                        (modf (aref cnt_memo next))
                        (deque-pushback d (cons next (1+ dist))))
                       ((= (aref distance_memo next) (1+ dist))
                        (incf (aref cnt_memo next) (aref cnt_memo pos))
                        (modf (aref cnt_memo next)))))
                   (aref edges pos))))
       finally
         (return (aref cnt_memo b)))))





(defun main ()
  (declare #.OPT)
  (declare (inline solve))
  (let ((n (read))
        (a (1- (read)))
        (b (1- (read)))
        (m (read)))
    (declare (fixnum n a b m))
    (let ((edges (make-array n
                             :initial-element nil)))
      (declare (array edges))
      (loop
         repeat m
         with tmp = nil
         do
           (setq tmp (cons (1- (read))
                           (1- (read))))
           (push (cdr tmp) (aref edges (car tmp)))
           (push (car tmp) (aref edges (cdr tmp))))
      (princ (solve n a b edges))
      (fresh-line))))

#-swank (main)

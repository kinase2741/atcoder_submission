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

(defmacro modf (place &optional (m +mod+))
  `(setf ,place (mod ,place ,m)))

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





(declaim (ftype (function (finxum &optional fixnum) fixnum) modint))
(defun modint (x &optional (m +mod+))
  (declare (integer x))
  (cond
    ((and (>= x 0) (< x m)) x)
    ((minusp x) (modint (+ x m)))
    (t (mod x m))))

(declaim (ftype (function (&rest fixnum) fixnum) mod+))
(defun mod+ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'+ args))))


(declaim (ftype (function (&rest fixnum) fixnum) mod-))
(defun mod- (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'- args))))

(declaim (ftype (function (&rest fixnum) fixnum) mod*))
(defun mod* (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (modint (reduce #'* args))))

(declaim (ftype (function (fixnum &optional fixnum) fixnum) mod-inv))
(defun mod-inv (a &optional (m +mod+))
  (declare (integer a m))
  (let ((b m)
        (u 1)
        (v 0))
    (loop until (zerop b) do
         (let ((w (truncate a b)))
           (decf a (* w b))
           (rotatef a b)
           (decf u (* w v))
           (rotatef u v))
       finally
         (loop while (minusp u) do
              (incf u m))
         (return (mod u m)))))

(declaim (ftype (function (&rest fixnum) fixnum) mod/))
(defun mod/ (&rest args)
  (declare (inline modint))
  (if (null args)
      0
      (reduce (lambda (x y)
                (modint (* x (mod-inv y))))
              args
              :initial-value 1)))

(declaim (ftype (function (finxum fixnum &optional fixnum) fixnum) mod-pow))
(defun mod-pow (a n &optional (m +mod+))
  (declare (integer a)
           ((integer 0) n))
  (labels ((sub (a n &optional (res 1))
             (if (zerop n)
                 res
                 (sub (mod (* a a) m)
                      (truncate n 2)
                      (if (oddp n)
                          (mod (* res a) m)
                          res)))))
    (sub a n)))


(declaim (ftype (function (fixnum fixnum &optional fixnum) fixnum) mod-binomial))
(defun mod-binomial (n k &optional (m +mod+))
  (declare ((integer 0) m))
  (if (or (< n k) (< n 0) (< k 0))
      0
      (let ((k (if (< k (- n k)) k (- n k)))
            (num 1)
            (denom 1))
        (declare ((integer 0) k num denom))
        (loop for x from n above (- n k) do
             (setq num (mod (* num x) m)))
        (loop for x from 1 to k do
             (setq denom (mod (* denom x) m)))
        (mod (* num (mod-inv denom m)) m))))
(defun calc-digit (k)
  (length (format nil "~a" k)))

(declaim (inline char->int))
(defun char->int (c)
  (- (char-int c)
     (char-int #\0)))

(defun solve (s)
  (let* ((n (length s))
         (dp (make-array (1+ n) :initial-element 0)))
    (loop for i below n
       do
         (setf (aref dp (1+ i))
               (mod (+ (aref dp i)
                       (* (char->int (elt s (- n i 1)))
                          (mod-pow 10 i 2019)))
                    2019)))
    (loop for i below (1+ n)
       with memo = (make-array 2019) 
       do
         (incf (aref memo (aref dp i)))
       finally
         (return 
           (reduce #'+
                   (map 'vector (lambda (m)
                                  (floor (* m (1- m)) 2))
                        memo))))))


(defun main ()
  (declare #.OPT)
  (let ((s (read-line)))
    (princ (solve s))
    (fresh-line)))

#-swank (main)

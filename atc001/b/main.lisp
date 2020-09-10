;;; Utils

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


(let (parents)
  ;; "parents" retain uf-tree

  (defun uf-init (size)
    (setf parents (make-array size :initial-element -1)))

  (defun uf-show-parents ()
    parents)
  
  (defun uf-find (x)
    (if (minusp (aref parents x))
        x ; x is root
        (setf (aref parents x) (uf-find (aref parents x)))))

  (defun uf-unite (x y)
    (when (> x y)
      (rotatef x y))
    (let ((x-parent (uf-find x))
          (y-parent (uf-find y)))
      (unless (= x-parent y-parent)
        (incf (aref parents x-parent)
              (aref parents y-parent))
        (setf (aref parents y-parent) x-parent))))

  (defun uf-get-tree-size (x)
    (- (aref parents (uf-find x))))

  (defun uf-conut-trees ()
    (length (remove-duplicates
             (mapcar (lambda (idx)
                       (uf-find idx))
                     (loop for i below (length parents) collect i))
             :test #'=)))

  (defun uf-friends-p (x y)
    (= (uf-find x)
       (uf-find y))))


(defun main ()
  (let ((n (read))
        (q (read)))
    (uf-init n)
    (dotimes (i q)
      (let ((p (read)))
        (if (zerop p)
            (uf-unite (read) (read))
            (if (uf-friends-p (read) (read))
                (format t "Yes~%")
                (format t "No~%")))))))

(main)

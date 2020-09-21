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

;;; body

;; TLE (stuck exhausted?)


(defparameter *inf* 1000000001)

(defun solve (n xs)
  (declare ((array fixnum) xs))
  (let ((memo (make-array (+ n 10)
                          :initial-element *inf*)))
    (labels ((calc-cost (x y)
               (if (or (> x (1- n))
                       (> x (1- n)))
                   *inf*
                   (abs (- (aref xs x)
                           (aref xs y)))))
             (dfs (i)
               (cond
                 ((= i (1- n)) 0)
                 ((> i (1- n)) *inf*)
                 ((< (aref memo i) *inf*) (aref memo i))
                 (t
                  (setf (aref memo i) (min (+ (dfs (+ i 1)) (calc-cost i (+ i 1)))
                                           (+ (dfs (+ i 2)) (calc-cost i (+ i 2)))))
                  (aref memo i)))))
      (dfs 0))))


(defun main ()
  (let ((n (read)))
    (declare (fixnum n))
    (let ((xs (make-array n
                          :element-type 'fixnum
                          :initial-contents (loop repeat n collect (read)))))
      (declare ((array fixnum 1) xs))
      (format t "~a~&" (solve n xs)))))

#-swank (main)

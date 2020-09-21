#|
------------------------------------
|               Body               |
------------------------------------
|#

(defparameter *inf* 10000000)

(defun count-proc (s x)
  (let ((w-cnt (count #\W s
                      :start 0
                      :end x))
        (r-cnt (count #\R s
                      :start x)))
    (max w-cnt
         r-cnt)))

(defun solve (n s)
  (loop for i below n
     with w-cnt = 0
     with r-cnt = (count #\R s)
     with res = r-cnt
     do
       (if (char-equal (char s i) #\W)
           (incf w-cnt)
           (decf r-cnt))
       (setf res (min res (max w-cnt r-cnt)))
     finally
       (return res)))
       

(defun main ()
  (let ((n (read))
        (s (read-line)))
    (princ (solve n s))
    (fresh-line)))

#-swank (main)

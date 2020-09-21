(defun solve (xs &optional (cnt 0))
  (cond
    ((= cnt 3) t)
    ((null xs) nil)
    ((= (first (first xs)) (rest (first xs))) (solve (rest xs) (1+ cnt)))
    (t (solve (rest xs) 0))))


(defun main ()
  (let* ((n (read))
         (xs (loop repeat n collect (cons (read) (read)))))
    (princ (if (solve xs)
               "Yes"
               "No"))
    (fresh-line)))

#-swank (main)

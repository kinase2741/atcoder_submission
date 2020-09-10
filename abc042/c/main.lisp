;;; body

(defun calc-digit (int)
  (if (zerop int)
      0
      (1+ (calc-digit (floor int 10)))))

(defun int->lst (int)
  (if (zerop int)
      nil
      (cons (rem int 10) (int->lst (floor int 10)))))

(defun solve (n k ng-list)
  (loop with cand = n do
       (when (every (lambda (int)
                      (not (find int ng-list)))
                    (int->lst cand))
         (return cand))
       (incf cand)))



(defun main ()
  (let ((n (read))
        (k (read)))
    (let ((ng-list (loop repeat k collect (read))))
      (princ (solve n k ng-list))
      (fresh-line))))

#-swank (main)

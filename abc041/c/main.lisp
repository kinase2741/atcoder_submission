;;; body

(defun main ()
  (let ((n (read)))
    (let ((id-height (sort (loop for i below n collect (cons (1+ i) (read)))
                           (lambda (a b)
                             (> (rest a)
                                (rest b))))))
      (let ((ans (mapcar #'first id-height)))
        (format t "~{~a~&~}" ans)))))


#-swank (main)

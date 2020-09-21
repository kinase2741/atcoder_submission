;;; body

(defun main()
  (let ((n (read)))
    (let ((city-list
           (loop repeat n collect
                (let* ((tmp (read-line))
                       (pos (position #\space tmp))
                       (s (subseq tmp 0 pos))
                       (num (parse-integer (subseq tmp (1+ pos)))))
                  (cons s num)))))
      (let ((population (reduce #'+
                                (mapcar #'rest city-list)))
            (res "atcoder"))
        (loop for city-pop in city-list do
             (when (> (* (rest city-pop) 2)
                       population)
               (setq res (first city-pop)))
           finally
             (princ res)
             (fresh-line))))))

#-swank (main)

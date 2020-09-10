(defparameter *char-list*
  (loop for i below 26 collect (code-char (+ (char-code #\a)
                                             i))))
(defun lst-equal-p (xs ys)
  (if (null xs)
      t
      (if (equal (first xs) (first ys))
          (lst-equal-p (rest xs)
                       (rest ys))
          nil)))

(defun make-pos-list (lst chr &optional (acc nil) (cnt 0))
  (if (null lst)
      (reverse acc)
      (progn
        (when (char-equal (first lst) chr)
          (push cnt acc))
        (make-pos-list (rest lst) chr acc (1+ cnt)))))

(defun solve (s1 s2)
  (let ((p1 (sort (remove-if #'null
                             (mapcar (lambda (c) (make-pos-list s1 c))
                                     *char-list*))
                  (lambda (xs ys) (< (first xs) (first ys)))))
        (p2 (sort (remove-if #'null
                             (mapcar (lambda (c) (make-pos-list s2 c))
                                     *char-list*))
                  (lambda (xs ys) (< (first xs) (first ys))))))
    (if (/= (length p1) (length p2))
        "No")
    (if (lst-equal-p p1 p2)
        "Yes"
        "No")))

(defun main ()
  (let ((str1 (concatenate 'list (read-line)))
        (str2 (concatenate 'list (read-line))))
    (format t "~a~%" (solve str1 str2))))

(main)

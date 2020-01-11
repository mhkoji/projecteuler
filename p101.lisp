(defun diff-list (value-list)
  (let ((len (length value-list)))
    (loop for i from 0 below (1- len)
      collect (- (nth (1+ i) value-list)
                 (nth i value-list)))))

(defun guess-next (value-list)
  (if (apply #'= value-list)
      (car value-list)
      (+ (car (last value-list))
         (guess-next (diff-list value-list)))))

(defun u (n)
  (loop for i from 0 to 10
        for sign = (expt -1 i)
    sum (* sign (expt n i))))

(defvar *u-list*
  (loop for n from 1 to 12 collect (u n)))

;; 37076114526
(defun main ()
  (loop for i from 1
        for gold in (cdr *u-list*)
        for guess = (guess-next (subseq *u-list* 0 i))
    while (/= guess gold)
      sum guess
      do (format t "GOLD: ~A, GUESS: ~A~%" gold guess)))

;;; N = 10^12
;;; x = the number of blue discs
;;; x/N * (x-1)/(N-1) = 1/2

(defun pell (x y)
  (- (* x x) (* 2 y y)))

(defun next (current init)
  (destructuring-bind (x y) current
    (destructuring-bind (a b) init
      (list (+ (* a x) (* 2 b y))
            (+ (* a y) (* b x))))))

;; (1 1)
(defun min-sol ()
  (let ((solutions nil))
    (dotimes (x 42)
      (dotimes (y 30)
        (when (= (pell x y) -1)
          (push (list x y) solutions))))
    (car (sort solutions #'<
               :key (lambda (x-y) (apply #'+ x-y))))))

;; 756872327473
(defun main ()
  (let ((min (expt 10 12))
        (init (min-sol)))
    (loop for current = init then (next current init)
          for (2n-1 2x-1) = current
          for n = (/ (1+ 2n-1) 2)
          do (print current)
          when (and (oddp 2n-1) (< min n))
            return (/ (1+ 2x-1) 2))))
    

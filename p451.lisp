(defun l (n)
  (loop for i from 2 to n
    when (= (rem (* i i) n) 1) return (- n i)))

(defun main ()
  (loop for n from 3 to (* 2 (expt 10 7))
    do (when (= (rem n 1000) 0)
         (print n))
     sum (l n)))


;;6C3 = 6*5*4/3*2*1
(defun C (n r hash)
  (setf (gethash (list n r) hash)
    (if (< n (* 2 r))
        (gethash (list n (- n r)) hash)
        (/ (apply #'* (loop repeat r
                        for k from n downto 0 collect k))
           (apply #'* (loop repeat r
                        for k from 1 collect k))))))

(defvar *memo* (make-hash-table :test #'equal))

;4075
#+nil
(progn
  (loop for n from 1 to 100 do
    (print n)
    (loop for r from 0 to n do
      (C n r *memo*)))
  (loop for val being the hash-value of *memo*
    count (< 1000000 val)))

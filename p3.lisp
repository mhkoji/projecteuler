;; 6857
(let* ((N 600851475143)
       (lim (ceiling (sqrt N))))
  (defun main ()
    (let ((prime? (make-array lim :initial-element t))
          (biggest-prime 2))
      (setf (aref prime? 0) nil)
      (setf (aref prime? 1) nil)
      (loop for i from 2 below lim
        when (aref prime? i) do
           (loop for j from (* i 2) below lim by i
             do (setf (aref prime? j) nil))
           (setf biggest-prime i))
      (loop for i from (1- lim) downto 2
        when (and (aref prime? i) (= (mod N i) 0))
           do (return i)))))

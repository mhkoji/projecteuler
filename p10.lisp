;142913828922
(let* ((N 2000000)
       (prime? (make-array (ceiling (sqrt N))
                           :initial-element t)))
  (defun main ()
    (setf (aref prime? 0) nil)
    (setf (aref prime? 1) nil)
    (loop for i from 2 below (length prime?)
      when (aref prime? i) do
        (loop for j from (* i 2) below (length prime?) by i do
          (setf (aref prime? j) nil)))
    (labels ((primep (n)
               (if (< n (length prime?))
                   (aref prime? n)
                   (loop for i from 2 below (length prime?)
                     when (and (aref prime? i) (= (mod n i) 0)) do
                       (return-from primep nil)
                     finally 
                       (return-from primep t)))))
      (loop for n from 2 to N when (primep n) sum n))))

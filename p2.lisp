;; a(n+2) = a(n+1) + a(n)
(defun main ()
  (let ((table (make-array 1000000))) ;十分長い
    (setf (aref table 0) 1)
    (setf (aref table 1) 2)
    (loop for n from 2 do
      (setf (aref table n) (+ (aref table (- n 1))
                              (aref table (- n 2))))
      (when (< 4000000 (aref table n))
        (return
          (loop for i from 0 to n
                for val = (aref table i)
            when (evenp val) sum val))))))

           
          

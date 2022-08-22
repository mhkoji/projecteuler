;25164150
(defun main ()
  (- (let ((sum (loop for i from 1 to 100 sum i)))
       (* sum sum))
     (loop for i from 1 to 100 sum (* i i))))
     

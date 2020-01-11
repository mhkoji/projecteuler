(defun main ()
  (loop for a from 1 to 1000 do
    (loop for b from (+ a 1)
          for c = (- 1000 a b)
      while (< 0 c)
        when (= (+ (* a a) (* b b))
                (* c c))
          do (return-from main (* a b c)))))

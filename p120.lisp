
(defun r-max (a)
  (let ((a-square (* a a)))
    (loop for n from 1 by 2
          for val = (mod (* 2 n a) a-square)
      when (member val acc) return (reduce #'max acc)
      collect val into acc)))

;333082500
#+nil
(loop for a from 3 to 1000 sum (r-max a))


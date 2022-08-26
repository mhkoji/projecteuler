(defun next (n)
  (if (evenp n)
      (/ n 2)
      (1+ (* 3 n))))

(defun gen-sequence (start)
  (loop for i from 1
        for n = start then (next n)
        collect n into seq
        when (= n 1) return seq))

(defun main ()
  (let* ((size 1000000)
         (lens (make-array size :initial-element nil)))
    (loop for start from 100 below size do
      (when (null (aref lens start))
        (loop for seq = (gen-sequence start) then (cdr seq)
              while seq
              for n = (car seq)
              when (and (< n size)
                        (null (aref lens n)))
                do (setf (aref lens n) (length seq)))))
    (let ((max-len nil)
          (max-len-n nil))
      (loop for start from 100 below size
            for len = (aref lens start) do
        (when (or (null max-len)
                  (< max-len len))
          (setq max-len len)
          (setq max-len-n start)))
      max-len-n)))

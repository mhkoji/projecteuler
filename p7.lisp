(defmacro make-pipe (head tail)
  `(cons ,head #'(lambda (), tail)))

(defun head (pipe) (first pipe))

(defun tail (pipe)
  (if (functionp (rest pipe))
      (setf (rest pipe) (funcall (rest pipe)))
      (rest pipe)))

(defun pipe-elt (pipe i)
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (- i 1))))

(defun pipe-remove-if (fn pipe)
  (cond ((null pipe) nil)
        ((funcall fn (head pipe))
         (pipe-remove-if fn (tail pipe)))
        (t
         (make-pipe (head pipe)
                    (pipe-remove-if fn (tail pipe))))))

(defun integers (&optional (start 0) end)
  (if (or (null end) (<= start end))
      (make-pipe start (integers (1+ start) end))
      nil))

(defun primes (&optional (integers (integers 2)))
  (let ((prime (head integers)))
    (make-pipe prime
               (primes (pipe-remove-if
                        (lambda (x) (= (mod x prime) 0))
                        integers)))))

(defun main ()
  (pipe-elt (primes) 10000))



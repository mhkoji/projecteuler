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

;;;

(defun next (item div)
  (destructuring-bind (prev-a prev-b) item
    (let ((a (+ prev-a (* prev-b 7)))
          (b (+ prev-a prev-b)))
      (when div
        (setq a (mod a div))
        (setq b (mod b div)))
      (list a b))))

;; (num-stream) = ((1 1) (8 2) (22 10) (92 32) (316 124) ...)
(defun num-stream (&optional div)
  (labels ((rec (item)
             (let ((next-item (next item div)))
               (make-pipe item (rec next-item)))))
    (rec (list 1 1))))

(defun compute-g (x)
  (let ((seen-hash (make-hash-table :test #'equal)))
    (loop for n from 1
          for num-stream = (num-stream x) then (tail num-stream)
          for item = (head num-stream) do
      (progn
        #+nil
        (when (= (mod n 100) 0)
          (print n))
        (cond ((equal item '(1 0))
               (return n))
              ((gethash item seen-hash)
               (return 0))
              (t
               (setf (gethash item seen-hash) item)))))))

;;;

(defun primes ()
  (labels ((integers (start)
             (make-pipe start (integers (1+ start))))
           (rec (integers)
             (let ((prime (head integers)))
               (make-pipe prime
                          (rec (pipe-remove-if
                                (lambda (x) (= (mod x prime) 0))
                                integers))))))
    (rec (integers 2))))

(defun factors (n primes)
  (let ((factor-count-hash (make-hash-table :test #'equal)))
    (loop while (/= n 1) do
      (loop for ps = primes then (tail ps)
            for p = (head ps)
            while (<= p n)
            do (progn
                 (when (= (mod n p) 0)
                   (incf (gethash p factor-count-hash 0))
                   (setq n (/ n p))
                   (return)))))
    (let ((factor-count-list nil))
      (maphash (lambda (factor count)
                 (push (list factor count) factor-count-list))
               factor-count-hash)
      factor-count-list)))

(defvar *primes*
  (primes))

(let ((primes *primes*)
      (g-cache (make-hash-table :test #'equal)))
  (labels ((get-cached (x)
             (or (gethash x g-cache)
                 (setf (gethash x g-cache)
                       (compute-g x)))))
    (defun g (x)
      (let ((factor-count-list (factors x primes)))
        (let ((k (reduce #'lcm
                         (let ((factors (mapcar #'first factor-count-list)))
                           (mapcar #'get-cached factors))))
              (l (reduce #'*
                         (loop for (factor count) in factor-count-list
                               collect (expt factor (1- count))))))
          (let ((g (if (= (mod l 7) 0)
                       (* k l)
                       (lcm k l))))
            (setf (gethash x g-cache) g)))))))

(defun large-g (n &optional (from 2))
  (loop for x from from to n
        do (when (= (mod x 100) 0)
             (print x))
        sum (g x)))

;;(large-g 1000000)

;; (* 2 2 2 2  2  2  2  2  2) = 512 > 500
;; (* 2 3 5 7 11 13 17 19 23)
(require :alexandria)
(import '(alexandria:appendf
          alexandria:when-let))


(defun rec (powers test-fn &optional (start 1) (end 30))
  (let ((powers-list nil))
    (loop for i from start to end do
      (let ((new-powers (cons i powers)))
        (cond ((not (funcall test-fn new-powers))
               (appendf powers-list (rec new-powers test-fn i end)))
              (t
               (appendf powers-list (list new-powers))
               (return)))))
    powers-list))

(defun permutation (list)
  (if (null list)
      (list nil)
      (loop for item in list
        nconc (mapcar (lambda (rest)
                        (cons item rest))
                      (permutation (remove item list))))))

(defvar *powers-list*
  (labels ((appropriate-powers-p (powers)
             (< 500 (apply #'* (mapcar #'1+ powers)))))
    (rec nil #'appropriate-powers-p)))

(defvar *primes* '(2 3 5 7 11 13 17 19 23))

(defvar *prime-permutations* (permutation *primes*))

(defun make-number (primes powers)
  (apply #'* (loop for power in powers
                   for prime in primes
                nconc (loop repeat power collect prime))))

(defun triangle-number-p (x)
  (let ((squared (1+ (* 8 x))))
    (when-let ((a (ignore-errors
                    (ceiling (sqrt squared)))))
      (or (= squared (* a a))
          (= squared (* (- a 1) (- a 1)))
          (= squared (* (- a 2) (- a 2)))  
          (= squared (* (+ a 1) (+ a 1)))
          (= squared (* (+ a 2) (+ a 2)))))))


;76576500
(defun main ()
  (let ((list nil) (counter 0))
    (dolist (perm *prime-permutations*)
      (incf counter)
      (when (= (mod counter 1000) 0)
        (print counter))
      (dolist (powers *powers-list*)
        (let ((num (make-number perm powers)))
          (when (triangle-number-p num)
            (push num list)))))
    (car (sort list #'<))))
   

(let ((primes '(2 3 5 7 11 13 17 19 23)))
  (defun count-divisors (num)
    (let ((count 1))
      (dolist (prime primes)
        (when (= (mod num prime) 0)
          (setf count
                (* count
                   (loop for sub-count from 0
                         while (= (mod num prime) 0)
                     do (setf num (/ num prime))
                     finally (return (1+ sub-count)))))))
     count)))

(defun main-2 ()
  (loop for n from 100
        for num = (/ (* n (1+ n)) 2)
    when (< 500 (count-divisors num))
       return num))



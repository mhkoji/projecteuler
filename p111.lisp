(defvar *N* (+ 100000 1))
(defvar *sieve* (make-array *N* :initial-element t))

(let ((limit (ceiling (sqrt *N*))))
  (loop for i from 2 below limit do
       (when (aref *sieve* i)
         (loop for j from (* i 2) below *N* by i do
              (setf (aref *sieve* j) nil)))))

(defun primep (n)
  (loop for i from 2 below (min *N* n)
     when (and (aref *sieve* i)
               (= (mod n i) 0))
     do (return-from primep nil))
  t)

(defun combinations (length &optional (d -1))
  (cond ((= 1 length)
         (loop for i from 0 to 9
            when (/= i d) collect (list i)))
        ((< 1 length)
         (let ((rest (combinations (1- length) d)))
           (loop for i from 0 to 9
              when (/= i d)
              nconc (mapcar (lambda (list) (cons i list))
                            rest))))
        (t nil)))

(defun pickup-all (num set size)
  (cond ((or (= num 0) (< size num))
         (list nil))
        ((= size num)
         (list set))
        (t
         (nconc (pickup-all num (cdr set) (1- size))
                (mapcar (lambda (subset) (cons (car set) subset))
                        (pickup-all (1- num) (cdr set) (1- size)))))))

(defun digits->number (digits)
  (parse-integer (format nil "~{~A~}" digits)))

(defun find-primes (n d &optional (end 9))
  (let ((primes nil)
        (indexes (loop for i from 0 to (1- n) collect i)))
    ;; num: d以外の数字の数
    (loop for num from 1 to end do
         (let ((pos-list (pickup-all num indexes n))
               (combinations (combinations num d)))
           (dolist (others combinations)
             (dolist (pos pos-list)
               (let ((digits (make-list n :initial-element d)))
                 (loop for i in pos for rep in others do
                      (setf (nth i digits) rep))
                 (when (/= (car digits) 0)
                   (let ((number (digits->number digits)))
                     #+nil(format t "~A" number)
                     (when (primep number)
                       #+nil(format t " prime!")
                       (push number primes))
                     #+nil(format t "~%"))))))
           (when primes (return-from find-primes primes))))))

;612407567715
#+nil
(loop for d from 0 to 9
   sum (apply #'+ (find-primes 10 d)))

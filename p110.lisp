(import '(alexandria:appendf))


(defun num-solutions (terms)
  (+ (/ (- (apply #'* terms) 1) 2) 1))

;; n^2の素因数はp^{2a+1}の形をしている
;; 素数の肩に乗るのは3以上の奇数。
(defun rec (terms goal &optional (start 3) (end 30))
  (let ((terms-list nil))
    (loop for i from start to end by 2 do
      (let ((new-terms (cons i terms)))
        (cond ((< (num-solutions new-terms) goal)
               (appendf terms-list
                        (rec new-terms goal i end)))
              (t
               (appendf terms-list (list new-terms))
               (return)))))
    terms-list))

(defvar *primes*
  (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43))

(defvar *terms-list* nil)

;9350130049860600
(defun main ()
  (reduce #'min
    (mapcar (lambda (terms)
              (let ((powers (mapcar (lambda (x) (/ (1- x) 2))
                                    terms)))
                (reduce #'* (mapcar #'expt *primes*
                                           (sort powers #'>)))))
            (setq *terms-list* (rec nil 4000000)))))

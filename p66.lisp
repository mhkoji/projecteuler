(defvar *squares*
  (loop for i from 1
        for sq = (* i i)
    while (< sq 1000) collect sq))

(defun min-sol (D)
  (let* ((curr-s 0)
         (curr-t 1)
         (init-a (floor (sqrt D)))
         (curr-a init-a)
         (prev-x 1)
         (prev-y 0)
         (curr-x init-a)
         (curr-y 1))
    (loop for k = 1 then (* k -1) do
      (let* ((next-s (- (* curr-a curr-t) curr-s))
             (next-t (/ (- D (* next-s next-s)) curr-t))
             (next-a (floor (+ init-a next-s) next-t))
             (next-x (+ (* next-a curr-x) prev-x))
             (next-y (+ (* next-a curr-y) prev-y)))
        (when (and (= k -1) (= next-t 1))
          (return (list curr-x curr-y)))
        (setq prev-x curr-x)
        (setq prev-y curr-y)
        (setq curr-s next-s)
        (setq curr-t next-t)
        (setq curr-a next-a)
        (setq curr-x next-x)
        (setq curr-y next-y)))))
    
;; (x, y) (a, b): two solutions => x < a <=> x+y*sqrt(D) < a+b*sqrt(D)
;; 661
(defun main ()
  (caar (sort (loop for D from 1 to 1000
                when (not (member D *squares* :test #'=))
                  collect (cons D (car (min-sol D))))
              #'> :key #'cdr)))


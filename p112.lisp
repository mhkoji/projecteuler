;; ユーザーがdigit=1を入力することは想定していない
(defun num-increasing-numbers (digits limits &key (left-most>= 0)
                                                  (first-p t))
  (if (= digits 1)
      (- 10 left-most>=)
      (loop for i from (car limits) downto (if first-p 1 0)
        sum (num-increasing-numbers
             (1- digits) (cdr limits)
             :left-most>= i :first-p nil))))

(defun num-decreasing-numbers (digits limits &key (left-most<= 9)
                                                  (first-p t))
  (if (= digits 1)
      (1+ left-most<=)
      (loop for i from 9 downto (if first-p 1 0)
         sum (num-decreasing-numbers
              (1- digits) :left-most<= i :first-p nil))))

(defun num-bouncy-numbers (digits)
  (+ 9 ;; 11..., 22..., ..., 99...の分だけ余分に引かれてる
     (- (parse-integer
         (make-string digits :initial-element #\9))
        (num-increasing-numbers digits)
        (num-decreasing-numbers digits))))
        

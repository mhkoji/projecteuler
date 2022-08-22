;; x, y: 3-digit numbers s.t. x * y => palindrome

(defun palindromic-p (number)
  (let ((str (format nil "~A" number)))
    (string= str (reverse str))))

;; これは誤答。深さ優先はまずい。しかもzいらんで。
;; (defun main ()
;;   (loop for x from 999 downto 100 do
;;     (loop for y from 999 downto 100 do
;;       (loop for z from 999 downto 100 do
;;         (when (palindromic-p (* x y z))
;;           (return-from main (* x y z)))))))

;; 多分 900 <= x,y s<= 999の間に解があるだろう => あった
;; 906609
(defun main ()
  (let ((numbers nil))
    (loop for x from 999 downto 900 do
      (loop for y from 999 downto 900 do
        (push (* x y) numbers)))
    (find-if #'palindromic-p (sort numbers #'>))))

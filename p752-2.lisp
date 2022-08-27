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

(defvar *primes*
  (primes))

(defun find-factor (n)
  (let ((sqrt (floor (sqrt n)))
        (primes *primes*))
    (loop for ps = primes then (tail ps)
          for p = (head ps)
          while (<= p sqrt)
          when (= (mod n p) 0) do (return-from find-factor p))))

(defun prime-p (n)
  (null (find-factor n)))

(defun factor-count-list (n)
  (let ((factor-count-hash (make-hash-table :test #'equal)))
    (loop while (/= n 1) do
      (let ((factor (find-factor n)))
        (cond ((null factor)
               (setf (gethash n factor-count-hash) 1)
               (return))
              (t
               (let ((count 0))
                 (loop while (and (/= n 1) (= (mod n factor) 0)) do
                   (incf count)
                   (setq n (/ n factor)))
                 (setf (gethash factor factor-count-hash) count))))))
    (let ((factor-count-list nil))
      (maphash (lambda (factor count)
                 (push (list factor count) factor-count-list))
               factor-count-hash)
      factor-count-list)))

;;;

(defvar *div* nil)

(defun add (x y)
  (if *div*
      (mod (+ x y) *div*)
      (+ x y)))

(defun mul (x y)
  (if *div*
      (mod (* x y) *div*)
      (* x y)))

(defun make-mat (a b c d)
  (let ((M (make-array '(2 2))))
    (setf (aref M 0 0) (mod a *div*)
          (aref M 0 1) (mod b *div*)
          (aref M 1 0) (mod c *div*)
          (aref M 1 1) (mod d *div*))
    M))

(defun mat-mul (A B)
  (make-mat
   (add (mul (aref A 0 0) (aref B 0 0))
        (mul (aref A 0 1) (aref B 1 0)))
   (add (mul (aref A 0 0) (aref B 0 1))
        (mul (aref A 0 1) (aref B 1 1)))
   (add (mul (aref A 1 0) (aref B 0 0))
        (mul (aref A 1 1) (aref B 1 0)))
   (add (mul (aref A 1 0) (aref B 0 1))
        (mul (aref A 1 1) (aref B 1 1)))))

(defun mat-expt (A n)
  (cond ((= n 1) A)
        ((= n 2) (mat-mul A A))
        ((evenp n)
         (mat-expt (mat-expt A (/ n 2)) 2))
        (t
         (mat-mul A (mat-expt (mat-expt A (/ (1- n) 2)) 2)))))

(defun mat-nmul (A B)
  (let ((h (add (mul (aref A 0 0) (aref B 0 0))
                (mul (aref A 0 1) (aref B 1 0))))
        (j (add (mul (aref A 0 0) (aref B 0 1))
                (mul (aref A 0 1) (aref B 1 1))))
        (k (add (mul (aref A 1 0) (aref B 0 0))
                (mul (aref A 1 1) (aref B 1 0))))
        (l (add (mul (aref A 1 0) (aref B 0 1))
                (mul (aref A 1 1) (aref B 1 1)))))
    (setf (aref A 0 0) h
          (aref A 0 1) j
          (aref A 1 0) k
          (aref A 1 1) l))
  A)

(defun mat-identity-p (A)
  (and (= (aref A 0 0) 1)
       (= (aref A 0 1) 0)
       (= (aref A 1 0) 0)
       (= (aref A 1 1) 1)))

;;;

(defun mat-pipe (M)
  (labels ((rec (N)
             (make-pipe N (rec (mat-mul M N)))))
    (rec M)))

(defun find-1/6 (x)
  (loop for i from 0 below x
        when (= (mod (* 6 i) x) 1)
          do (return-from find-1/6 i))
  nil)

;; |GL(2, Z/pZ)| = (p^2 - 1)(p^2 - p)
;;               = (p - 1)(p + 1)p(p - 1)
(defun gl-size-factors (p)
  (let ((factor-count-list
         (let ((list nil)
               (hash (make-hash-table :test #'equal)))
           (labels ((inc-hash (factor-count)
                      (destructuring-bind (factor count) factor-count
                        (incf (gethash factor hash 0) count))))
             (mapc #'inc-hash (factor-count-list (1+ p)))
             (mapc #'inc-hash (factor-count-list (1- p)))
             #+ nil(mapc #'inc-hash (factor-count-list p))
             #+ nil(mapc #'inc-hash (factor-count-list (1- p))))
           (maphash (lambda (factor count)
                      (push (list factor count) list))
                    hash)
           list)))
    (let ((acc '(1)))
      (loop for (factor count) in factor-count-list do
        (let ((new-acc nil))
          (loop for a in acc do
            (loop for i from 0 to count do
              (push (* a (expt factor i)) new-acc)))
          (setq acc new-acc)))
      (sort acc #'<))))

(defun compute-g-prime (p)
  (if (= p 7)
      7
      (let ((*div* p))
        (let ((M (make-mat 1 7 1 1))
              (factors (gl-size-factors p)))
          (loop for f in factors
                when (mat-identity-p (mat-expt M f))
                  do (return-from compute-g-prime f))))))

(defun compute-g-compound (x compute-g)
  (let ((x-factor-count-list (factor-count-list x))
        (g-factor-count-hash (make-hash-table :test #'equal)))
    (labels ((g-merge (factor count)
               (when (or (null (gethash factor g-factor-count-hash))
                         (< (gethash factor g-factor-count-hash) count))
                 (setf (gethash factor g-factor-count-hash) count))))
      (loop for (x-factor x-count) in x-factor-count-list do
        (progn
          (loop for (g-factor g-count)
                    in (factor-count-list (funcall compute-g x-factor)) do
            (g-merge g-factor g-count))
          (cond ((= x-factor 7)
                 (g-merge x-factor x-count))
                ((< 1 x-count)
                 (g-merge x-factor (1- x-count)))))))
    (let ((g 1))
      (maphash (lambda (factor count)
                 (setq g (* g (expt factor count))))
               g-factor-count-hash)
      g)))

(defun compute-g-sequentially (x)
  (when (find-1/6 x)
    (let ((*div* x))
      (let ((M (make-mat 1 7 1 1)))
        #+nil
        (loop for index from 1
              for pipe = (mat-pipe M) then (tail pipe)
              for mat = (head pipe)
              when (mat-identity-p mat) return index)
        (let ((N (make-mat 1 7 1 1)))
          (loop for index from 1 do
            (progn
              (when (mat-identity-p N)
                (return-from compute-g-sequentially index))
              (mat-nmul N M))))))))

(let ((g-cache (make-hash-table :test #'equal)))
  (defun g (x)
    (cond ((not (find-1/6 x)) 0)
          ((prime-p x)
           (or (gethash x g-cache)
               (setf (gethash x g-cache)
                     (compute-g-prime x))))
          (t (compute-g-compound x #'g))
          #+nil
          (t (compute-g-sequentially x)))))

(ql:quickload :log4cl)

;; CL-USER> (large-g 1000000)
;;  <INFO> [15:28:08] cl-user p752-2.lisp (large-g) - G(10000)=8776953135
;;  <INFO> [15:28:09] cl-user p752-2.lisp (large-g) - G(20000)=64909963232
;;  <INFO> [15:28:11] cl-user p752-2.lisp (large-g) - G(30000)=200176384407
;;  <INFO> [15:28:14] cl-user p752-2.lisp (large-g) - G(40000)=481094853879
;;  <INFO> [15:28:17] cl-user p752-2.lisp (large-g) - G(50000)=890116827847
;;  <INFO> [15:28:21] cl-user p752-2.lisp (large-g) - G(60000)=1517360759895
;;  <INFO> [15:28:25] cl-user p752-2.lisp (large-g) - G(70000)=2357584912834
;;  <INFO> [15:28:31] cl-user p752-2.lisp (large-g) - G(80000)=3474926032776
;;  <INFO> [15:28:37] cl-user p752-2.lisp (large-g) - G(90000)=4889401680703
;;  <INFO> [15:28:44] cl-user p752-2.lisp (large-g) - G(100000)=6733163074521
;;  <INFO> [15:28:51] cl-user p752-2.lisp (large-g) - G(110000)=8735205431634
;;  <INFO> [15:28:59] cl-user p752-2.lisp (large-g) - G(120000)=11349657070940
;;  <INFO> [15:29:08] cl-user p752-2.lisp (large-g) - G(130000)=14320442359917
;;  <INFO> [15:29:17] cl-user p752-2.lisp (large-g) - G(140000)=17709691390312
;;  <INFO> [15:29:26] cl-user p752-2.lisp (large-g) - G(150000)=21556862821064
;;  <INFO> [15:29:37] cl-user p752-2.lisp (large-g) - G(160000)=26251173536187
;;  <INFO> [15:29:48] cl-user p752-2.lisp (large-g) - G(170000)=31337198159402
;;  <INFO> [15:29:59] cl-user p752-2.lisp (large-g) - G(180000)=37337142741072
;;  <INFO> [15:30:12] cl-user p752-2.lisp (large-g) - G(190000)=44017782912209
;;  <INFO> [15:30:24] cl-user p752-2.lisp (large-g) - G(200000)=51048836186359
;;  <INFO> [15:30:39] cl-user p752-2.lisp (large-g) - G(210000)=58151307697278
;;  <INFO> [15:30:54] cl-user p752-2.lisp (large-g) - G(220000)=66762278418462
;;  <INFO> [15:31:09] cl-user p752-2.lisp (large-g) - G(230000)=76084346714365
;;  <INFO> [15:31:25] cl-user p752-2.lisp (large-g) - G(240000)=85399142037323
;;  <INFO> [15:31:43] cl-user p752-2.lisp (large-g) - G(250000)=96335862370508
;;  <INFO> [15:32:00] cl-user p752-2.lisp (large-g) - G(260000)=108428849975449
;;  <INFO> [15:32:17] cl-user p752-2.lisp (large-g) - G(270000)=120698318456033
;;  <INFO> [15:32:35] cl-user p752-2.lisp (large-g) - G(280000)=134241429198595
;;  <INFO> [15:32:53] cl-user p752-2.lisp (large-g) - G(290000)=148947699875397
;;  <INFO> [15:33:12] cl-user p752-2.lisp (large-g) - G(300000)=165085787571158
;;  <INFO> [15:33:32] cl-user p752-2.lisp (large-g) - G(310000)=181885393887120
;;  <INFO> [15:33:51] cl-user p752-2.lisp (large-g) - G(320000)=198434813706848
;;  <INFO> [15:34:12] cl-user p752-2.lisp (large-g) - G(330000)=217760224162303
;;  <INFO> [15:34:33] cl-user p752-2.lisp (large-g) - G(340000)=237173063875277
;;  <INFO> [15:34:55] cl-user p752-2.lisp (large-g) - G(350000)=257832771556139
;;  <INFO> [15:35:18] cl-user p752-2.lisp (large-g) - G(360000)=277484355829227
;;  <INFO> [15:35:41] cl-user p752-2.lisp (large-g) - G(370000)=300936778797857
;;  <INFO> [15:36:05] cl-user p752-2.lisp (large-g) - G(380000)=325665484466937
;;  <INFO> [15:36:31] cl-user p752-2.lisp (large-g) - G(390000)=352334793578785
;;  <INFO> [15:36:57] cl-user p752-2.lisp (large-g) - G(400000)=379939539212107
;;  <INFO> [15:37:23] cl-user p752-2.lisp (large-g) - G(410000)=408271134632708
;;  <INFO> [15:37:49] cl-user p752-2.lisp (large-g) - G(420000)=438274180401036
;;  <INFO> [15:38:15] cl-user p752-2.lisp (large-g) - G(430000)=468913635053309
;;  <INFO> [15:38:42] cl-user p752-2.lisp (large-g) - G(440000)=501423206149441
;;  <INFO> [15:39:10] cl-user p752-2.lisp (large-g) - G(450000)=539026088222046
;;  <INFO> [15:39:39] cl-user p752-2.lisp (large-g) - G(460000)=576592824851632
;;  <INFO> [15:40:08] cl-user p752-2.lisp (large-g) - G(470000)=612362092853572
;;  <INFO> [15:40:38] cl-user p752-2.lisp (large-g) - G(480000)=651790504965259
;;  <INFO> [15:41:09] cl-user p752-2.lisp (large-g) - G(490000)=690306510898845
;;  <INFO> [15:41:39] cl-user p752-2.lisp (large-g) - G(500000)=735281794315823
;;  <INFO> [15:42:12] cl-user p752-2.lisp (large-g) - G(510000)=780283741449164
;;  <INFO> [15:42:45] cl-user p752-2.lisp (large-g) - G(520000)=825683364038791
;;  <INFO> [15:43:18] cl-user p752-2.lisp (large-g) - G(530000)=873844500466996
;;  <INFO> [15:43:52] cl-user p752-2.lisp (large-g) - G(540000)=923071430348851
;;  <INFO> [15:44:26] cl-user p752-2.lisp (large-g) - G(550000)=970976642504319
;;  <INFO> [15:45:00] cl-user p752-2.lisp (large-g) - G(560000)=1022715075913830
;;  <INFO> [15:45:36] cl-user p752-2.lisp (large-g) - G(570000)=1077596558662729
;;  <INFO> [15:46:12] cl-user p752-2.lisp (large-g) - G(580000)=1135053451963680
;;  <INFO> [15:46:48] cl-user p752-2.lisp (large-g) - G(590000)=1189361325057991
;;  <INFO> [15:47:25] cl-user p752-2.lisp (large-g) - G(600000)=1249844166573067
;;  <INFO> [15:48:03] cl-user p752-2.lisp (large-g) - G(610000)=1311570539402665
;;  <INFO> [15:48:43] cl-user p752-2.lisp (large-g) - G(620000)=1375114598408294
;;  <INFO> [15:49:22] cl-user p752-2.lisp (large-g) - G(630000)=1433916002525943
;;  <INFO> [15:50:02] cl-user p752-2.lisp (large-g) - G(640000)=1505238081141682
;;  <INFO> [15:50:43] cl-user p752-2.lisp (large-g) - G(650000)=1569849090108743
;;  <INFO> [15:51:24] cl-user p752-2.lisp (large-g) - G(660000)=1641451312104936
;;  <INFO> [15:52:06] cl-user p752-2.lisp (large-g) - G(670000)=1713126011233377
;;  <INFO> [15:52:49] cl-user p752-2.lisp (large-g) - G(680000)=1788429100557270
;;  <INFO> [15:53:35] cl-user p752-2.lisp (large-g) - G(690000)=1873992064813939
;;  <INFO> [15:54:22] cl-user p752-2.lisp (large-g) - G(700000)=1952826312748744
;;  <INFO> [15:55:08] cl-user p752-2.lisp (large-g) - G(710000)=2044282026914254
;;  <INFO> [15:55:53] cl-user p752-2.lisp (large-g) - G(720000)=2127457007137136
;;  <INFO> [15:56:49] cl-user p752-2.lisp (large-g) - G(730000)=2217405393071287
;;  <INFO> [15:57:39] cl-user p752-2.lisp (large-g) - G(740000)=2309069270570601
;;  <INFO> [15:58:28] cl-user p752-2.lisp (large-g) - G(750000)=2399669905397799
;;  <INFO> [15:59:17] cl-user p752-2.lisp (large-g) - G(760000)=2496773658471645
;;  <INFO> [16:00:07] cl-user p752-2.lisp (large-g) - G(770000)=2592244449676198
;;  <INFO> [16:00:58] cl-user p752-2.lisp (large-g) - G(780000)=2693695470912876
;;  <INFO> [16:01:53] cl-user p752-2.lisp (large-g) - G(790000)=2801987019661918
;;  <INFO> [16:02:45] cl-user p752-2.lisp (large-g) - G(800000)=2909750084947622
;;  <INFO> [16:03:37] cl-user p752-2.lisp (large-g) - G(810000)=3003619587668629
;;  <INFO> [16:04:30] cl-user p752-2.lisp (large-g) - G(820000)=3122853490026898
;;  <INFO> [16:05:23] cl-user p752-2.lisp (large-g) - G(830000)=3231040492546216
;;  <INFO> [16:06:17] cl-user p752-2.lisp (large-g) - G(840000)=3338483910008348
;;  <INFO> [16:07:11] cl-user p752-2.lisp (large-g) - G(850000)=3461464170363836
;;  <INFO> [16:08:08] cl-user p752-2.lisp (large-g) - G(860000)=3585154009976822
;;  <INFO> [16:09:08] cl-user p752-2.lisp (large-g) - G(870000)=3714872472091806
;;  <INFO> [16:10:05] cl-user p752-2.lisp (large-g) - G(880000)=3838210868694196
;;  <INFO> [16:11:02] cl-user p752-2.lisp (large-g) - G(890000)=3966482944967789
;;  <INFO> [16:12:00] cl-user p752-2.lisp (large-g) - G(900000)=4096773987787277
;;  <INFO> [16:12:59] cl-user p752-2.lisp (large-g) - G(910000)=4243971267087047
;;  <INFO> [16:13:56] cl-user p752-2.lisp (large-g) - G(920000)=4375567947441809
;;  <INFO> [16:14:55] cl-user p752-2.lisp (large-g) - G(930000)=4522454625447260
;;  <INFO> [16:16:00] cl-user p752-2.lisp (large-g) - G(940000)=4659715661888668
;;  <INFO> [16:17:08] cl-user p752-2.lisp (large-g) - G(950000)=4814553351937719
;;  <INFO> [16:18:25] cl-user p752-2.lisp (large-g) - G(960000)=4965080433286464
;;  <INFO> [16:19:46] cl-user p752-2.lisp (large-g) - G(970000)=5122967457459771
;;  <INFO> [16:20:51] cl-user p752-2.lisp (large-g) - G(980000)=5271885478618993
;;  <INFO> [16:21:56] cl-user p752-2.lisp (large-g) - G(990000)=5431583338686515
;;  <INFO> [16:23:01] cl-user p752-2.lisp (large-g) - G(1000000)=5610899769745488
;; 5610899769745488
(defun large-g (n &optional (from 2))
  (let ((sum 0))
    (loop for x from from to n
          do (progn
               (incf sum (g x))
               (when (= (mod x 10000) 0)
                 (log:info "G(~A)=~A" x sum))))
    sum))

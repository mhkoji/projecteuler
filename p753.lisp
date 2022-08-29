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

#+nil
(defun sols (p)
  (let ((list nil))
    (loop for a from 1 below p do
      (loop for b from a below p do
        (let ((left (mod (+ (mod (* a a a) p)
                            (mod (* b b b) p))
                         p)))
          (loop for c from 1 below p do
            (when (= left (mod (* c c c) p))
              (if (= a b)
                  (push (list a b c) list)
                  (progn
                    (push (list a b c) list)
                    (push (list b a c) list))))))))
    list))


(defun F-1 (p)
  (let ((count 0))
    (loop for a from 1 below p do
      (loop for b from a below p do
        (let ((left (mod (+ (mod (* a a a) p)
                            (mod (* b b b) p))
                         p)))
          (loop for c from 1 below p do
            (when (= left (mod (* c c c) p))
              (incf count (if (= a b) 1 2)))))))
    count))

(defun F-2 (p)
  (let ((count 0))
    (loop for a from 1 below p do
      (when (= (mod a 100) 0)
        (print a))
      (let ((left (mod (+ (* a a a) (* 1 1 1)) p)))
        (loop for c from 1 below p do
          (when (= left (mod (* c c c) p))
            (incf count)))))
    (* count (1- p))))

(defun F-3 (p left-count-arr right-count-arr)
  (declare (fixnum p)
           ((simple-array fixnum (6000000)) left-count-arr)
           ((simple-array fixnum (6000000)) right-count-arr))
  (declare (optimize (speed 3) (debug 0)))
   (loop for i from 0 to p
         do (progn (setf (aref left-count-arr i) 0)
                   (setf (aref right-count-arr i) 0)))
   (loop for x from 1 below p do
     (let* ((xxx (rem (the fixnum (* x (rem (the fixnum (* x x)) p)))
                      p))
            (xxx+1 (the fixnum (1+ xxx))))
       (incf (aref left-count-arr xxx+1))
       (incf (aref right-count-arr xxx))))
   (let ((count 0))
     (loop for right from 1 below p do
       (let ((right-count (aref right-count-arr right))
             (left-count (aref left-count-arr right)))
         (setf count (the fixnum
                          (+ count
                             (the fixnum (* (the fixnum left-count)
                                            (the fixnum right-count))))))))
     (the fixnum (* (the fixnum count)
                    (the fixnum (1- p))))))

(defvar *left-count-arr* (make-array 6000000
                                     :element-type 'fixnum
                                     :initial-element 0
                                     :adjustable nil
                                     :fill-pointer nil))
(defvar *right-count-arr* (make-array 6000000
                                      :element-type 'fixnum
                                      :initial-element 0
                                      :adjustable nil
                                      :fill-pointer nil))

(ql:quickload :log4cl)

;; CL-USER> (main)
;;  <INFO> [09:21:21] cl-user p753.lisp (main) - started
;;  <INFO> [09:21:21] cl-user p753.lisp (main) - iter=1000, p=7919, sum=19029839336
;;  <INFO> [09:21:21] cl-user p753.lisp (main) -
;;   iter=2000, p=17389, sum=184956984938
;;  <INFO> [09:21:21] cl-user p753.lisp (main) -
;;   iter=3000, p=27449, sum=693636165416
;;  <INFO> [09:21:21] cl-user p753.lisp (main) -
;;   iter=4000, p=37813, sum=1766444542532
;;  <INFO> [09:21:22] cl-user p753.lisp (main) -
;;   iter=5000, p=48611, sum=3641147219816
;;  <INFO> [09:21:22] cl-user p753.lisp (main) -
;;   iter=6000, p=59359, sum=6565844326610
;;  <INFO> [09:21:23] cl-user p753.lisp (main) -
;;   iter=7000, p=70657, sum=10802626462844
;;  <INFO> [09:21:23] cl-user p753.lisp (main) -
;;   iter=8000, p=81799, sum=16621427393036
;;  <INFO> [09:21:24] cl-user p753.lisp (main) -
;;   iter=9000, p=93179, sum=24295708422392
;;  <INFO> [09:21:25] cl-user p753.lisp (main) -
;;   iter=10000, p=104729, sum=34097171502608
;;  <INFO> [09:21:26] cl-user p753.lisp (main) -
;;   iter=11000, p=116447, sum=46341044185532
;;  <INFO> [09:21:27] cl-user p753.lisp (main) -
;;   iter=12000, p=128189, sum=61306762402202
;;  <INFO> [09:21:28] cl-user p753.lisp (main) -
;;   iter=13000, p=139901, sum=79276623071042
;;  <INFO> [09:21:29] cl-user p753.lisp (main) -
;;   iter=14000, p=151703, sum=100566989145248
;;  <INFO> [09:21:31] cl-user p753.lisp (main) -
;;   iter=15000, p=163841, sum=125467493705870
;;  <INFO> [09:21:33] cl-user p753.lisp (main) -
;;   iter=16000, p=176081, sum=154370083256810
;;  <INFO> [09:21:34] cl-user p753.lisp (main) -
;;   iter=17000, p=187963, sum=187510589441894
;;  <INFO> [09:21:36] cl-user p753.lisp (main) -
;;   iter=18000, p=200183, sum=225193422068006
;;  <INFO> [09:21:38] cl-user p753.lisp (main) -
;;   iter=19000, p=212369, sum=267780536645330
;;  <INFO> [09:21:40] cl-user p753.lisp (main) -
;;   iter=20000, p=224737, sum=315586088946170
;;  <INFO> [09:21:42] cl-user p753.lisp (main) -
;;   iter=21000, p=237203, sum=368907658436852
;;  <INFO> [09:21:45] cl-user p753.lisp (main) -
;;   iter=22000, p=249439, sum=428160273262844
;;  <INFO> [09:21:48] cl-user p753.lisp (main) -
;;   iter=23000, p=262139, sum=493588107361190
;;  <INFO> [09:21:50] cl-user p753.lisp (main) -
;;   iter=24000, p=274529, sum=565576872457184
;;  <INFO> [09:21:53] cl-user p753.lisp (main) -
;;   iter=25000, p=287117, sum=644420496601220
;;  <INFO> [09:21:57] cl-user p753.lisp (main) -
;;   iter=26000, p=300023, sum=730607666051402
;;  <INFO> [09:22:00] cl-user p753.lisp (main) -
;;   iter=27000, p=312583, sum=824389662169844
;;  <INFO> [09:22:04] cl-user p753.lisp (main) -
;;   iter=28000, p=324949, sum=925989710035310
;;  <INFO> [09:22:08] cl-user p753.lisp (main) -
;;   iter=29000, p=337541, sum=1035709178827100
;;  <INFO> [09:22:12] cl-user p753.lisp (main) -
;;   iter=30000, p=350377, sum=1154042951823020
;;  <INFO> [09:22:17] cl-user p753.lisp (main) -
;;   iter=31000, p=363269, sum=1281284050436528
;;  <INFO> [09:22:22] cl-user p753.lisp (main) -
;;   iter=32000, p=376127, sum=1417971033410666
;;  <INFO> [09:22:27] cl-user p753.lisp (main) -
;;   iter=33000, p=389171, sum=1564411343247704
;;  <INFO> [09:22:33] cl-user p753.lisp (main) -
;;   iter=34000, p=401987, sum=1720797737229578
;;  <INFO> [09:22:39] cl-user p753.lisp (main) -
;;   iter=35000, p=414977, sum=1887709287631646
;;  <INFO> [09:22:46] cl-user p753.lisp (main) -
;;   iter=36000, p=427991, sum=2065341752749994
;;  <INFO> [09:22:54] cl-user p753.lisp (main) -
;;   iter=37000, p=440723, sum=2253975734309306
;;  <INFO> [09:23:02] cl-user p753.lisp (main) -
;;   iter=38000, p=453889, sum=2453981679401210
;;  <INFO> [09:23:10] cl-user p753.lisp (main) -
;;   iter=39000, p=467213, sum=2666067804931508
;;  <INFO> [09:23:19] cl-user p753.lisp (main) -
;;   iter=40000, p=479909, sum=2890266006174308
;;  <INFO> [09:23:28] cl-user p753.lisp (main) -
;;   iter=41000, p=493127, sum=3126981057703880
;;  <INFO> [09:23:38] cl-user p753.lisp (main) -
;;   iter=42000, p=506131, sum=3376547351822084
;;  <INFO> [09:23:48] cl-user p753.lisp (main) -
;;   iter=43000, p=519227, sum=3639466032448688
;;  <INFO> [09:23:57] cl-user p753.lisp (main) -
;;   iter=44000, p=532333, sum=3915872798755016
;;  <INFO> [09:24:08] cl-user p753.lisp (main) -
;;   iter=45000, p=545747, sum=4206410591549378
;;  <INFO> [09:24:18] cl-user p753.lisp (main) -
;;   iter=46000, p=559081, sum=4511541200955554
;;  <INFO> [09:24:29] cl-user p753.lisp (main) -
;;   iter=47000, p=572311, sum=4831574081244806
;;  <INFO> [09:24:41] cl-user p753.lisp (main) -
;;   iter=48000, p=585493, sum=5166673608160406
;;  <INFO> [09:24:53] cl-user p753.lisp (main) -
;;   iter=49000, p=598687, sum=5517154943085128
;;  <INFO> [09:25:05] cl-user p753.lisp (main) -
;;   iter=50000, p=611953, sum=5883546848446826
;;  <INFO> [09:25:18] cl-user p753.lisp (main) -
;;   iter=51000, p=625187, sum=6266132903328092
;;  <INFO> [09:25:32] cl-user p753.lisp (main) -
;;   iter=52000, p=638977, sum=6665707359521132
;;  <INFO> [09:25:45] cl-user p753.lisp (main) -
;;   iter=53000, p=652429, sum=7082678791448894
;;  <INFO> [09:26:00] cl-user p753.lisp (main) -
;;   iter=54000, p=665659, sum=7517102638321898
;;  <INFO> [09:26:15] cl-user p753.lisp (main) -
;;   iter=55000, p=679277, sum=7969365555529736
;;  <INFO> [09:26:30] cl-user p753.lisp (main) -
;;   iter=56000, p=692543, sum=8439810291937244
;;  <INFO> [09:26:46] cl-user p753.lisp (main) -
;;   iter=57000, p=706019, sum=8928877336684412
;;  <INFO> [09:27:02] cl-user p753.lisp (main) -
;;   iter=58000, p=719639, sum=9436804082238974
;;  <INFO> [09:27:19] cl-user p753.lisp (main) -
;;   iter=59000, p=732923, sum=9964150592828990
;;  <INFO> [09:27:36] cl-user p753.lisp (main) -
;;   iter=60000, p=746773, sum=10511365027540694
;;  <INFO> [09:27:56] cl-user p753.lisp (main) -
;;   iter=61000, p=760267, sum=11079206865745568
;;  <INFO> [09:28:17] cl-user p753.lisp (main) -
;;   iter=62000, p=773603, sum=11667339948266852
;;  <INFO> [09:28:38] cl-user p753.lisp (main) -
;;   iter=63000, p=787207, sum=12276413868565904
;;  <INFO> [09:29:00] cl-user p753.lisp (main) -
;;   iter=64000, p=800573, sum=12906695196267194
;;  <INFO> [09:29:22] cl-user p753.lisp (main) -
;;   iter=65000, p=814279, sum=13558648312428902
;;  <INFO> [09:29:43] cl-user p753.lisp (main) -
;;   iter=66000, p=827719, sum=14232800775601238
;;  <INFO> [09:30:04] cl-user p753.lisp (main) -
;;   iter=67000, p=841459, sum=14929319456570930
;;  <INFO> [09:30:24] cl-user p753.lisp (main) -
;;   iter=68000, p=855359, sum=15649125255899048
;;  <INFO> [09:30:46] cl-user p753.lisp (main) -
;;   iter=69000, p=868771, sum=16392306206028374
;;  <INFO> [09:31:08] cl-user p753.lisp (main) -
;;   iter=70000, p=882377, sum=17158952305727024
;;  <INFO> [09:31:30] cl-user p753.lisp (main) -
;;   iter=71000, p=896009, sum=17949691137759794
;;  <INFO> [09:31:53] cl-user p753.lisp (main) -
;;   iter=72000, p=909683, sum=18765210268129718
;;  <INFO> [09:32:17] cl-user p753.lisp (main) -
;;   iter=73000, p=923591, sum=19605672010394162
;;  <INFO> [09:32:40] cl-user p753.lisp (main) -
;;   iter=74000, p=937379, sum=20471312131778432
;;  <INFO> [09:33:05] cl-user p753.lisp (main) -
;;   iter=75000, p=951161, sum=21362967331805480
;;  <INFO> [09:33:31] cl-user p753.lisp (main) -
;;   iter=76000, p=965113, sum=22281231687670226
;;  <INFO> [09:33:59] cl-user p753.lisp (main) -
;;   iter=77000, p=978947, sum=23226061969421360
;;  <INFO> [09:34:27] cl-user p753.lisp (main) -
;;   iter=78000, p=993107, sum=24198445112617208
;;  <INFO> [09:34:57] cl-user p753.lisp (main) -
;;   iter=79000, p=1006721, sum=25198404994058468
;;  <INFO> [09:35:27] cl-user p753.lisp (main) -
;;   iter=80000, p=1020379, sum=26225604859297610
;;  <INFO> [09:35:53] cl-user p753.lisp (main) -
;;   iter=81000, p=1034221, sum=27280868093499548
;;  <INFO> [09:36:21] cl-user p753.lisp (main) -
;;   iter=82000, p=1048129, sum=28365054948547502
;;  <INFO> [09:36:49] cl-user p753.lisp (main) -
;;   iter=83000, p=1062511, sum=29478731901359252
;;  <INFO> [09:37:17] cl-user p753.lisp (main) -
;;   iter=84000, p=1076143, sum=30622292934557870
;;  <INFO> [09:37:46] cl-user p753.lisp (main) -
;;   iter=85000, p=1090373, sum=31795561535161184
;;  <INFO> [09:38:15] cl-user p753.lisp (main) -
;;   iter=86000, p=1103923, sum=32999058107654102
;;  <INFO> [09:38:45] cl-user p753.lisp (main) -
;;   iter=87000, p=1117579, sum=34232622067693364
;;  <INFO> [09:39:15] cl-user p753.lisp (main) -
;;   iter=88000, p=1131617, sum=35497539015414014
;;  <INFO> [09:39:46] cl-user p753.lisp (main) -
;;   iter=89000, p=1145689, sum=36793885186390112
;;  <INFO> [09:40:17] cl-user p753.lisp (main) -
;;   iter=90000, p=1159523, sum=38122603345316198
;;  <INFO> [09:40:48] cl-user p753.lisp (main) -
;;   iter=91000, p=1173301, sum=39482962881306308
;;  <INFO> [09:41:19] cl-user p753.lisp (main) -
;;   iter=92000, p=1187003, sum=40875684331417802
;;  <INFO> [09:41:52] cl-user p753.lisp (main) -
;;   iter=93000, p=1200949, sum=42301065489282116
;;  <INFO> [09:42:25] cl-user p753.lisp (main) -
;;   iter=94000, p=1215133, sum=43760704378052084
;;  <INFO> [09:42:58] cl-user p753.lisp (main) -
;;   iter=95000, p=1229269, sum=45254377603836392
;;  <INFO> [09:43:32] cl-user p753.lisp (main) -
;;   iter=96000, p=1243709, sum=46783347144268628
;;  <INFO> [09:44:06] cl-user p753.lisp (main) -
;;   iter=97000, p=1257517, sum=48347307643317332
;;  <INFO> [09:44:41] cl-user p753.lisp (main) -
;;   iter=98000, p=1271293, sum=49946137064231522
;;  <INFO> [09:45:17] cl-user p753.lisp (main) -
;;   iter=99000, p=1285517, sum=51580629916490438
;;  <INFO> [09:45:53] cl-user p753.lisp (main) -
;;   iter=100000, p=1299709, sum=53251182622586120
;;  <INFO> [09:46:29] cl-user p753.lisp (main) -
;;   iter=101000, p=1313839, sum=54958779755895602
;;  <INFO> [09:47:06] cl-user p753.lisp (main) -
;;   iter=102000, p=1327901, sum=56703454252472168
;;  <INFO> [09:47:43] cl-user p753.lisp (main) -
;;   iter=103000, p=1342051, sum=58485750170760908
;;  <INFO> [09:48:21] cl-user p753.lisp (main) -
;;   iter=104000, p=1356227, sum=60305974594294988
;;  <INFO> [09:48:59] cl-user p753.lisp (main) -
;;   iter=105000, p=1370459, sum=62164800909608870
;;  <INFO> [09:49:38] cl-user p753.lisp (main) -
;;   iter=106000, p=1384631, sum=64062231377499362
;;  <INFO> [09:50:17] cl-user p753.lisp (main) -
;;   iter=107000, p=1398367, sum=65998761840427298
;;  <INFO> [09:50:56] cl-user p753.lisp (main) -
;;   iter=108000, p=1412641, sum=67974322874608538
;;  <INFO> [09:51:36] cl-user p753.lisp (main) -
;;   iter=109000, p=1427039, sum=69990972336360068
;;  <INFO> [09:52:16] cl-user p753.lisp (main) -
;;   iter=110000, p=1441049, sum=72047486065754558
;;  <INFO> [09:52:57] cl-user p753.lisp (main) -
;;   iter=111000, p=1455089, sum=74144706618110546
;;  <INFO> [09:53:39] cl-user p753.lisp (main) -
;;   iter=112000, p=1469393, sum=76282792306655108
;;  <INFO> [09:54:21] cl-user p753.lisp (main) -
;;   iter=113000, p=1483451, sum=78462841565415638
;;  <INFO> [09:55:03] cl-user p753.lisp (main) -
;;   iter=114000, p=1497541, sum=80684652374637182
;;  <INFO> [09:55:46] cl-user p753.lisp (main) -
;;   iter=115000, p=1511539, sum=82948518750660794
;;  <INFO> [09:56:30] cl-user p753.lisp (main) -
;;   iter=116000, p=1525921, sum=85255105033313618
;;  <INFO> [09:57:14] cl-user p753.lisp (main) -
;;   iter=117000, p=1540499, sum=87606182614525856
;;  <INFO> [09:57:58] cl-user p753.lisp (main) -
;;   iter=118000, p=1554881, sum=90001665286965656
;;  <INFO> [09:58:43] cl-user p753.lisp (main) -
;;   iter=119000, p=1569173, sum=92441194306418588
;;  <INFO> [09:59:28] cl-user p753.lisp (main) -
;;   iter=120000, p=1583539, sum=94925806528708142
;;  <INFO> [10:00:13] cl-user p753.lisp (main) -
;;   iter=121000, p=1598011, sum=97456532867614748
;;  <INFO> [10:00:59] cl-user p753.lisp (main) -
;;   iter=122000, p=1612133, sum=100032872517524936
;;  <INFO> [10:01:46] cl-user p753.lisp (main) -
;;   iter=123000, p=1626479, sum=102655445953494134
;;  <INFO> [10:02:32] cl-user p753.lisp (main) -
;;   iter=124000, p=1640833, sum=105323855450417780
;;  <INFO> [10:03:19] cl-user p753.lisp (main) -
;;   iter=125000, p=1655131, sum=108039978334937348
;;  <INFO> [10:04:07] cl-user p753.lisp (main) -
;;   iter=126000, p=1669313, sum=110803356417569126
;;  <INFO> [10:04:55] cl-user p753.lisp (main) -
;;   iter=127000, p=1683673, sum=113613861416894624
;;  <INFO> [10:05:43] cl-user p753.lisp (main) -
;;   iter=128000, p=1698077, sum=116473258439724854
;;  <INFO> [10:06:32] cl-user p753.lisp (main) -
;;   iter=129000, p=1712353, sum=119380904626118300
;;  <INFO> [10:07:21] cl-user p753.lisp (main) -
;;   iter=130000, p=1726943, sum=122338389434945678
;;  <INFO> [10:08:11] cl-user p753.lisp (main) -
;;   iter=131000, p=1741409, sum=125346015181723124
;;  <INFO> [10:09:01] cl-user p753.lisp (main) -
;;   iter=132000, p=1755563, sum=128403453235036196
;;  <INFO> [10:09:52] cl-user p753.lisp (main) -
;;   iter=133000, p=1770437, sum=131511984980408852
;;  <INFO> [10:10:43] cl-user p753.lisp (main) -
;;   iter=134000, p=1784633, sum=134671768581419594
;;  <INFO> [10:11:34] cl-user p753.lisp (main) -
;;   iter=135000, p=1798967, sum=137882462882173616
;;  <INFO> [10:12:27] cl-user p753.lisp (main) -
;;   iter=136000, p=1813351, sum=141144682740197474
;;  <INFO> [10:13:19] cl-user p753.lisp (main) -
;;   iter=137000, p=1827659, sum=144459280113637754
;;  <INFO> [10:14:12] cl-user p753.lisp (main) -
;;   iter=138000, p=1842349, sum=147826086783188708
;;  <INFO> [10:15:06] cl-user p753.lisp (main) -
;;   iter=139000, p=1856297, sum=151246454905150280
;;  <INFO> [10:16:00] cl-user p753.lisp (main) -
;;   iter=140000, p=1870667, sum=154719128590939748
;;  <INFO> [10:16:54] cl-user p753.lisp (main) -
;;   iter=141000, p=1885201, sum=158245847362808888
;;  <INFO> [10:17:49] cl-user p753.lisp (main) -
;;   iter=142000, p=1899481, sum=161826509975791592
;;  <INFO> [10:18:44] cl-user p753.lisp (main) -
;;   iter=143000, p=1913983, sum=165462646474319522
;;  <INFO> [10:19:40] cl-user p753.lisp (main) -
;;   iter=144000, p=1928257, sum=169153498685595656
;;  <INFO> [10:20:36] cl-user p753.lisp (main) -
;;   iter=145000, p=1942747, sum=172899753464684900
;;  <INFO> [10:21:33] cl-user p753.lisp (main) -
;;   iter=146000, p=1957129, sum=176701957875814556
;;  <INFO> [10:22:30] cl-user p753.lisp (main) -
;;   iter=147000, p=1972177, sum=180561628646480072
;;  <INFO> [10:23:28] cl-user p753.lisp (main) -
;;   iter=148000, p=1986757, sum=184480003617928352
;;  <INFO> [10:24:26] cl-user p753.lisp (main) -
;;   iter=149000, p=2000963, sum=188455743000798428
;;  <INFO> [10:25:25] cl-user p753.lisp (main) -
;;   iter=150000, p=2015177, sum=192488403409869698
;;  <INFO> [10:26:24] cl-user p753.lisp (main) -
;;   iter=151000, p=2029717, sum=196578776266888166
;;  <INFO> [10:27:26] cl-user p753.lisp (main) -
;;   iter=152000, p=2044127, sum=200727963993727496
;;  <INFO> [10:28:27] cl-user p753.lisp (main) -
;;   iter=153000, p=2058871, sum=204937320056420540
;;  <INFO> [10:29:28] cl-user p753.lisp (main) -
;;   iter=154000, p=2073349, sum=209206144983585602
;;  <INFO> [10:30:30] cl-user p753.lisp (main) -
;;   iter=155000, p=2088133, sum=213536101499800484
;;  <INFO> [10:31:39] cl-user p753.lisp (main) -
;;   iter=156000, p=2102717, sum=217926969152144840
;;  <INFO> [10:32:42] cl-user p753.lisp (main) -
;;   iter=157000, p=2117131, sum=222378873632033972
;;  <INFO> [10:33:46] cl-user p753.lisp (main) -
;;   iter=158000, p=2131813, sum=226892714846407772
;;  <INFO> [10:34:50] cl-user p753.lisp (main) -
;;   iter=159000, p=2146231, sum=231468380702153114
;;  <INFO> [10:35:54] cl-user p753.lisp (main) -
;;   iter=160000, p=2160553, sum=236106201867905306
;;  <INFO> [10:36:59] cl-user p753.lisp (main) -
;;   iter=161000, p=2175451, sum=240806591952109874
;;  <INFO> [10:38:04] cl-user p753.lisp (main) -
;;   iter=162000, p=2190031, sum=245570663370196742
;;  <INFO> [10:39:09] cl-user p753.lisp (main) -
;;   iter=163000, p=2204827, sum=250399428269804276
;;  <INFO> [10:40:14] cl-user p753.lisp (main) -
;;   iter=164000, p=2219641, sum=255292938753619118
;;  <INFO> [10:41:20] cl-user p753.lisp (main) -
;;   iter=165000, p=2234129, sum=260252349243189470
;;  <INFO> [10:42:27] cl-user p753.lisp (main) -
;;   iter=166000, p=2248723, sum=265276265108403254
;;  <INFO> [10:43:34] cl-user p753.lisp (main) -
;;   iter=167000, p=2263381, sum=270366522053075702
;;  <INFO> [10:44:41] cl-user p753.lisp (main) -
;;   iter=168000, p=2277553, sum=275521118055809990
;;  <INFO> [10:45:49] cl-user p753.lisp (main) -
;;   iter=169000, p=2292469, sum=280742847076291436
;;  <INFO> [10:46:57] cl-user p753.lisp (main) -
;;   iter=170000, p=2307229, sum=286032032834882972
;;  <INFO> [10:48:06] cl-user p753.lisp (main) -
;;   iter=171000, p=2322143, sum=291389306016242330
;;  <INFO> [10:49:15] cl-user p753.lisp (main) -
;;   iter=172000, p=2336861, sum=296815977296034518
;;  <INFO> [10:50:26] cl-user p753.lisp (main) -
;;   iter=173000, p=2351821, sum=302311915194714932
;;  <INFO> [10:51:36] cl-user p753.lisp (main) -
;;   iter=174000, p=2366297, sum=307877487586507328
;;  <INFO> [10:52:47] cl-user p753.lisp (main) -
;;   iter=175000, p=2381147, sum=313512159607284404
;;  <INFO> [10:53:58] cl-user p753.lisp (main) -
;;   iter=176000, p=2395867, sum=319216386668511206
;;  <INFO> [10:55:10] cl-user p753.lisp (main) -
;;   iter=177000, p=2410621, sum=324991029028168298
;;  <INFO> [10:56:22] cl-user p753.lisp (main) -
;;   iter=178000, p=2425229, sum=330837050087975594
;;  <INFO> [10:57:35] cl-user p753.lisp (main) -
;;   iter=179000, p=2440219, sum=336754725177780326
;;  <INFO> [10:58:48] cl-user p753.lisp (main) -
;;   iter=180000, p=2454587, sum=342744460260744674
;;  <INFO> [11:00:02] cl-user p753.lisp (main) -
;;   iter=181000, p=2469413, sum=348805468001622872
;;  <INFO> [11:01:16] cl-user p753.lisp (main) -
;;   iter=182000, p=2484331, sum=354940249349351480
;;  <INFO> [11:02:31] cl-user p753.lisp (main) -
;;   iter=183000, p=2499023, sum=361148319747290162
;;  <INFO> [11:03:46] cl-user p753.lisp (main) -
;;   iter=184000, p=2513591, sum=367429960381288568
;;  <INFO> [11:05:01] cl-user p753.lisp (main) -
;;   iter=185000, p=2528411, sum=373785643639520006
;;  <INFO> [11:06:17] cl-user p753.lisp (main) -
;;   iter=186000, p=2543221, sum=380215062929585654
;;  <INFO> [11:07:33] cl-user p753.lisp (main) -
;;   iter=187000, p=2558009, sum=386720566406828960
;;  <INFO> [11:08:51] cl-user p753.lisp (main) -
;;   iter=188000, p=2572649, sum=393301474447558130
;;  <INFO> [11:10:09] cl-user p753.lisp (main) -
;;   iter=189000, p=2587007, sum=399957004569554858
;;  <INFO> [11:11:26] cl-user p753.lisp (main) -
;;   iter=190000, p=2601857, sum=406687911929924048
;;  <INFO> [11:12:46] cl-user p753.lisp (main) -
;;   iter=191000, p=2616703, sum=413496916349089970
;;  <INFO> [11:14:05] cl-user p753.lisp (main) -
;;   iter=192000, p=2631529, sum=420383424681759266
;;  <INFO> [11:15:24] cl-user p753.lisp (main) -
;;   iter=193000, p=2646089, sum=427347161264222696
;;  <INFO> [11:16:43] cl-user p753.lisp (main) -
;;   iter=194000, p=2660753, sum=434388372824417756
;;  <INFO> [11:18:03] cl-user p753.lisp (main) -
;;   iter=195000, p=2675909, sum=441508361635103762
;;  <INFO> [11:19:24] cl-user p753.lisp (main) -
;;   iter=196000, p=2690557, sum=448708502814751310
;;  <INFO> [11:20:45] cl-user p753.lisp (main) -
;;   iter=197000, p=2705243, sum=455987282459364260
;;  <INFO> [11:22:06] cl-user p753.lisp (main) -
;;   iter=198000, p=2719631, sum=463345398096004304
;;  <INFO> [11:23:28] cl-user p753.lisp (main) -
;;   iter=199000, p=2735129, sum=470784631643854148
;;  <INFO> [11:24:51] cl-user p753.lisp (main) -
;;   iter=200000, p=2750159, sum=478306612021390352
;;  <INFO> [11:26:14] cl-user p753.lisp (main) -
;;   iter=201000, p=2764873, sum=485910486687642380
;;  <INFO> [11:27:38] cl-user p753.lisp (main) -
;;   iter=202000, p=2779771, sum=493595998648990982
;;  <INFO> [11:29:02] cl-user p753.lisp (main) -
;;   iter=203000, p=2794723, sum=501364428612312680
;;  <INFO> [11:30:27] cl-user p753.lisp (main) -
;;   iter=204000, p=2809309, sum=509216146588254158
;;  <INFO> [11:31:53] cl-user p753.lisp (main) -
;;   iter=205000, p=2823589, sum=517148170115917430
;;  <INFO> [11:33:18] cl-user p753.lisp (main) -
;;   iter=206000, p=2838169, sum=525163362184868222
;;  <INFO> [11:34:44] cl-user p753.lisp (main) -
;;   iter=207000, p=2853187, sum=533261598034464854
;;  <INFO> [11:36:11] cl-user p753.lisp (main) -
;;   iter=208000, p=2868043, sum=541445544199329500
;;  <INFO> [11:37:38] cl-user p753.lisp (main) -
;;   iter=209000, p=2883371, sum=549715680602026334
;;  <INFO> [11:39:05] cl-user p753.lisp (main) -
;;   iter=210000, p=2898527, sum=558072663844936352
;;  <INFO> [11:40:33] cl-user p753.lisp (main) -
;;   iter=211000, p=2913259, sum=566516142329641280
;;  <INFO> [11:42:02] cl-user p753.lisp (main) -
;;   iter=212000, p=2928151, sum=575046706314692678
;;  <INFO> [11:43:31] cl-user p753.lisp (main) -
;;   iter=213000, p=2943257, sum=583664646186635360
;;  <INFO> [11:45:00] cl-user p753.lisp (main) -
;;   iter=214000, p=2958383, sum=592371483067587206
;;  <INFO> [11:46:30] cl-user p753.lisp (main) -
;;   iter=215000, p=2973059, sum=601166532517084910
;;  <INFO> [11:48:01] cl-user p753.lisp (main) -
;;   iter=216000, p=2987843, sum=610049974500946760
;;  <INFO> [11:49:32] cl-user p753.lisp (main) -
;;   iter=217000, p=3002743, sum=619022603873960120
;;  <INFO> [11:51:03] cl-user p753.lisp (main) -
;;   iter=218000, p=3017689, sum=628083549886401938
;;  <INFO> [11:52:35] cl-user p753.lisp (main) -
;;   iter=219000, p=3032831, sum=637236189247194122
;;  <INFO> [11:54:08] cl-user p753.lisp (main) -
;;   iter=220000, p=3047767, sum=646480430913783074
;;  <INFO> [11:55:40] cl-user p753.lisp (main) -
;;   iter=221000, p=3062989, sum=655816349987774030
;;  <INFO> [11:57:14] cl-user p753.lisp (main) -
;;   iter=222000, p=3077687, sum=665244366183980324
;;  <INFO> [11:58:48] cl-user p753.lisp (main) -
;;   iter=223000, p=3092371, sum=674762262701472218
;;  <INFO> [12:00:22] cl-user p753.lisp (main) -
;;   iter=224000, p=3107633, sum=684372515491500314
;;  <INFO> [12:01:57] cl-user p753.lisp (main) -
;;   iter=225000, p=3122321, sum=694075537668678788
;;  <INFO> [12:03:32] cl-user p753.lisp (main) -
;;   iter=226000, p=3137447, sum=703871394716567528
;;  <INFO> [12:05:08] cl-user p753.lisp (main) -
;;   iter=227000, p=3152341, sum=713762378369502224
;;  <INFO> [12:06:44] cl-user p753.lisp (main) -
;;   iter=228000, p=3167141, sum=723746964048734972
;;  <INFO> [12:08:23] cl-user p753.lisp (main) -
;;   iter=229000, p=3182341, sum=733825717995697610
;;  <INFO> [12:10:02] cl-user p753.lisp (main) -
;;   iter=230000, p=3196933, sum=743999239064469824
;;  <INFO> [12:11:41] cl-user p753.lisp (main) -
;;   iter=231000, p=3211441, sum=754266006249230702
;;  <INFO> [12:13:20] cl-user p753.lisp (main) -
;;   iter=232000, p=3226423, sum=764627483191551650
;;  <INFO> [12:15:00] cl-user p753.lisp (main) -
;;   iter=233000, p=3241201, sum=775084737067574504
;;  <INFO> [12:16:40] cl-user p753.lisp (main) -
;;   iter=234000, p=3256423, sum=785639574491573654
;;  <INFO> [12:18:19] cl-user p753.lisp (main) -
;;   iter=235000, p=3271277, sum=796292093108984390
;;  <INFO> [12:19:58] cl-user p753.lisp (main) -
;;   iter=236000, p=3286331, sum=807042438438831002
;;  <INFO> [12:21:39] cl-user p753.lisp (main) -
;;   iter=237000, p=3301591, sum=817891577639319884
;;  <INFO> [12:23:19] cl-user p753.lisp (main) -
;;   iter=238000, p=3316427, sum=828841084771420412
;;  <INFO> [12:25:00] cl-user p753.lisp (main) -
;;   iter=239000, p=3331423, sum=839889848111298410
;;  <INFO> [12:26:42] cl-user p753.lisp (main) -
;;   iter=240000, p=3346601, sum=851039226219462212
;;  <INFO> [12:28:24] cl-user p753.lisp (main) -
;;   iter=241000, p=3362159, sum=862290398279740346
;;  <INFO> [12:30:07] cl-user p753.lisp (main) -
;;   iter=242000, p=3376991, sum=873643940268833966
;;  <INFO> [12:31:50] cl-user p753.lisp (main) -
;;   iter=243000, p=3391831, sum=885097922989046648
;;  <INFO> [12:33:34] cl-user p753.lisp (main) -
;;   iter=244000, p=3406801, sum=896653270338542378
;;  <INFO> [12:35:18] cl-user p753.lisp (main) -
;;   iter=245000, p=3421751, sum=908309984444166086
;;  <INFO> [12:37:03] cl-user p753.lisp (main) -
;;   iter=246000, p=3436571, sum=920069048013612398
;;  <INFO> [12:38:48] cl-user p753.lisp (main) -
;;   iter=247000, p=3451229, sum=931929918743602586
;;  <INFO> [12:40:34] cl-user p753.lisp (main) -
;;   iter=248000, p=3466829, sum=943895493336468302
;;  <INFO> [12:42:20] cl-user p753.lisp (main) -
;;   iter=249000, p=3482333, sum=955968181344591698
;;  <INFO> [12:44:07] cl-user p753.lisp (main) -
;;   iter=250000, p=3497861, sum=968149021040247596
;;  <INFO> [12:45:54] cl-user p753.lisp (main) -
;;   iter=251000, p=3512869, sum=980437005853311554
;;  <INFO> [12:47:42] cl-user p753.lisp (main) -
;;   iter=252000, p=3528403, sum=992831587006848602
;;  <INFO> [12:49:30] cl-user p753.lisp (main) -
;;   iter=253000, p=3543791, sum=1005335403424375358
;;  <INFO> [12:51:19] cl-user p753.lisp (main) -
;;   iter=254000, p=3558913, sum=1017947674464663194
;;  <INFO> [12:53:08] cl-user p753.lisp (main) -
;;   iter=255000, p=3573877, sum=1030666337917844858
;;  <INFO> [12:54:58] cl-user p753.lisp (main) -
;;   iter=256000, p=3588941, sum=1043493959698755842
;;  <INFO> [12:56:48] cl-user p753.lisp (main) -
;;   iter=257000, p=3604121, sum=1056429689675241602
;;  <INFO> [12:58:39] cl-user p753.lisp (main) -
;;   iter=258000, p=3618497, sum=1069471425346063250
;;  <INFO> [13:00:30] cl-user p753.lisp (main) -
;;   iter=259000, p=3633347, sum=1082618849458836008
;;  <INFO> [13:02:22] cl-user p753.lisp (main) -
;;   iter=260000, p=3648923, sum=1095876801456639326
;;  <INFO> [13:04:14] cl-user p753.lisp (main) -
;;   iter=261000, p=3664351, sum=1109248741343062880
;;  <INFO> [13:06:06] cl-user p753.lisp (main) -
;;   iter=262000, p=3678931, sum=1122730854571670648
;;  <INFO> [13:07:59] cl-user p753.lisp (main) -
;;   iter=263000, p=3694217, sum=1136322169920765068
;;  <INFO> [13:09:53] cl-user p753.lisp (main) -
;;   iter=264000, p=3709073, sum=1150024032551246150
;;  <INFO> [13:11:47] cl-user p753.lisp (main) -
;;   iter=265000, p=3724223, sum=1163837983075478774
;;  <INFO> [13:13:42] cl-user p753.lisp (main) -
;;   iter=266000, p=3738937, sum=1177761650525268362
;;  <INFO> [13:15:37] cl-user p753.lisp (main) -
;;   iter=267000, p=3754087, sum=1191799399163336960
;;  <INFO> [13:17:33] cl-user p753.lisp (main) -
;;   iter=268000, p=3769763, sum=1205951716229039018
;;  <INFO> [13:19:29] cl-user p753.lisp (main) -
;;   iter=269000, p=3784943, sum=1220220738468304022
;;  <INFO> [13:21:25] cl-user p753.lisp (main) -
;;   iter=270000, p=3800201, sum=1234605099176525036
;;  <INFO> [13:23:22] cl-user p753.lisp (main) -
;;   iter=271000, p=3815209, sum=1249104570043608302
;;  <INFO> [13:25:19] cl-user p753.lisp (main) -
;;   iter=272000, p=3830003, sum=1263716111137521128
;;  <INFO> [13:27:18] cl-user p753.lisp (main) -
;;   iter=273000, p=3845273, sum=1278444137780083934
;;  <INFO> [13:29:17] cl-user p753.lisp (main) -
;;   iter=274000, p=3860347, sum=1293288298999665344
;;  <INFO> [13:31:17] cl-user p753.lisp (main) -
;;   iter=275000, p=3875827, sum=1308251076511809482
;;  <INFO> [13:33:17] cl-user p753.lisp (main) -
;;   iter=276000, p=3890969, sum=1323332502809918594
;;  <INFO> [13:35:17] cl-user p753.lisp (main) -
;;   iter=277000, p=3906319, sum=1338530233727115086
;;  <INFO> [13:37:18] cl-user p753.lisp (main) -
;;   iter=278000, p=3921217, sum=1353846999044631956
;;  <INFO> [13:39:19] cl-user p753.lisp (main) -
;;   iter=279000, p=3936301, sum=1369281714767830832
;;  <INFO> [13:41:21] cl-user p753.lisp (main) -
;;   iter=280000, p=3951161, sum=1384835201226002804
;;  <INFO> [13:43:23] cl-user p753.lisp (main) -
;;   iter=281000, p=3967043, sum=1400509088528520488
;;  <INFO> [13:45:26] cl-user p753.lisp (main) -
;;   iter=282000, p=3981883, sum=1416305465125977146
;;  <INFO> [13:47:29] cl-user p753.lisp (main) -
;;   iter=283000, p=3997859, sum=1432225120097283896
;;  <INFO> [13:49:33] cl-user p753.lisp (main) -
;;   iter=284000, p=4012847, sum=1448267784970800728
;;  <INFO> [13:51:38] cl-user p753.lisp (main) -
;;   iter=285000, p=4027913, sum=1464431563609608176
;;  <INFO> [13:53:42] cl-user p753.lisp (main) -
;;   iter=286000, p=4042729, sum=1480716250026663224
;;  <INFO> [13:55:48] cl-user p753.lisp (main) -
;;   iter=287000, p=4057967, sum=1497120701056234580
;;  <INFO> [13:57:54] cl-user p753.lisp (main) -
;;   iter=288000, p=4073233, sum=1513650214154452202
;;  <INFO> [14:00:00] cl-user p753.lisp (main) -
;;   iter=289000, p=4088237, sum=1530302661938629370
;;  <INFO> [14:02:07] cl-user p753.lisp (main) -
;;   iter=290000, p=4103629, sum=1547078856795837818
;;  <INFO> [14:04:15] cl-user p753.lisp (main) -
;;   iter=291000, p=4119053, sum=1563980148243053150
;;  <INFO> [14:06:23] cl-user p753.lisp (main) -
;;   iter=292000, p=4134629, sum=1581010365466614404
;;  <INFO> [14:08:31] cl-user p753.lisp (main) -
;;   iter=293000, p=4149889, sum=1598169600436130192
;;  <INFO> [14:10:41] cl-user p753.lisp (main) -
;;   iter=294000, p=4164637, sum=1615452775348391972
;;  <INFO> [14:12:51] cl-user p753.lisp (main) -
;;   iter=295000, p=4180097, sum=1632861772445751914
;;  <INFO> [14:15:00] cl-user p753.lisp (main) -
;;   iter=296000, p=4195057, sum=1650397231632354614
;;  <INFO> [14:17:11] cl-user p753.lisp (main) -
;;   iter=297000, p=4210253, sum=1668059956783902944
;;  <INFO> [14:19:21] cl-user p753.lisp (main) -
;;   iter=298000, p=4225457, sum=1685850719139112622
;;  <INFO> [14:21:32] cl-user p753.lisp (main) -
;;   iter=299000, p=4241099, sum=1703771554420548068
;;  <INFO> [14:23:44] cl-user p753.lisp (main) -
;;   iter=300000, p=4256233, sum=1721823064662644972
;;  <INFO> [14:25:56] cl-user p753.lisp (main) -
;;   iter=301000, p=4271581, sum=1740003968282514464
;;  <INFO> [14:28:09] cl-user p753.lisp (main) -
;;   iter=302000, p=4287253, sum=1758316879599048854
;;  <INFO> [14:30:23] cl-user p753.lisp (main) -
;;   iter=303000, p=4302631, sum=1776763741145863208
;;  <INFO> [14:32:36] cl-user p753.lisp (main) -
;;   iter=304000, p=4317823, sum=1795341478874102804
;;  <INFO> [14:34:50] cl-user p753.lisp (main) -
;;   iter=305000, p=4333097, sum=1814051724284882450
;;  <INFO> [14:37:05] cl-user p753.lisp (main) -
;;   iter=306000, p=4348691, sum=1832895367852726022
;;  <INFO> [14:39:20] cl-user p753.lisp (main) -
;;   iter=307000, p=4364111, sum=1851873879887969678
;;  <INFO> [14:41:36] cl-user p753.lisp (main) -
;;   iter=308000, p=4379467, sum=1870986206201405972
;;  <INFO> [14:43:52] cl-user p753.lisp (main) -
;;   iter=309000, p=4394783, sum=1890231587709964538
;;  <INFO> [14:46:09] cl-user p753.lisp (main) -
;;   iter=310000, p=4410317, sum=1909614194421563708
;;  <INFO> [14:48:27] cl-user p753.lisp (main) -
;;   iter=311000, p=4425107, sum=1929130722645326420
;;  <INFO> [14:50:45] cl-user p753.lisp (main) -
;;   iter=312000, p=4440041, sum=1948778809267980470
;;  <INFO> [14:53:04] cl-user p753.lisp (main) -
;;   iter=313000, p=4455043, sum=1968559191718439816
;;  <INFO> [14:55:23] cl-user p753.lisp (main) -
;;   iter=314000, p=4470287, sum=1988474960155607732
;;  <INFO> [14:57:42] cl-user p753.lisp (main) -
;;   iter=315000, p=4485479, sum=2008527339211039154
;;  <INFO> [15:00:02] cl-user p753.lisp (main) -
;;   iter=316000, p=4500731, sum=2028714459548540864
;;  <INFO> [15:02:23] cl-user p753.lisp (main) -
;;   iter=317000, p=4516231, sum=2049040943863110638
;;  <INFO> [15:04:43] cl-user p753.lisp (main) -
;;   iter=318000, p=4531811, sum=2069508339977152538
;;  <INFO> [15:07:05] cl-user p753.lisp (main) -
;;   iter=319000, p=4547239, sum=2090116978670203640
;;  <INFO> [15:09:27] cl-user p753.lisp (main) -
;;   iter=320000, p=4562693, sum=2110865024387944898
;;  <INFO> [15:11:49] cl-user p753.lisp (main) -
;;   iter=321000, p=4577927, sum=2131751639370657176
;;  <INFO> [15:14:12] cl-user p753.lisp (main) -
;;   iter=322000, p=4593089, sum=2152778823530419172
;;  <INFO> [15:16:36] cl-user p753.lisp (main) -
;;   iter=323000, p=4608493, sum=2173946693686358954
;;  <INFO> [15:19:00] cl-user p753.lisp (main) -
;;   iter=324000, p=4623793, sum=2195254914015366818
;;  <INFO> [15:21:24] cl-user p753.lisp (main) -
;;   iter=325000, p=4639267, sum=2216705949824448482
;;  <INFO> [15:23:49] cl-user p753.lisp (main) -
;;   iter=326000, p=4654697, sum=2238300436048493906
;;  <INFO> [15:26:15] cl-user p753.lisp (main) -
;;   iter=327000, p=4670207, sum=2260040368475052686
;;  <INFO> [15:28:41] cl-user p753.lisp (main) -
;;   iter=328000, p=4685257, sum=2281921729751794394
;;  <INFO> [15:31:08] cl-user p753.lisp (main) -
;;   iter=329000, p=4700473, sum=2303944418576081288
;;  <INFO> [15:33:36] cl-user p753.lisp (main) -
;;   iter=330000, p=4716053, sum=2326111553444793620
;;  <INFO> [15:36:03] cl-user p753.lisp (main) -
;;   iter=331000, p=4731271, sum=2348423616002803844
;;  <INFO> [15:38:32] cl-user p753.lisp (main) -
;;   iter=332000, p=4746551, sum=2370879786521630528
;;  <INFO> [15:41:00] cl-user p753.lisp (main) -
;;   iter=333000, p=4762099, sum=2393484556095635186
;;  <INFO> [15:43:30] cl-user p753.lisp (main) -
;;   iter=334000, p=4777723, sum=2416235772474287894
;;  <INFO> [15:46:01] cl-user p753.lisp (main) -
;;   iter=335000, p=4793237, sum=2439135819078095666
;;  <INFO> [15:48:31] cl-user p753.lisp (main) -
;;   iter=336000, p=4808369, sum=2462184231767096258
;;  <INFO> [15:51:02] cl-user p753.lisp (main) -
;;   iter=337000, p=4823821, sum=2485380027729253262
;;  <INFO> [15:53:33] cl-user p753.lisp (main) -
;;   iter=338000, p=4838963, sum=2508725464303154468
;;  <INFO> [15:56:05] cl-user p753.lisp (main) -
;;   iter=339000, p=4854623, sum=2532218371119086882
;;  <INFO> [15:58:38] cl-user p753.lisp (main) -
;;   iter=340000, p=4869863, sum=2555861295455737928
;;  <INFO> [16:01:10] cl-user p753.lisp (main) -
;;   iter=341000, p=4885339, sum=2579652299529553742
;;  <INFO> [16:03:43] cl-user p753.lisp (main) -
;;   iter=342000, p=4900099, sum=2603591214177957194
;;  <INFO> [16:06:18] cl-user p753.lisp (main) -
;;   iter=343000, p=4915571, sum=2627677378929601034
;;  <INFO> [16:08:52] cl-user p753.lisp (main) -
;;   iter=344000, p=4930963, sum=2651917404649854218
;;  <INFO> [16:11:29] cl-user p753.lisp (main) -
;;   iter=345000, p=4946231, sum=2676308755627150940
;;  <INFO> [16:14:05] cl-user p753.lisp (main) -
;;   iter=346000, p=4961707, sum=2700851148764583350
;;  <INFO> [16:16:42] cl-user p753.lisp (main) -
;;   iter=347000, p=4976899, sum=2725543964746873400
;;  <INFO> [16:19:18] cl-user p753.lisp (main) -
;;   iter=348000, p=4991843, sum=2750388580214348480
;;  <INFO> [16:21:55] cl-user p753.lisp (main) -
;;   iter=349000, p=5007593, sum=2775386834905949906
;;  <INFO> [16:24:33] cl-user p753.lisp (main) -
;;   iter=350000, p=5023307, sum=2800541650151100980
;;  <INFO> [16:27:11] cl-user p753.lisp (main) -
;;   iter=351000, p=5038793, sum=2825851515749132498
;;  <INFO> [16:29:50] cl-user p753.lisp (main) -
;;   iter=352000, p=5054249, sum=2851318923100705322
;;  <INFO> [16:32:30] cl-user p753.lisp (main) -
;;   iter=353000, p=5069663, sum=2876941598578452992
;;  <INFO> [16:35:08] cl-user p753.lisp (main) -
;;   iter=354000, p=5084999, sum=2902721106566315432
;;  <INFO> [16:37:48] cl-user p753.lisp (main) -
;;   iter=355000, p=5100587, sum=2928656414830760534
;;  <INFO> [16:40:29] cl-user p753.lisp (main) -
;;   iter=356000, p=5116141, sum=2954751854169570176
;;  <INFO> [16:43:09] cl-user p753.lisp (main) -
;;   iter=357000, p=5131541, sum=2981005373362025006
;;  <INFO> [16:45:51] cl-user p753.lisp (main) -
;;   iter=358000, p=5146763, sum=3007416210939154436
;;  <INFO> [16:48:33] cl-user p753.lisp (main) -
;;   iter=359000, p=5162387, sum=3033986935504755128
;;  <INFO> [16:51:15] cl-user p753.lisp (main) -
;;   iter=360000, p=5178049, sum=3060718039755854876
;;  <INFO> [16:53:59] cl-user p753.lisp (main) -
;;   iter=361000, p=5193751, sum=3087611395685157656
;;  <INFO> [16:56:42] cl-user p753.lisp (main) -
;;   iter=362000, p=5208887, sum=3114665246003478032
;;  <INFO> [16:59:26] cl-user p753.lisp (main) -
;;   iter=363000, p=5224451, sum=3141878993768291150
;;  <INFO> [17:02:10] cl-user p753.lisp (main) -
;;   iter=364000, p=5239777, sum=3169253966325724346
;;  <INFO> [17:04:55] cl-user p753.lisp (main) -
;;   iter=365000, p=5254859, sum=3196788420568030892
;;  <INFO> [17:07:40] cl-user p753.lisp (main) -
;;   iter=366000, p=5270521, sum=3224485073544976424
;;  <INFO> [17:10:27] cl-user p753.lisp (main) -
;;   iter=367000, p=5286331, sum=3252345232636889618
;;  <INFO> [17:13:13] cl-user p753.lisp (main) -
;;   iter=368000, p=5301481, sum=3280369716982072970
;;  <INFO> [17:16:00] cl-user p753.lisp (main) -
;;   iter=369000, p=5316973, sum=3308556216073228808
;;  <INFO> [17:18:47] cl-user p753.lisp (main) -
;;   iter=370000, p=5332519, sum=3336910595900506532
;;  <INFO> [17:21:35] cl-user p753.lisp (main) -
;;   iter=371000, p=5347789, sum=3365428458525044294
;;  <INFO> [17:24:24] cl-user p753.lisp (main) -
;;   iter=372000, p=5363461, sum=3394111687706645594
;;  <INFO> [17:27:13] cl-user p753.lisp (main) -
;;   iter=373000, p=5378921, sum=3422963337752165312
;;  <INFO> [17:30:02] cl-user p753.lisp (main) -
;;   iter=374000, p=5394401, sum=3451977960307732094
;;  <INFO> [17:32:53] cl-user p753.lisp (main) -
;;   iter=375000, p=5410121, sum=3481162555187096720
;;  <INFO> [17:35:43] cl-user p753.lisp (main) -
;;   iter=376000, p=5425517, sum=3510515315860576436
;;  <INFO> [17:38:34] cl-user p753.lisp (main) -
;;   iter=377000, p=5440889, sum=3540035570609690438
;;  <INFO> [17:41:25] cl-user p753.lisp (main) -
;;   iter=378000, p=5456533, sum=3569724434989859600
;;  <INFO> [17:44:17] cl-user p753.lisp (main) -
;;   iter=379000, p=5472031, sum=3599581010167294214
;;  <INFO> [17:47:10] cl-user p753.lisp (main) -
;;   iter=380000, p=5487701, sum=3629609814583565150
;;  <INFO> [17:50:04] cl-user p753.lisp (main) -
;;   iter=381000, p=5503133, sum=3659810856617412668
;;  <INFO> [17:52:58] cl-user p753.lisp (main) -
;;   iter=382000, p=5518847, sum=3690181865805180242
;;  <INFO> [17:55:52] cl-user p753.lisp (main) -
;;   iter=383000, p=5534677, sum=3720728333730601634
;;  <INFO> [17:58:47] cl-user p753.lisp (main) -
;;   iter=384000, p=5550287, sum=3751446615891725882
;;  <INFO> [18:01:42] cl-user p753.lisp (main) -
;;   iter=385000, p=5566073, sum=3782339980100083460
;;  <INFO> [18:04:38] cl-user p753.lisp (main) -
;;   iter=386000, p=5581397, sum=3813404634760089584
;;  <INFO> [18:07:34] cl-user p753.lisp (main) -
;;   iter=387000, p=5596937, sum=3844644009544336010
;;  <INFO> [18:10:31] cl-user p753.lisp (main) -
;;   iter=388000, p=5612441, sum=3876057232643050694
;;  <INFO> [18:13:28] cl-user p753.lisp (main) -
;;   iter=389000, p=5628257, sum=3907646782432504964
;;  <INFO> [18:16:26] cl-user p753.lisp (main) -
;;   iter=390000, p=5644031, sum=3939411505972266656
;;  <INFO> [18:19:24] cl-user p753.lisp (main) -
;;   iter=391000, p=5659751, sum=3971354970316006670
;;  <INFO> [18:22:23] cl-user p753.lisp (main) -
;;   iter=392000, p=5675381, sum=4003476134981462180
;;  <INFO> [18:25:23] cl-user p753.lisp (main) -
;;   iter=393000, p=5690941, sum=4035775616724206576
;;  <INFO> [18:28:23] cl-user p753.lisp (main) -
;;   iter=394000, p=5706079, sum=4068247291668103922
;;  <INFO> [18:31:24] cl-user p753.lisp (main) -
;;   iter=395000, p=5721481, sum=4100896025189882324
;;  <INFO> [18:34:25] cl-user p753.lisp (main) -
;;   iter=396000, p=5737393, sum=4133721188112188690
;;  <INFO> [18:37:27] cl-user p753.lisp (main) -
;;   iter=397000, p=5752739, sum=4166725799511671684
;;  <INFO> [18:40:28] cl-user p753.lisp (main) -
;;   iter=398000, p=5768663, sum=4199910766979486618
;;  <INFO> [18:43:31] cl-user p753.lisp (main) -
;;   iter=399000, p=5784439, sum=4233278665789680608
;;  <INFO> [18:46:34] cl-user p753.lisp (main) -
;;   iter=400000, p=5800079, sum=4266829703947222256
;;  <INFO> [18:49:38] cl-user p753.lisp (main) -
;;   iter=401000, p=5815519, sum=4300560489265150424
;;  <INFO> [18:52:44] cl-user p753.lisp (main) -
;;   iter=402000, p=5831387, sum=4334472848585045906
;;  <INFO> [18:55:48] cl-user p753.lisp (main) -
;;   iter=403000, p=5846957, sum=4368570025704660782
;;  <INFO> [18:58:55] cl-user p753.lisp (main) -
;;   iter=404000, p=5862617, sum=4402846617143022524
;;  <INFO> [19:02:00] cl-user p753.lisp (main) -
;;   iter=405000, p=5878123, sum=4437308880700211492
;;  <INFO> [19:05:07] cl-user p753.lisp (main) -
;;   iter=406000, p=5893357, sum=4471950395520571094
;;  <INFO> [19:08:14] cl-user p753.lisp (main) -
;;   iter=407000, p=5908739, sum=4506772668705329630
;;  <INFO> [19:11:22] cl-user p753.lisp (main) -
;;   iter=408000, p=5923901, sum=4541775630965657042
;;  <INFO> [19:14:30] cl-user p753.lisp (main) -
;;   iter=409000, p=5939371, sum=4576959544921559768
;;  <INFO> [19:17:39] cl-user p753.lisp (main) -
;;   iter=410000, p=5955031, sum=4612328623060945706
;;  <INFO> [19:20:48] cl-user p753.lisp (main) -
;;   iter=411000, p=5970803, sum=4647884757012308654
;;  <INFO> [19:23:58] cl-user p753.lisp (main) -
;;   iter=412000, p=5986613, sum=4683631504486698356
;; 4714126766770661630
(defun main ()
  (let ((sum 0)
        (iter 0))
    (log:info "started")
    (loop for p from 2 below 6000000
          when (prime-p p)
            do (progn
                 (incf sum (F-3 p *left-count-arr* *right-count-arr*))
                 (incf iter)
                 (when (= (mod iter 1000) 0)
                   (log:info "iter=~A, p=~A, sum=~A" iter p sum))))
    sum))

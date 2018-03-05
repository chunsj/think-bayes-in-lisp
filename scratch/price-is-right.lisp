(in-package :think-bayes)

(defun read-showcase-data (fname)
  (let ((lines (read-lines-from fname)))
    #{:showcase1 (mapcar #'parse-integer (cdr (split #\, ($3 lines))))
      :showcase2 (mapcar #'parse-integer (cdr (split #\, ($4 lines))))
      :bid1 (mapcar #'parse-integer (cdr (split #\, ($6 lines))))
      :bid2 (mapcar #'parse-integer (cdr (split #\, ($7 lines))))
      :difference1 (mapcar #'parse-integer (cdr (split #\, ($9 lines))))
      :difference2 (mapcar #'parse-integer (cdr (split #\, ($ lines 10))))}))

(defun read-showcase-data2 (fname)
  (let ((lines (read-lines-from fname)))
    #{:showcase1 (mapcar #'parse-integer (cdr (split #\, ($2 lines))))
      :showcase2 (mapcar #'parse-integer (cdr (split #\, ($3 lines))))
      :bid1 (mapcar #'parse-integer (cdr (split #\, ($5 lines))))
      :bid2 (mapcar #'parse-integer (cdr (split #\, ($6 lines))))
      :difference1 (mapcar #'parse-integer (cdr (split #\, ($8 lines))))
      :difference2 (mapcar #'parse-integer (cdr (split #\, ($9 lines))))}))

(defparameter *showcase2011* (read-showcase-data
                              "/Users/Sungjin/Documents/Python/ThinkBayes/code/showcases.2011.csv"))
(defparameter *showcase2012* (read-showcase-data2
                              "/Users/Sungjin/Documents/Python/ThinkBayes/code/showcases.2012.csv"))

(-> (to-pmf (empirical ($ *showcase2011* :showcase1))
            :xs (linspace 0 75000 101))
    (plot :xtics 10))

(-> (to-pmf (empirical ($ *showcase2011* :showcase2))
            :xs (linspace 0 75000 101))
    (plot :xtics 10))

(-> (to-pmf (empirical ($ *showcase2011* :difference1))
            :xs (linspace -50000 50000 101))
    (to-cdf)
    (plot :xtics 10))

(-> (to-pmf (empirical ($ *showcase2011* :difference2))
            :xs (linspace -50000 50000 101))
    (to-cdf)
    (plot :xtics 10))

(defclass player ()
  ((pdf-price :initform nil :accessor pdf-price)
   (cdf-diff :initform nil :accessor cdf-diff)
   (pdf-error :initform nil :accessor pdf-error)
   (n :initform 101 :accessor player-steps)
   (price-xs :initform (linspace 0 75000 101) :accessor price-xs)
   (diff-xs :initform (linspace -50000 50000 101) :accessor diff-xs)
   (prior :initform nil :accessor player-prior)
   (posterior :initform nil :accessor player-posterior)))

(defun player (prices diffs)
  (let ((instance (make-instance 'player))
        (price-emp (empirical prices))
        (diff-emp (empirical diffs)))
    (setf (pdf-price instance) price-emp)
    (setf (cdf-diff instance) (-> diff-emp
                                  (to-pmf :xs (diff-xs instance))
                                  (to-cdf)))
    (setf (pdf-error instance) (gaussian :sigma (sqrt (xvariance diff-emp))))
    instance))

(defgeneric error-density (player error))
(defmethod error-density ((player player) error) (p (pdf-error player) error))

(defclass price (pmf)
  ((player :initform nil :accessor price-player)))

(defun price (pmf player)
  (let ((price (pmf :class 'price)))
    (setf (xpmap price) (xpmap pmf))
    (setf (price-player price) player)
    price))

(defmethod likelihood ((price price) evidence hypo)
  (let ((prc hypo)
        (guess evidence))
    (error-density (price-player price) (- prc guess))))

(defgeneric make-beliefs (player guess))
(defmethod make-beliefs ((player player) guess)
  (setf (player-prior player) (price (to-pmf (pdf-price player) :xs (price-xs player)) player))
  (setf (player-posterior player) (price (to-pmf (pdf-price player) :xs (price-xs player)) player))
  (update (player-posterior player) guess))

(let ((player (player ($ *showcase2011* :showcase1) ($ *showcase2011* :difference1))))
  (make-beliefs player 20000)
  (plot (player-posterior player) :xtics 10))

(defclass gain-calculator ()
  ((player :initform nil :accessor gain-player)
   (opponent :initform nil :accessor gain-opponent)))

(defun gain-calculator (player opponent)
  (let ((instance (make-instance 'gain-calculator)))
    (setf (gain-player instance) player
          (gain-opponent instance) opponent)
    instance))

(defgeneric expected-gain (calculator bid))
(defgeneric expected-gains (calculator &key low high n))
(defgeneric gain (calculator bid price))
(defgeneric prob-win (calculator diff))

(defmethod expected-gains ((calc gain-calculator) &key (low 0) (high 75000) (n 101))
  (let ((bids (linspace low high n)))
    (mapcar (lambda (bid) (cons bid (expected-gain calc bid))) bids)))

(defmethod expected-gain ((calc gain-calculator) bid)
  (let ((suite (player-posterior (gain-player calc)))
        (total 0D0))
    (loop :for xp :in (xps suite)
          :for price = (car xp)
          :for prob = (cdr xp)
          :for gain = (gain calc bid price)
          :do (incf total (* prob gain)))
    total))

(defmethod gain ((calc gain-calculator) bid price)
  (cond ((> bid price) 0)
        (t (let* ((diff (- price bid))
                  (prob (prob-win calc diff)))
             (if (<= diff 250)
                 (* 2D0 price prob)
                 (* price prob))))))

(defgeneric prob-over-bid (player))
(defgeneric prob-worse-than (player diff))

(defmethod prob-win ((calc gain-calculator) diff)
  (+ (prob-over-bid (gain-opponent calc))
     (prob-worse-than (gain-opponent calc) diff)))

(defmethod prob-over-bid ((player player)) (p (cdf-diff player) -1))
(defmethod prob-worse-than ((player player) diff) (- 1D0 (p (cdf-diff player) diff)))

(defgeneric optimal-bid (player guess opponent))

(defmethod optimal-bid ((player player) guess opponent)
  (make-beliefs player guess)
  (let* ((calc (gain-calculator player opponent))
         (bids-gains (expected-gains calc))
         (bid-gain (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) bids-gains)))
    bid-gain))

(let ((player (player ($ *showcase2011* :showcase1) ($ *showcase2011* :difference1)))
      (opponent (player ($ *showcase2011* :showcase2) ($ *showcase2011* :difference2))))
  (optimal-bid player 20000 opponent))

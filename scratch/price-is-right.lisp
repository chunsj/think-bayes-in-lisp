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
(defparameter *showcase* #{:showcase1 (append ($ *showcase2011* :showcase1)
                                       ($ *showcase2012* :showcase1))
                           :showcase2 (append ($ *showcase2011* :showcase2)
                                       ($ *showcase2012* :showcase2))
                           :bid1 (append ($ *showcase2011* :bid1)
                                  ($ *showcase2012* :bid1))
                           :bid2 (append ($ *showcase2011* :bid2)
                                  ($ *showcase2012* :bid2))
                           :difference1 (append ($ *showcase2011* :difference1)
                                         ($ *showcase2012* :difference1))
                           :difference2 (append ($ *showcase2011* :difference2)
                                         ($ *showcase2012* :difference2))})

(defparameter *max-price* (max (apply #'max ($ *showcase* :showcase1))
                               (apply #'max ($ *showcase* :showcase2))))

(defparameter *maxprc* 75000)
(defparameter *prices* (linspace 0 *maxprc* 101))
(defparameter *diffs* (linspace (* -1 *maxprc*) *maxprc* 101))

;; actual prices distribution for player1
(-> (to-pmf (empirical ($ *showcase* :showcase1))
            :xs *prices*)
    (plot :xtics 10))

;; actual prices distribution for player2
(-> (to-pmf (empirical ($ *showcase* :showcase2))
            :xs *prices*)
    (plot :xtics 10))

;; bids distribution for player1
(-> (to-pmf (empirical ($ *showcase* :bid1))
            :xs *prices*)
    (plot :xtics 10))

;; bids distribution for player2
(-> (to-pmf (empirical ($ *showcase* :bid2))
            :xs *prices*)
    (plot :xtics 10))

;; actual price - bid
(-> (to-pmf (empirical ($ *showcase* :difference1))
            :xs *diffs*)
    (plot :xtics 10))

(-> (to-pmf (empirical ($ *showcase* :difference2))
            :xs *diffs*)
    (plot :xtics 10))

;; what to solve => if the actual price p is given what is the likelihood that the contestant's
;; estimate would be guess g?
;;
;; error: price - guess
;; diff = price - bid
;;
;; what is the likelihood tha the contestant's estimate is off by error e?

;; first contestant overbids ~25%
(let ((pmf (to-pmf (empirical ($ *showcase* :difference1))
                   :xs *diffs*)))
  (loop :for x :in *diffs* :when (< x 0) :sum (p pmf x)))

;; second contestant overbids ~28%
(let ((pmf (to-pmf (empirical ($ *showcase* :difference2))
                   :xs *diffs*)))
  (loop :for x :in *diffs* :when (< x 0) :sum (p pmf x)))

;; mean estimations
(xmean (to-pmf (empirical ($ *showcase* :bid1)) :xs *prices*))
(xmean (to-pmf (empirical ($ *showcase* :bid2)) :xs *prices*))

;; assumption
;; the distribution of error is gaussian with mean 0 and the same variance as diff

(let ((cdf-diff (to-cdf (to-pmf (empirical ($ *showcase* :difference1)) :xs *diffs*))))
  (plot cdf-diff :xtics 10))

(let ((cdf-diff (to-cdf (to-pmf (empirical ($ *showcase* :difference2)) :xs *diffs*))))
  (plot cdf-diff :xtics 10))

;; assumed error distribution
(let ((pdf-error (gaussian :sigma (sd ($ *showcase* :difference1)))))
  (plot pdf-error))

(let ((pdf-error (gaussian :sigma (sd ($ *showcase* :difference2)))))
  (plot pdf-error))

(defclass case-price (pmf)
  ((cdf-diff :initform nil :accessor cdf-diff)
   (pdf-error :initform nil :accessor pdf-error)))

(defun case-price (showcases differences)
  (let ((instance (make-instance 'case-price))
        (pdf-price (empirical showcases)))
    (setf (cdf-diff instance) (to-cdf (to-pmf (empirical differences) :xs *diffs*)))
    (let ((pmf (to-pmf pdf-price :xs *prices*)))
      (setf (xpmap instance) (xpmap pmf))
      (setf (pdf-error instance) (gaussian :sigma (sd differences))))
    instance))

(defun showcase1-price () (case-price ($ *showcase* :showcase1) ($ *showcase* :difference1)))
(defun showcase2-price () (case-price ($ *showcase* :showcase2) ($ *showcase* :difference2)))

(xmean (showcase1-price))
(xmean (showcase2-price))

(defmethod likelihood ((pmf case-price) evidence hypothesis)
  (let ((price hypothesis)
        (guess evidence))
    (p (pdf-error pmf) (- price guess))))

;; price distribution changes according to error, case 1
(plot (showcase1-price) :xtics 10)
(let ((pmf (showcase1-price)))
  (observe pmf 20000)
  (plot pmf :xtics 10))

;; case 2
(plot (showcase2-price) :xtics 10)
(let ((pmf (showcase2-price)))
  (observe pmf 20000)
  (plot pmf :xtics 10))

(defclass player () ((pmf-price :initform nil :accessor pmf-price)))

(defun player (showcase-number)
  (let ((instance (make-instance 'player)))
    (cond ((eq showcase-number 1) (setf (pmf-price instance) (showcase1-price)))
          ((eq showcase-number 2) (setf (pmf-price instance) (showcase2-price))))
    instance))

(defmethod pdf-error ((player player)) (pdf-error (pmf-price player)))
(defmethod cdf-diff ((player player)) (cdf-diff (pmf-price player)))

(defgeneric make-beliefs (player guess))
(defmethod make-beliefs ((player player) guess) (observe (pmf-price player) guess))

(defgeneric overbid-probability (player))
(defgeneric worse-probability (player diff))

(defmethod overbid-probability ((player player)) (p (cdf-diff player) -1))
(defmethod worse-probability ((player player) diff) (- 1D0 (p (cdf-diff player) diff)))

;; original price distribution
(let ((player1 (player 1))) (plot (pmf-price player1) :xtics 10))

(let ((player1 (player 1))) (maximum-likelihood (pmf-price player1)))
;; after guess, the distribution is left shifted (20000 < 27750)
(let ((player1 (player 1)))
  (make-beliefs player1 20000)
  (plot (pmf-price player1) :xtics 10))
;; if you think/guess the price is 20000, then you should believe it it 24000
(let ((player1 (player 1)))
  (make-beliefs player1 20000)
  (maximum-likelihood (pmf-price player1)))

(defclass gain-calc ()
  ((player :initform nil :accessor gain-player)
   (opponent :initform nil :accessor gain-opponent)))

(defun gain-calc (p o)
  (let ((instance (make-instance 'gain-calc)))
    (setf (gain-player instance) p
          (gain-opponent instance) o)
    instance))

(defmethod pmf-price ((calc gain-calc)) (pmf-price (gain-player calc)))

($first *prices*)
($last *prices*)

(defgeneric win-probability (calc diff))
(defmethod win-probability ((calc gain-calc) diff)
  (+ (overbid-probability (gain-opponent calc))
     (worse-probability (gain-opponent calc) diff)))

(defgeneric gain (calc bid price))
(defmethod gain ((calc gain-calc) bid price)
  (cond ((> bid price) 0D0)
        (t (let* ((diff (- price bid))
                  (prob (win-probability calc diff)))
             (if (<= diff 250D0)
                 (* 2D0 price prob)
                 (* price prob))))))

(defgeneric expected-gain (calc bid))
(defmethod expected-gain ((calc gain-calc) bid)
  (let ((suite (pmf-price calc)))
    (->> (mapcar (lambda (xp)
                   (let ((price (car xp))
                         (prob (cdr xp)))
                     (* prob (gain calc bid price))))
                 (xps suite))
         (reduce #'+))))

(defgeneric expected-gains (calc &key low high n))
(defmethod expected-gains ((calc gain-calc) &key (low ($first *prices*))
                                              (high ($last *prices*))
                                              (n 101))
  ;; returns a list of (bid . gain)
  (mapcar (lambda (bid) (cons bid (expected-gain calc bid))) (linspace low high n)))

(defgeneric optimal-bid (player guess opponent))
(defmethod optimal-bid ((player player) guess opponent)
  (make-beliefs player guess)
  (let ((calc (gain-calc player opponent)))
    (->> (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) (expected-gains calc))
         (car))))

;; prior mles
(let ((p1 (player 1))
      (p2 (player 2)))
  #{:prior-mle1 (maximum-likelihood (pmf-price p1))
    :mean1 (xmean (pmf-price p1))
    :prior-mle2 (maximum-likelihood (pmf-price p2))
    :mean2 (xmean (pmf-price p2))})

;; after guessing
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  #{:prior-mle1 (maximum-likelihood (pmf-price p1))
    :mean1 (xmean (pmf-price p1))
    :prior-mle2 (maximum-likelihood (pmf-price p2))
    :mean2 (xmean (pmf-price p2))})

;; expected gains for player 1
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  (let* ((c1 (gain-calc p1 p2))
         (gains (expected-gains c1)))
    (plot-boxes gains :xtics 10)))

;; optimal bid and expected gain for player 1
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  (let* ((c1 (gain-calc p1 p2))
         (gains (expected-gains c1)))
    (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) gains)))

;; for player 2
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  (let* ((c2 (gain-calc p2 p1))
         (gains (expected-gains c2)))
    (plot-boxes gains :xtics 10)))

;; for player 2
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  (let* ((c2 (gain-calc p2 p1))
         (gains (expected-gains c2)))
    (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) gains)))

(in-package :think-bayes)

;; the game scenario suggests several questions
;;
;; 1. before seeing the prize, what prior beliefs should the contestant have about the price of
;;    the showcase?
;; 2. after seeing the prizes, how should the contestant update those beliefs?
;; 3. based on the posterior distribution, what should the contestant bid?
;;
;; the third question demonstrates a common use of bayesian analysis: decision analysis.
;; given a posterior distribution, we can choose the bid that maximizeds the contestant's
;; expected return.
;;
;; this problem is inspired by an example in 'bayesian methods for hackers'. i should have try
;; this one as well.

;; implementations are slightly different from the book, trying to be simpler ones

;; preparation of previous shows data
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

(defun datafn (filename)
  (concatenate 'string "/Users/Sungjin/Documents/MLStudy/BAP/ThinkBayes/code/"
               filename))

(defparameter *showcase2011* (read-showcase-data (datafn "showcases.2011.csv")))
(defparameter *showcase2012* (read-showcase-data2 (datafn "showcases.2012.csv")))
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

;; distribution of prices for showcases
(defparameter *showcase1-prices-pmf* (-> (empirical ($ *showcase* :showcase1))
                                         (to-pmf :xs *prices*)))
(defparameter *showcase2-prices-pmf* (-> (empirical ($ *showcase* :showcase2))
                                         (to-pmf :xs *prices*)))

;; some statistics of price distributions
(xmean *showcase1-prices-pmf*)
(xmean *showcase2-prices-pmf*)
(maximum-likelihood *showcase1-prices-pmf*)
(maximum-likelihood *showcase2-prices-pmf*)

;; shape of distributions
(view *showcase1-prices-pmf* :xtics 10)
(view *showcase2-prices-pmf* :xtics 10)

;; modeling the contestants
;;
;; 1. what data should we consider and how should we quantify it?
;; 2. can we compute a likelihood function; that is, for each hypothetical value of
;;    price, can we compute the conditional likelihood of the data?
;;
;; contestant: a price-guessing instrument with known error characteristics.
;;             when the contestant sees the prizes, he or she guesses the price of each prize.
;;
;; question: if the actual price is p, what is the likelihood that the contestant's estimate
;;           would be guess g?
;;
;; or: define error = price - guess
;;     what is the likelihood that the contestant's estimate is off by error?

;; from data we know difference between price and bid
;; difference = price - bid
(defparameter *difference1-pmf* (-> (empirical ($ *showcase* :difference1))
                                    (to-pmf :xs *diffs*)))
(defparameter *difference2-pmf* (-> (empirical ($ *showcase* :difference2))
                                    (to-pmf :xs *diffs*)))

;; by overbidding probability we can deduce that bids are biased, (1 - overbid probability) > 50%
;; overbidding probability of contestant 1 ~ 23%
(p (to-cdf *difference1-pmf*) -1D0)
;; for contestant 2 ~ 26%
(p (to-cdf *difference2-pmf*) -1D0)

;; finally, we can use this distribution to estimate the reliability of the contestans' guesses.
;; because we don't actually know the contestant's guesses (we only know what they bid), this is
;; a little tricky.
;; so, we'll have to make some assumptions. specifically, the distribution of error is gaussian
;; with mean 0 and the same variance as difference.

;; we can model the price guess.
;; we can use the variance of difference to estimate the variance of error.
;; guesstimation has price(self), difference(cdf-diff), and error(pdf-error) distributions
(defclass guesstimation (pmf)
  ((cdf-diff :initform nil :accessor cdf-diff)
   (pdf-error :initform nil :accessor pdf-error)))

(defun guesstimation (showcases differences)
  (let ((instance (make-instance 'guesstimation))
        (pdf-price (empirical showcases)))
    (setf (cdf-diff instance) (to-cdf (to-pmf (empirical differences) :xs *diffs*)))
    (let ((pmf (to-pmf pdf-price :xs *prices*)))
      (setf (xpmap instance) (xpmap pmf))
      (setf (pdf-error instance) (gaussian :sigma (sd differences))))
    instance))

(defun showcase1-guess () (guesstimation ($ *showcase* :showcase1) ($ *showcase* :difference1)))
(defun showcase2-guess () (guesstimation ($ *showcase* :showcase2) ($ *showcase* :difference2)))

;; we can update our guesstimation on price of showcase using contestant's best guess
;; price: hypothesis
;; evidence: guess of contentant
(defmethod likelihood ((pmf guesstimation) evidence hypothesis)
  (let ((price hypothesis)
        (guess evidence))
    ;; note that error = price - guess, we can compute the probability density of error
    (p (pdf-error pmf) (- price guess))))

;; model of a contestant which has a price distribution (of guesstimation)
(defclass player () ((pmf-price :initform nil :accessor pmf-price)))

(defun player (showcase-number)
  (let ((instance (make-instance 'player)))
    (cond ((eq showcase-number 1) (setf (pmf-price instance) (showcase1-guess)))
          ((eq showcase-number 2) (setf (pmf-price instance) (showcase2-guess))))
    instance))

(defmethod pdf-error ((player player)) (pdf-error (pmf-price player)))
(defmethod cdf-diff ((player player)) (cdf-diff (pmf-price player)))

;; update guesstimation using guess to make posterior distribution
(defun make-beliefs (player guess) (observe (pmf-price player) guess))

(defun overbid-probability (player) (p (cdf-diff player) -1))
(defun worse-probability (player diff) (- 1D0 (p (cdf-diff player) diff)))

;; original price distribution
(let ((player1 (player 1))) (view (pmf-price player1) :xtics 10))
(let ((player1 (player 1))) (maximum-likelihood (pmf-price player1)))

;; after guess, the distribution is left shifted (20000 < 27750)
(let ((player1 (player 1)))
  (make-beliefs player1 20000)
  (view (pmf-price player1) :xtics 10))
;; if you think/guess the price is 20000, then you should believe it it 24000
;; why? guess is from the prizes you see and price is from past historical data.
;; so what you've guessed updates the prior or historical price distribution.
;; that is, we are treating the historical data as the prior and updating it based on your guess.
(let ((player1 (player 1)))
  (make-beliefs player1 20000)
  (maximum-likelihood (pmf-price player1)))

;; for optimal bid
(defclass gain-calc ()
  ((player :initform nil :accessor gain-player)
   (opponent :initform nil :accessor gain-opponent)))

(defun gain-calc (p o)
  (let ((instance (make-instance 'gain-calc)))
    (setf (gain-player instance) p
          (gain-opponent instance) o)
    instance))

(defmethod pmf-price ((calc gain-calc)) (pmf-price (gain-player calc)))

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
  #{:mle1 (maximum-likelihood (pmf-price p1))
    :mean1 (xmean (pmf-price p1))
    :mle2 (maximum-likelihood (pmf-price p2))
    :mean2 (xmean (pmf-price p2))})

;; after guessing
(let ((p1 (player 1))
      (p2 (player 2))
      (g1 20000)
      (g2 40000))
  (make-beliefs p1 g1)
  (make-beliefs p2 g2)
  #{:mle1 (maximum-likelihood (pmf-price p1))
    :mean1 (xmean (pmf-price p1))
    :mle2 (maximum-likelihood (pmf-price p2))
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
    (mplot:plot-boxes gains :xtics 10)))

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
    (mplot:plot-boxes gains :xtics 10)))

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

;; optimal bid and expected gains
(-> (loop :for bid :in (linspace 15000 50000 36)
          :for p1 = (player 1)
          :for p2 = (player 2)
          :do (make-beliefs p1 bid)
          :do (make-beliefs p2 40000)
          :collect (let ((c (gain-calc p1 p2)))
                     (reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) (expected-gains c))))
    (mplot:plot-boxes :xtics 10))

(in-package :think-bayes)

;;
;; THE DICE PROBLEM
;;
;; a box of dice that contains 4, 6, 8, 12, and 20 sided die
;; suppose a die selected from the box at random, rolled and 6 is get,
;; what is the probability that each die rolled is?
;;
;; problem solving steps
;;
;; 1. choose a representation for the hypotheses
;; 2. choose a representation for the data
;; 3. write the likelihood function

;; the problem
(defclass dice (pmf) ())

(defmethod likelihood ((self dice) data hypothesis)
  (let ((side data))
    (cond ((< hypothesis side) 0D0)
          (t (/ 1D0 hypothesis)))))

(defparameter *dice-suite* (pmf :class 'dice :hypotheses '(4 6 8 12 20)))

;; prior probability distribution
(view *dice-suite*)

;; if 6 is rolled
(observe *dice-suite* 6)

;; posterior distribution
(view *dice-suite*)

;; rolled more - 6 8 7 7 5 4
(observe *dice-suite* '(6 8 7 7 5 4) :multiplep t)

;; updated posterior distribution
(view *dice-suite*) ;; with 94%, we're certain that the selected die is the 8-sided one

;;
;; THE LOCOMOTIVE PROBLEM
;;
;; a railroad numbers its locomotives in order 1 to N. one day you see a locomotive with
;; the number 60. estimate how many locomotives the railroad has.
;;
;; problem solving steps
;;
;; 1. what did we know about n before we saw the data?
;; 2. for any given value n, what is the likelihood of seeing the data, a locomontive with number 60?
;;
;; the answer for step 1 is prior and for step 2 is likelihood.

;; problem
(defclass train (pmf) ())

(defmethod likelihood ((self train) data hypothesis)
  (let ((number data))
    (cond ((< hypothesis number) 0D0)
          (t (/ 1D0 hypothesis)))))

(defparameter *train-hypotheses* (xrange 1 1001))
(defparameter *train-suite* (pmf :class 'train :hypotheses *train-hypotheses*))

;; prior distribution
(view *train-suite*)

;; update with observed data
(observe *train-suite* 60)

;; posterior distribution
(view *train-suite*)

;; to maximize the change of getting the answer correctly
(maximum-likelihood *train-suite*)

;; mean of posterior to minimize error, which will minimize the mean squared error over the long run
(xmean *train-suite*)

;; multiple different prior makes different result
(let ((s1 (pmf :class 'train :hypotheses (xrange 1 501)))
      (s2 (pmf :class 'train :hypotheses (xrange 1 1001)))
      (s3 (pmf :class 'train :hypotheses (xrange 1 2001))))
  (loop :for s :in (list s1 s2 s3)
        :do (observe s 60)
        :collect (xmean s)))

;; but with more observations, posteriors will converge
(let ((s1 (pmf :class 'train :hypotheses (xrange 1 501)))
      (s2 (pmf :class 'train :hypotheses (xrange 1 1001)))
      (s3 (pmf :class 'train :hypotheses (xrange 1 2001))))
  (loop :for s :in (list s1 s2 s3)
        :do (observe s '(60 30 90) :multiplep t)
        :collect (xmean s)))

;; if we cannot get more data, then we can improve our prior, use power law
(defclass train2 (train) ((alpha :initform 1D0 :reader train2-alpha)))

(defmethod init ((self train2) &key hypotheses &allow-other-keys)
  (loop :for hypo :in hypotheses :do (assign self hypo (expt hypo (- (train2-alpha self)))))
  (normalize self))

(defparameter *train2-suite* (pmf :class 'train2 :hypotheses *train-hypotheses*))

;; prior distribution
(view *train2-suite*)

;; update with observed data
(observe *train2-suite* 60)

;; posterior distribution
(view *train2-suite*)

;; multiple different prior makes different result
(let ((s1 (pmf :class 'train2 :hypotheses (xrange 1 501)))
      (s2 (pmf :class 'train2 :hypotheses (xrange 1 1001)))
      (s3 (pmf :class 'train2 :hypotheses (xrange 1 2001))))
  (loop :for s :in (list s1 s2 s3)
        :do (observe s 60)
        :collect (xmean s)))

;; but with more observations, posteriors will converge
(let ((s1 (pmf :class 'train2 :hypotheses (xrange 1 501)))
      (s2 (pmf :class 'train2 :hypotheses (xrange 1 1001)))
      (s3 (pmf :class 'train2 :hypotheses (xrange 1 2001))))
  (loop :for s :in (list s1 s2 s3)
        :do (observe s '(60 30 90) :multiplep t)
        :collect (xmean s)))

;; percentile
(identity #{:%5 (percentile *train2-suite* 5) :%95 (percentile *train2-suite* 95)})

;; credible interval - inefficient
(credible-interval *train2-suite* 90)

;; credible interval - efficient
(credible-interval (to-cdf *train2-suite*) 90)

;;
;; THE EURO PROBLEM
;;
;; when flipped 250 times, a coin cam up heads 140 times and tails 110. is this coin biased?
;;
;; problem solving steps
;;
;; 1. to estimate the probability that the coin lands face up
;; 2. to evaluate whether the data support the hypothesis that the coin is biased

;; define hypotheses, where a hypothesis x is the probability of heads is x%.
;; start with uniform prior.
(defclass euro (pmf) ())

(defmethod likelihood ((self euro) data hypothesis)
  (let ((faceprob (/ hypothesis 100D0))
        (face data))
    (cond ((eq face :h) faceprob)
          (t (- 1D0 faceprob)))))

(defparameter *euro-suite* (pmf :class 'euro :hypotheses (xrange 0 101)))
(defparameter *euro-dataset* (append (repeat 140 :h) (repeat 110 :t)))

;; prior distribution
(view *euro-suite*)

;; update observations
(observe *euro-suite* *euro-dataset* :multiplep t)

;; posterior distribution
(view *euro-suite*)

;; most likely value from posterior
(maximum-likelihood *euro-suite*)

;; mean
(xmean *euro-suite*)

;; median
(percentile *euro-suite* 50)

;; credible interval - unfair?
(credible-interval (to-cdf *euro-suite*) 90)

;; better prior
(defun triangular-prior ()
  (let ((suite (pmf :class 'euro)))
    (loop :for x :in (xrange 0 51) :do (assign suite x (coerce x 'double-float)))
    (loop :for x :in (xrange 51 101) :do (assign suite x (- 100D0 x)))
    (normalize suite)
    suite))

(defparameter *euro2-suite* (triangular-prior))

;; prior distribution
(view *euro2-suite*)

;; update with observations
(observe *euro2-suite* *euro-dataset* :multiplep t)

;; posterior distribution - swamping the priors, with enough data, the result will converge
(view *euro2-suite*)

;; most likely value from posterior
(maximum-likelihood *euro2-suite*)

;; mean
(xmean *euro2-suite*)

;; median
(percentile *euro2-suite* 50)

;; credible interval - unfair?
(credible-interval (to-cdf *euro2-suite*) 90)

;; more efficient likelihood
(defmethod likelihood ((self euro) data hypothesis)
  (let ((faceprob (/ hypothesis 100D0))
        (heads (car data))
        (tails (cdr data)))
    (* (expt faceprob heads) (expt (- 1D0 faceprob) tails))))

(defparameter *euro2-suite* (triangular-prior))

;; prior distribution
(view *euro2-suite*)

;; update with observations
(observe *euro2-suite* (cons 140 110))

;; posterior distribution - swamping the priors, with enough data, the result will converge
(view *euro2-suite*)

;; with beta distribution
(defparameter *beta* (beta))
(view *beta*)
(observe *beta* (cons 140 110))
(view *beta*)
(xmean *beta*)

(in-package :think-bayes)

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
(plot *euro-suite*)

;; update observations
(observe *euro-suite* *euro-dataset* :multiplep t)

;; posterior distribution
(plot *euro-suite*)

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
(plot *euro2-suite*)

;; update with observations
(observe *euro2-suite* *euro-dataset* :multiplep t)

;; posterior distribution - swamping the priors, with enough data, the result will converge
(plot *euro2-suite*)

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
(plot *euro2-suite*)

;; update with observations
(observe *euro2-suite* (cons 140 110))

;; posterior distribution - swamping the priors, with enough data, the result will converge
(plot *euro2-suite*)

;; with beta distribution
(defparameter *beta* (beta))
(plot *beta*)
(observe *beta* (cons 140 110))
(plot *beta*)
(xmean *beta*)

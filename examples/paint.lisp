(in-package :think-bayes)

;; MODELING
;;
;; the room is 30 feet wide and 50 feet long

(defclass paintball (pmf)
  ((locations :initform nil :accessor locations)))

(defun paintball (alphas betas locations)
  (let ((self (make-instance 'paintball)))
    (setf (locations self) locations)
    (loop :for alpha :in alphas
          :do (loop :for beta :in betas
                    :do (assign self (list alpha beta) 1D0)))
    (normalize self)
    self))

(defparameter *alphas* (xrange 0 31))
(defparameter *betas* (xrange 1 51))
(defparameter *locations* (xrange 0 31))

(defparameter *suite* (paintball *alphas* *betas* *locations*))

;; prior (alpha, beta) positions
(view *suite*)

(defun strafing-speed (alpha beta x)
  (let ((theta (atan (- x alpha) beta)))
    (/ beta (expt (cos theta) 2))))

(defun location-pmf (alpha beta locations)
  (let ((pmf (pmf)))
    (loop :for x :in locations
          :for prob = (/ 1D0 (strafing-speed alpha beta x))
          :do (assign pmf x prob))
    (normalize pmf)
    pmf))

;; pmf of location given alpha = 10, for several values of beta
(view (location-pmf 10 10 *locations*))
(view (location-pmf 10 20 *locations*))
(view (location-pmf 10 40 *locations*))

(defmethod likelihood ((self paintball) evidence hypothesis)
  (let ((alpha ($0 hypothesis))
        (beta ($1 hypothesis))
        (x evidence))
    (p (location-pmf alpha beta (locations self)) x)))

;; observations
(observe *suite* '(15 16 18 21) :multiplep t)

;; posterior (alpha, beta) positions
(view *suite*)

;; JOINT DISTRIBUTIONS
;;
;; when each value in a distribution is a list of variables, it is called a joint distribution because
;; it represents the distributions of the variables together, that is "jointly".
;; a joint distribution contains the distributions of the variables, as well information about the
;; relationships among them.

;; given a join distribution, we can compute the distribution of each variable independently, which
;; are called the marginal distributions.

(defparameter *marginal-alpha* (marginal *suite* 0))
(defparameter *marginal-beta* (marginal *suite* 1))

;; marginal distributions for painball
(view *marginal-alpha*)
(view *marginal-beta*)

;; credible intervals for each marginal distributions
(credible-interval *marginal-alpha* 50)
(credible-interval *marginal-beta* 50)

;; CONDITIONAL DISTRIBUTIONS
;;
;; the above marginal distributions contains information about the variables independently, but they
;; do not capture the dependence between variables, if any.
;;
;; one way to visualize dependence is by computing conditional distributions.

;; conditional distribution of alpha(0) w.r.t beta(1) of given value
(defparameter *cond-beta-10* (conditional *suite* 0 1 10))
(view *cond-beta-10*)
(defparameter *cond-beta-20* (conditional *suite* 0 1 20))
(view *cond-beta-20*)
(defparameter *cond-beta-40* (conditional *suite* 0 1 40))
(view *cond-beta-40*)

;; distributions with credible intervals
(mplot:plot-points (maximum-likelihood-interval *suite* :percentage 75) :xrange '(0 30) :yrange '(0 50))
(mplot:plot-points (maximum-likelihood-interval *suite* :percentage 50) :xrange '(0 30) :yrange '(0 50))
(mplot:plot-points (maximum-likelihood-interval *suite* :percentage 25) :xrange '(0 30) :yrange '(0 50))

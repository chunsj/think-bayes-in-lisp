(in-package :think-bayes)

;; a model for hockey goal distribution
(defclass hockey (pmf) ())

;; historical data says average goals per game is 2.7 with standard deviation of 0.3
(defun hockey ()
  (let ((pmf (make-instance 'hockey)))
    (setf (xpmap pmf) (xpmap (gaussian-pmf :mu 2.7 :sigma 0.3 :nsigma 4)))
    pmf))

;; historical or prior distribution
(plot (hockey) :xtics 10)

;; 1. goal scoring in hockey is at least approximately poisson process which means that it is
;;    equally likely for a goal to be scored at any time during a game.
;; 2. we can assume that against a particular opponent, each team has some long-term average
;;    goal per game, denoted as λ.
;; how to represent each hypothesis; the hypothesis that λ = x with the float value.

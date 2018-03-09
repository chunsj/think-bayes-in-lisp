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

(defmethod likelihood ((self hockey) evidence hypothesis)
  (let ((l hypothesis)
        (k evidence))
    (p (poisson :rate l) k)))

(let ((s0 (hockey)))
  (plot s0))

;; bruins
(let ((s1 (hockey)))
  (observe s1 '(0 2 8 4) :multiplep t)
  (plot s1))

(let ((s1 (hockey)))
  (observe s1 '(0 2 8 4) :multiplep t)
  (maximum-likelihood s1))

;; canucks
(let ((s2 (hockey)))
  (observe s2 '(1 3 1 0) :multiplep t)
  (plot s2))

(let ((s2 (hockey)))
  (observe s2 '(1 3 1 0) :multiplep t)
  (maximum-likelihood s2))

(let* ((l 3.4)
       (goal-dist (poisson-pmf :rate l :n 11)))
  (plot goal-dist))

(defun goal-pmf (s)
  (let ((meta-pmf (pmf))
        (max-goal 10))
    (loop :for xp :in (xps s)
          :for l = (car xp)
          :for p = (cdr xp)
          :for pmf = (poisson-pmf :rate l :n (1+ max-goal))
          :do (assign meta-pmf pmf p))
    (mixture meta-pmf)))

(defparameter *bruins* (hockey))
(defparameter *canucks* (hockey))

(observe *bruins* '(0 2 8 4) :multiplep t)
(observe *canucks* '(1 3 1 0) :multiplep t)

(plot *bruins*)
(plot *canucks*)

(defparameter *bruins-goal-dist* (goal-pmf *bruins*))
(defparameter *canucks-goal-dist* (goal-pmf *canucks*))
(defparameter *goal-diff* (subtract *bruins-goal-dist* *canucks-goal-dist*))

(plot *bruins-goal-dist*)
(plot *canucks-goal-dist*)
(plot *goal-diff*)

;; for bruins's win
(defparameter *p-win* (p> *goal-diff* 0))
(defparameter *p-loss* (p< *goal-diff* 0))
(defparameter *p-tie* (p *goal-diff* 0))

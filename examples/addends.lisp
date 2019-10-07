(in-package :think-bayes)

;; DO THINK DISTRIBUTIONS AS FUNDAMENTAL UNITS OF COMPUTATION, not just containers with values
;; and probabilities

;; distribution of the sum of three 6 side dice
;;
;; 1. by simulation: given a pmf for the single die, draw samples add them up then accumulate
;; 2. by enumeration: given pmfs, you can enumerate all possible list of values then sums

;; simulation approach
(defclass die (pmf) ())

(defun die (side) (pmf :class 'die :hypotheses (xrange 1 (1+ side))))

(defparameter *d6* (die 6))
(defparameter *dice* (loop :repeat 3 :collect (die 6)))

;; compute sum of randomly sampled data from given distributions
(defun random-sum (dists) (reduce #'+ (mapcar #'rand dists)))

;; make pmf from n times repeated sums of random sampling from distributions
(defun sample-sum (dists n) (pmf :hypotheses (loop :repeat n :collect (random-sum dists))))

(defparameter *three* (sample-sum *dice* 1000))
(view *three*) ;; of course, not perfect

;; enumeration approach
(defparameter *three-exact* (reduce #'add *dice*))
;; add works under the assumption of the random selections are independent
(view *three-exact*) ;; exact

;;
;; now for max instead of sum
;;
(defparameter *sum-dice* (loop :repeat 6 :collect (reduce #'add *dice*)))

;; simulation
(defun random-max (dists) (reduce #'max (mapcar #'rand dists)))
(defun sample-max (dists n) (pmf :hypotheses (loop :repeat n :collect (random-max dists))))

(defparameter *max-six* (sample-max *sum-dice* 1000))
(view *max-six*)

;; enumeration
(defun pmfmax (pmf1 pmf2)
  (let ((res (pmf)))
    (loop :for xp1 :in (xps pmf1)
          :for v1 = (car xp1)
          :for p1 = (cdr xp1)
          :do (loop :for xp2 :in (xps pmf2)
                    :for v2 = (car xp2)
                    :for p2 = (cdr xp2)
                    :do (increase res (max v1 v2) (* p1 p2))))
    res))

(defparameter *max-six-exact* (reduce #'pmfmax *sum-dice*))
(view *max-six-exact*)

;; using cdf where cdf(x) = p(X <= x)
;; CDF(5) means the probability that a value from this distribution is <= 5
;; for k times selection, CDF(5)**k
;; this is more efficient than above enumeration
(defparameter *max-cdf* (maximum *three-exact* 6))
(defparameter *max-pmf* (to-pmf *max-cdf*))
(view *max-pmf*)

;; mixtures
;;
;; a box of dice: 5 x d4, 4 x d6, 3 x d8, 2 x d12, 1 x d20
;;
;; if one die is selected from the box and rolled, what is the distribution of the outcome?
;; if we know the side of die, we know that it will be a uniform distribution from 1 to the side.
;; but if we do not know which die is selected, the resulting distribution is a mixture of uniform
;; distributions with different bounds.

;; simple case of d6 and d8
(defparameter *d6* (die 6))
(defparameter *d8* (die 8))

(defun mix ()
  (let ((mix (pmf)))
    (loop :for die :in (list *d6* *d8*)
          :do (loop :for xp :in (xps die)
                    :for outcome = (car xp)
                    :for prob = (cdr xp)
                    :do (increase mix outcome prob)))
    (normalize mix)
    mix))

;; distribution
(view (mix))

;; pmf for the box of dice
(defun pmf-dice ()
  (let ((pmf (pmf)))
    (assign pmf (die 4) 5)
    (assign pmf (die 6) 4)
    (assign pmf (die 8) 3)
    (assign pmf (die 12) 2)
    (assign pmf (die 20) 1)
    (normalize pmf)
    pmf))

(defun box ()
  (let ((pmf (pmf))
        (dice (pmf-dice)))
    (loop :for xp :in (xps dice)
          :for die = (car xp)
          :for weight = (cdr xp)
          :do (loop :for xp2 :in (xps die)
                    :for outcome = (car xp2)
                    :for prob = (cdr xp2)
                    :do (increase pmf outcome (* weight prob))))
    pmf))

(view (box))

;; mixture
(view (mixture (pmf-dice)))

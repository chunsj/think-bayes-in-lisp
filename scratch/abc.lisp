(in-package :think-bayes)

;; coefficient of variation (CV)
;; to compare variability between groups, CV is proper choice, which is the standard deviation
;; divided by the mean. it is dimensionless measure of variability relative to scale.
;;
;; but we can use bayesian methods to make this conclusion more precise.
;;
;; 1. start with the simplest implementation, but it only works for datasets smaller than 1000
;;    values.
;; 2. by computing probabilities under a log transform, we can scale up to the full size of the
;;    dataset, but the computation gets slow.
;; 3. finally, we speed things up substantially with approximate bayesian computation, ABC.

(defclass height (pmf) ())

(defun height (mus sigmas)
  (let ((pmf (pmf :class 'height)))
    (loop :for mu :in mus
          :do (loop :for sigma :in sigmas
                    :do (increase pmf (list mu sigma))))
    (normalize pmf)
    pmf))

(defmethod likelihood ((self height) evidence hypothesis)
  (let ((x evidence)
        (mu ($0 hypothesis))
        (sigma ($1 hypothesis)))
    (p (gaussian :mu mu :sigma sigma) x)))

;; to make bayesian techniques more efficient, we can use estimators to find a likely location
;; for mu and sigma, and use the standard errors of those estimates to choose a likely spread.
;;
;; if true parameters of the distribution are µ and σ, and we take a sample of n values, an
;; estimator of µ is the sample sample mean, m, for σ is the sample stadnard variance, s.
;; the standard error of the estimated µ is s/sqrt(n) and the standard error of the estimated
;; σ is s/sqrt(2(n-1)).

(defun mkrange (estimate stderr nstderr npts)
  (let ((spread (* stderr nstderr)))
    (linspace (- estimate spread) (+ estimate spread) npts)))

(defun find-prior-range (xs npts &optional (nstderr 3D0))
  (let* ((n ($count xs))
         (m (mean xs))
         (s (sd xs))
         (stderr-m (/ s (sqrt n)))
         (mus (mkrange m stderr-m nstderr npts))
         (stderr-s (/ s (sqrt (* 2D0 (- n 1)))))
         (sigmas (mkrange s stderr-s nstderr npts)))
    (cons mus sigmas)))

;; make fake dataset (i do not want to find read data)
(defparameter *heights* (sample (gaussian-pmf :mu 178D0 :sigma 7.7D0) 2000))
(mean *heights*)
(sd *heights*)

(let* ((prior-range (find-prior-range *heights* 31))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (observe suite *heights* :multiplep t)
  (maximum-likelihood suite))

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
(defparameter *heights* (sample (gaussian-pmf :mu 178D0 :sigma 7.7D0) 100))
(defparameter *wheights* (sample (gaussian-pmf :mu 163D0 :sigma 7.3D0) 100))
(mean *heights*)
(sd *heights*)

;; we use the data to choose the range of the prior distribution
;; then, use the data again to do the update.
;; we get the posterior join distribution of mu and sigma.
(let* ((prior-range (find-prior-range *heights* 31 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (observe suite *heights* :multiplep t)
  (maximum-likelihood suite))

;; compute distribution of coefficient of variation (CV)
(defun cv (pmf)
  (let ((res (pmf :class (type-of pmf))))
    (loop :for xp :in (xps pmf)
          :for ms = (car xp)
          :for mu = ($0 ms)
          :for sigma = ($1 ms)
          :for p = (cdr xp)
          :do (increase res (/ sigma mu) p))
    res))

(let* ((prior-range (find-prior-range *heights* 31 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (observe suite *heights* :multiplep t)
  (view (cv suite)))

;; compare men and women
(defun posterior-stats (heights)
  (let* ((prior-range (find-prior-range heights 31 4D0))
         (mus (car prior-range))
         (sigmas (cdr prior-range))
         (suite (height mus sigmas)))
    (observe suite heights :multiplep t)
    suite))

;; women has higher probability of having larger cv in height.
(let ((men-stats (posterior-stats *heights*))
      (women-stats (posterior-stats *wheights*)))
  (p< (cv men-stats) (cv women-stats)))

;; log likelihood for numerical stability
(defmethod llikelihood ((self height) evidence hypothesis)
  (let ((x evidence)
        (mu ($0 hypothesis))
        (sigma ($1 hypothesis)))
    (log (p (gaussian :mu mu :sigma sigma) x))))

(defmethod lobserve ((self height) evidence &key multiplep)
  (declare (ignorable multiplep))
  (call-next-method))

(let* ((prior-range (find-prior-range *heights* 31 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (logarithmize suite)
  (lobserve suite *heights* :multiplep t)
  (exponentiate suite)
  (normalize suite)
  (maximum-likelihood suite))

;; ABC or approximate bayesian computation
;;
;; the motivation behind ABC is that the likelihood of any particular dataset is:
;; 1. very small, especially for large datasets, which is why we had to use the log transform
;; 2. expensive to compute, which is why we had to do so much optimization, and
;; 3. not really what we want anyway.

;; for this example, we just know that the distribution is gaussian and their sample statistics
;; follows the same rule as with our prior distribution construction; sample mean and its variance,
;; sample deviation and its variance. we can use this knoweldge.

(defmethod lobserve ((self height) evidence &key multiplep)
  (declare (ignore multiplep))
  (let* ((xs evidence)
         (n ($count xs))
         (m (mean xs))
         (s (sd xs)))
    (loop :for hypo :in (xs self)
          :for mu = ($0 hypo)
          :for sigma = ($1 hypo)
          :for stderr-m = (/ sigma (sqrt n))
          :for llm = (log (p (gaussian :mu mu :sigma stderr-m) m))
          :for stderr-s = (/ sigma (sqrt (* 2 (- n 1))))
          :for lls = (log (p (gaussian :mu sigma :sigma stderr-s) s))
          :do (increase self hypo (+ llm lls)))))

(let* ((prior-range (find-prior-range *heights* 31 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (logarithmize suite)
  (lobserve suite *heights*)
  (exponentiate suite)
  (normalize suite)
  (maximum-likelihood suite))

(median-inter-percentile-range *heights* 0.5)
(median-sigma *heights* 2)

;; for large case
(defparameter *heights* (sample (gaussian-pmf :mu 178D0 :sigma 7.7D0) 100000))
(defparameter *wheights* (sample (gaussian-pmf :mu 163D0 :sigma 7.3D0) 100000))

;; we can use median, sigma, instead of sample mean and sd
(defmethod lobserve ((self height) evidence &key multiplep)
  (declare (ignore multiplep))
  (let* ((xs evidence)
         (n ($count xs))
         (ms (median-sigma xs 2D0))
         (m (car ms))
         (s (cdr ms)))
    (loop :for hypo :in (xs self)
          :for mu = ($0 hypo)
          :for sigma = ($1 hypo)
          :for stderr-m = (/ sigma (sqrt n))
          :for llm = (log (p (gaussian :mu mu :sigma stderr-m) m))
          :for stderr-s = (/ sigma (sqrt (* 2 (- n 1))))
          :for lls = (log (p (gaussian :mu sigma :sigma stderr-s) s))
          :do (increase self hypo (+ llm lls)))))

;; men
(let* ((prior-range (find-prior-range *heights* 101 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (logarithmize suite)
  (lobserve suite *heights*)
  (exponentiate suite)
  (normalize suite)
  (plot (to-cdf (scale (empirical-pmf (sample (cv suite) 100)) 100D0))))

;; women
(let* ((prior-range (find-prior-range *wheights* 101 4D0))
       (mus (car prior-range))
       (sigmas (cdr prior-range))
       (suite (height mus sigmas)))
  (logarithmize suite)
  (lobserve suite *wheights*)
  (exponentiate suite)
  (normalize suite)
  (plot (to-cdf (scale (empirical-pmf (sample (cv suite) 100)) 100D0))))

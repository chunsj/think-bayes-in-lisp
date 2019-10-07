(in-package :think-bayes)

;; questions - unseen species problem
;;
;; - baesd on the number of species observed, can we estimate the total number of species in the
;;   environment?
;; - can we estimate the prevalence of each species, that is, the fraction of the total population
;;   beloning to each species?
;; - if we are planning to collect additional samples, can we predict how many new species we are
;;   likely to discover?
;; - how many additional reads are needed to increase the fraction of observed species to a given
;;   threshold?

;; simplified version of the problem
;;
;; we visit a wild animal preverse and see 3 lions, 2 tigers and one bear.
;; the number of each species is governed by the multinomial distribution. if the prevalence of
;; lions and tigers and bears is p-lion, p-tiger and p-bear, the likelihood of seeing 3 lions,
;; 2 tigers and one bear is proportional to p-lion^3 * p-tiger^2 * p-bear^1.
;; the approach that is tempting but not correct, is to use beta distributions to describe the
;; prevalence of each species separately. but there are 2 problems:
;; 1. we have implicitly used a prior for each species that is uniform from 0 to 1, but since we
;;    know that there are three species, that prior is not correct. the right prior should have a
;;    mean of 1/3, and there should be zero likelihood that any species has a prevalence of 100%.
;; 2. the distributions for each species are not independent, because the prevalence have to add
;;    up to 1. to capture this dependence, we need a join distribution for the three prevalences.
;; we can use a dirichlet distribution to solve both of these problems, which is the
;; multi-dimentional generalization of the beta distribution. instead of 2 outcomes,
;; like heads and tails, the dirichlet distribution handles any number of outcomes;
;; ex. p-lion, p-tiger, p-bear.

(defclass dirichlet ()
  ((n :initform nil :accessor n)
   (params :initform nil :accessor params)))

(defun dirichlet (n)
  (let ((instance (make-instance 'dirichlet)))
    (setf (n instance) n)
    (setf (params instance) (repeat n 1D0))
    instance))

(defun marginal-beta (self i)
  (let ((alpha0 (reduce #'+ (params self)))
        (alpha ($ (params self) i)))
    (beta :alpha alpha :beta (- alpha0 alpha))))

(let ((dirichlet (dirichlet 3)))
  (loop :for i :in '(0 1 2)
        :for beta = (marginal-beta dirichlet i)
        :collect (xmean beta)))

(defmethod observe ((self dirichlet) evidence &key multiplep)
  (declare (ignore multiplep))
  (let ((m (length evidence)))
    (loop :for i :from 0 :below m :do (incf ($ (params self) i) ($ evidence i)))))

;; if we know how many species there are, we can estimate the prevalence of each.
(let ((dirichlet (dirichlet 3)))
  (observe dirichlet '(3 2 1))
  (loop :for i :in '(0 1 2)
        :for beta = (marginal-beta dirichlet i)
        :collect (xmean beta)))

;; lion
(let ((dirichlet (dirichlet 3)))
  (observe dirichlet '(3 2 1))
  (view (to-pmf (marginal-beta dirichlet 0))))

;; tiger
(let ((dirichlet (dirichlet 3)))
  (observe dirichlet '(3 2 1))
  (view (to-pmf (marginal-beta dirichlet 1))))

;; bear
(let ((dirichlet (dirichlet 3)))
  (observe dirichlet '(3 2 1))
  (view (to-pmf (marginal-beta dirichlet 2))))

;; let's get back to the original problem, estimating the total number of species. to solve this
;; we should define a meta-suite, which is a suite that contains other suites as hypothesis.
;; in this case, the top-level suite contains hypotheses about the number of species. the bottom
;; level contains hypotheses about prevalences.

(defclass species (pmf) ())

(defun species (ns)
  (let ((hypos (mapcar (lambda (n) (dirichlet n)) ns)))
    (pmf :class 'species :hypotheses hypos)))

(defparameter *ns* (xrange 3 30))
(defparameter *suite* (species *ns*))

(defmethod observe ((self species) evidence &key multiplep)
  (declare (ignore multiplep))
  (call-next-method)
  (loop :for h :in (xs self) :do (observe h evidence)))

;; why call 1000 times? because dirichlet::likelihood doesn't actually compute the likelihood of
;; the data under the whole dirichlet distribution. instead, it draws one sample from the
;; hypothetical distribution and computes the likelihood of the data under the sampled set of
;; prevalences.
(defmethod likelihood ((self species) evidence hypothesis)
  (let ((dirichlet hypothesis))
    (loop :for i :from 0 :below 1000
          :sum (likelihood dirichlet evidence nil))))

(defun gamma-random (&key (alpha 1D0) (beta 1D0))
  (car (rgamma 1 alpha :rate beta)))

(defun random-dirichlet (self)
  (let* ((rs (mapcar (lambda (p) (gamma-random :alpha p)) (params self)))
         (sr (reduce #'+ rs)))
    (mapcar (lambda (r) (/ r sr)) rs)))

(defmethod likelihood ((self dirichlet) evidence hypothesis)
  (declare (ignore hypothesis))
  (let ((m (length evidence)))
    (cond ((< (n self) m) 0)
          (t (let* ((x evidence)
                    (ps (random-dirichlet self))
                    (qs (loop :for i :from 0 :to m :collect (expt ($ ps i) ($ x i)))))
               (reduce #'* qs))))))

(defun dist-of-n (species)
  (let ((pmf (pmf)))
    (loop :for xp :in (xps species)
          :for hypo = (n (car xp))
          :for prob = (cdr xp)
          :do (assign pmf hypo prob))
    pmf))

(view (dist-of-n *suite*))

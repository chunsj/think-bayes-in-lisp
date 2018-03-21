(in-package :think-bayes)

;; suppose that a radioactive source emits particles toward a geiger counter at an average rate of
;; r particles per second, but the counter only registers fraction, f, of the particles that hit it.
;; if f is 10% and the counter registers 15 particles in a one second interval, what is the posterior
;; distribution of n, the actual number of particles that hit the counter, and r, the average rate
;; particles are emitted?
;;
;; 1. the source emits particles at an average rate r.
;; 2. during any given second, the source emits n particles toward the counter.
;; 3. out of those n particles, some number, k, get counted.
;;
;; the probability that an atom decays is the same at any point in time, so radioactive decay is
;; well modeled by a poisson process. given r, the distribution of n is poisson distribution with
;; parameter r.

;; example poisson distribution with rate of 5.
(view (poisson-pmf :rate 5))

;; if we assume that the probability of detection for each particles is independent of the others,
;; the distribution of k is the binomial distribution with parameters n and f.

;; example of binomial f,k of 1, n of 5.
(view (binomial-pmf :k 1 :n 5))

;; given the parameters of the system, we can find the distribution of the data, so we can solve
;; what is called the "forward problem".
;; now, we want to go the other way; given the data, we want the distribution of the parameters.
;; this is called the inverse problem. and if you can solve forward problem, you can use bayesian
;; methods to solve the inverse problem.

;; SIMPLE VERSION
;;
;; a simple version of the problem where we know the value of r. we are given the value of f, so
;; all we have to do is estimate n.
;; model the behavior of the detector and estimate n.

(defclass detector (pmf)
  ((rate :initform nil :accessor rate)
   (fraction :initform nil :accessor fraction)))

(defun detector (r f &key (high 500) (step 1))
  (let ((pmf (poisson-pmf :rate r :xs (linspace 0 high (1+ (/ high step)))))
        (instance (pmf :class 'detector)))
    (copy pmf :to instance :class 'detector)
    (setf (rate instance) r)
    (setf (fraction instance) f)
    instance))

(defmethod likelihood ((self detector) evidence hypothesis)
  (let ((k evidence)
        (n hypothesis)
        (p (fraction self)))
    (binomial-probability p :k k :n n)))

(defparameter *detectors* (let ((f 0.1)
                                (k 15))
                            (loop :for r :in '(100 250 400)
                                  :collect (let ((suite (detector r f :step 1)))
                                             (observe suite k)
                                             suite))))

(mapcar #'maximum-likelihood *detectors*)
(view ($0 *detectors*))
(view ($1 *detectors*))
(view ($2 *detectors*))

;; HIERARCHICAL
;;
;; we've assumed that we know r. now, let's relax that assumption. another model, emitter will be
;; defined, which models the behavior os the emitters and estimates r.

;; An emitter is a meta pmf of detectors.
(defclass emitter (pmf) ())

(defun emitter (rs &key (f 0.1))
  (let ((detectors (mapcar (lambda (r) (detector r f)) rs)))
    (pmf :class 'emitter :hypotheses detectors)))

(defmethod likelihood ((self emitter) evidence hypothesis)
  (let ((detector hypothesis))
    (observe detector evidence)))

(defmethod observe ((self emitter) evidence &key multiplep)
  (declare (ignore multiplep))
  (call-next-method)
  (loop :for detector :in (xs self)
        :do (observe detector evidence)))

(defun dist-of-r (self)
  (let ((pmf (pmf))
        (items (->> (loop :for xp :in (xps self)
                          :for detector = (car xp)
                          :for prob = (cdr xp)
                          :collect (cons (rate detector) prob))
                    (sortxps))))
    (loop :for item :in items
          :for r = (car item)
          :for p = (cdr item)
          :do (assign pmf r p))
    pmf))

(defun dist-of-n (self) (mixture self))

;; 1. at the top-level, we start with a range of hypothetical values for r.
;; 2. for each value of r, we have a range of values for n, and the prior distribution of n depends
;;    on r.
;; 3. when we update the model, we go bottom-up. we compute a posterior distribution of n for each
;;    value of r, then compute the posterior distribution of r.
(defparameter *emitter* (emitter (linspace 50 400 36)))
(observe *emitter* 15)

(view (dist-of-r *emitter*))
(view (dist-of-n *emitter*))

(in-package :think-bayes)

;; simple model
;; time between discharge and diagnosis, in days
(defparameter *interval* 3291.0D0)

;; median volume doubling time reported by Zhang et al is 811 days
;; doubling time in linear measure is doubling time in volume * 3
(defparameter *dt* (* 811.0D0 3))

;; number of doublings since discharge
(defparameter *doublings* (/ *interval* *dt*))

;; how big was the tumor at time of discharge (diameter in cm)
(defparameter *d1* 15.5D0)
;; the computed result is about 6 cm. so if this tumor formed after the date of discharge,
;; it must have grown substantially faster than the median rate.
;; (tumor size is 15.5 cm at detection)
(defparameter *d0* (/ *d1* (expt 2.0 *doublings*)))

;; compute the growth rate that would be implied if this tumor had formed after the date of discharge,
;; which seems not be true. if we assume an initial size of 0.1 cm, we can compute the number of
;; doublings to get to a final size of 15.5 cm.
(defparameter *d0* 0.1D0)
(defparameter *d1* 15.2D0)

;; how many doublings would it take to get from *do* to *d1*
(defparameter *doublings* (/ (log (/ *d1* *d0*)) (log 2D0)))

;; what linear doubling time does that imply?
(defparameter *dt* (/ *interval* *doublings*))

;; compute the volumetric doubling time and RDT
(defparameter *vdt* (/ *dt* 3D0))
;; *doublings is about 7.3 which implies *rdt* of 2.4. in the data from Zhang et al, only 20% of
;; tumors grew this fast during a period of observation. so again, the conclusion is that "more
;; likely than not" the tumor formed pior to the date of discharge.
(defparameter *rdt* (/ 365D0 *vdt*))

(defun rdt-cdf ()
  (let ((n 53D0)
        (freqs '(0 2 31 42 48 51 52 53)))
    (cdf :ps (mapcar (lambda (freq) (/ freq n)) freqs)
         :xs (xrange -1.5D0 6.5D0 1D0))))

(view (rdt-cdf))
(view (to-cdf (pmf :hypotheses (sort (sample (rdt-cdf) 10000) #'<))))

;; probability of rdt > *rdt*
(- 1D0 (p (rdt-cdf) *rdt*))

;; more general model
;; given the size of a tumor at time of diagnosis, it would be most useful to know the probability
;; that the tumor formed before any given date; in other words, the distribution of ages.
;;
;; to find it, simulations of tumor growth is run to get the distribution of size conditioned on
;; age. then we can use a bayesian approach to get the distribution of age conditioned on size.
;;
;; 1. choose a growth rate from the distribution of RDT.
;; 2. compute the size of the tumor at the end of an interval.
;; 3. record the size of the tumor at each interval.
;; 4. repeat until the tumor exceeds the maximum relevant size.
;;
;; initial size: 0.3 cm
;; interval: 245 days
;; maximum size: 20 cm
;; assumes growth rate is constant during interval

(defclass cache ()
  ((joint :initform (pmf) :accessor joint)))

(defun diameter (volume &key (factor (/ 3D0 pi 4D0)) (exp (/ 1D0 3D0)))
  (* 2D0 (expt (* factor volume) exp)))

(defun cm-to-bucket (x &optional (factor 10D0)) (* factor (log x)))

(defun add-observation (self age seq)
  (let* ((final ($last seq))
         (cm (diameter final))
         (bucket (cm-to-bucket cm)))
    (increase (joint self) (list age bucket))))

(defun consitional-cdf (self bucket)
  (let ((pmf (conditional (joint self) 0 1 bucket)))
    (to-cdf pmf)))

(defun volume (diameter &optional (factor (/ (* 4 pi) 3D0))) (* factor (expt (/ diameter 2D0) 3D0)))

(defparameter *cache* (make-instance 'cache))

(defun extend-sequence (age seq rdt interval)
  (let* ((initial ($last seq))
         (doublings (* rdt interval))
         (final (* initial (expt 2D0 doublings)))
         (new-seq (append seq (list final))))
    (add-observation *cache* age new-seq)
    (cons final new-seq)))

(defun make-sequences (rdt-seq &key (v0 '(0.01D0)) (interval 0.67D0) (vmax (volume 20D0)))
  (loop :for rdt :in rdt-seq
        :for seq = v0 :then seqn
        :for age = interval :then (+ age interval)
        :for rs = (extend-sequence age seq rdt interval)
        :for final = (car rs)
        :for seqn = (cdr rs)
        :when (> final vmax)
          :return seq))

(make-sequences '(1.0 1.5 2.0 2.5))

(in-package :think-bayes)

;; THE MODEL
;;
;; passenger arrivals as a poisson process, which means that passengers are equally likely to arrive
;; at any time, and that they arrive at an unknown rate, λ, measured in passengers per minute, and is
;; assumed as a constant.
;;
;; the arrival process for trains is not poisson. trains are supposed to leave from the end of line
;; every 7~8 minutes during peak time, but by the time they get to current station, the time between
;; trains varies between 3 and 12 minutes.

;; observed gap time data
(defparameter *gap-times*
  (list 428.0 705.0 407.0 465.0 433.0 425.0 204.0 506.0 143.0 351.0
        450.0 598.0 464.0 749.0 341.0 586.0 754.0 256.0 378.0 435.0
        176.0 405.0 360.0 519.0 648.0 374.0 483.0 537.0 578.0 534.0
        577.0 619.0 538.0 331.0 186.0 629.0 193.0 360.0 660.0 484.0
        512.0 315.0 457.0 404.0 740.0 388.0 357.0 485.0 567.0 160.0
        428.0 387.0 901.0 187.0 622.0 616.0 585.0 474.0 442.0 499.0
        437.0 620.0 351.0 286.0 373.0 232.0 393.0 745.0 636.0 758.0))

;; if you stood on the platform from 4pm to 6pm and recorded the time between trains, this is the
;; distribution you would see.
(defparameter *z* (empirical-pmf *gap-times* :xs (linspace 0 1200 1201)))
(plot *z*)

;; but if you arrive at some random time (without regard to the train schedule) you would see a
;; different distribution. the average time between trains, as seen by a random passenger, is
;; substantially higher than the true average.
;;
;; why? because a passenger is more likely to arrive during a large interval than a small one.
;; consider a simple example: suppose that the time between trains is either 5 minutes or 10 minutes
;; with equal probability. in that case the average time between trains is 7.5 minutes.
;;
;; but a passenger is more likely to arrive during a 10 minute gap than a 5 minute gap; in fact,
;; twice as likely. if we surveyed arriving passengers, we would find that 2/3 of them arrived
;; during a 10 minute gap, and only 1/3 during a 5 minute gap. so the average time between trains,
;; as seen by an arriving passengers, is 8.33 minutes.
;;
;; this kind of observer bias appears in many contexts. strudents think that classes are bigger than
;; they are because more of them are in the big classes. airline passengers think that plains are
;; fuller than they are because more of them are on full flights.
;;
;; in each case, values from the actual distribution are oversampled in proportion to their value.
;; in the red line example, a gap that is twice as big is twice as likely to be observed.
(defparameter *zb* (bias-pmf *z*))
(plot *zb*)

;; wait time, which is called as y, is the time between the arrival of a passenger and the next
;; arrival of a train. elapsed time, x, is the time between arrival of the previous train and the
;; arrival of a passenger.

;; zb = x + y
;;
;; if a gap is given as n minute, then y is uniform from 0 to n minutes. so the overall distribution
;; is a mixture of uniform distributions weighted according to the probability of each gap.

(defun waittime-pmf (zb)
  (let ((metapmf (pmf)))
    (loop :for xp :in (xps zb)
          :for gap = (car xp)
          :for prob = (cdr xp)
          :do (let ((pmf (uniform-pmf :low 0 :high gap :skip 10D0)))
                (assign metapmf pmf prob)))
    (mixture metapmf)))

(plot (waittime-pmf *zb*))

;; encapsulate the process
(defclass waittime-calc ()
  ((pmf-z :initform nil :accessor pmf-z)
   (pmf-zb :initform nil :accessor pmf-zb)
   (pmf-y :initform nil :accessor pmf-y)
   (pmf-x :initform nil :accessor pmf-x)))

(defun waittime-calc (pmf-z)
  (let ((self (make-instance 'waittime-calc)))
    (setf (pmf-z self) pmf-z)
    (setf (pmf-zb self) (bias-pmf (pmf-z self)))
    (setf (pmf-y self) (waittime-pmf (pmf-zb self)))
    (setf (pmf-x self) (pmf-y self)) ;; x = zp - y, 0 to gap uniform as y
    self))

;; cdf of z
(let ((calc (waittime-calc *z*))) (plot (to-cdf (pmf-z calc))))

;; cdf of zb
(let ((calc (waittime-calc *z*))) (plot (to-cdf (pmf-zb calc))))

;; cdf of y
(let ((calc (waittime-calc *z*))) (plot (to-cdf (pmf-y calc))))

;; mean of z
(let ((calc (waittime-calc *z*))) (/ (xmean (pmf-z calc)) 60D0))

;; mean of zb
(let ((calc (waittime-calc *z*))) (/ (xmean (pmf-zb calc)) 60D0))

;; mean of y
(let ((calc (waittime-calc *z*))) (/ (xmean (pmf-y calc)) 60D0))

;; wait times prediction
;;
;; suppose that when i arrive at the platform i see 10 people waiting, how long should i expect to
;; wait until the next train arrives?
;;
;; suppose that we are given the actual distribution of z, and we know that the passenger arrival
;; rate λ as 2 passengers per minute.
;;
;; in this case we can:
;; 1. use the distribution of z to compute the prior distribution of zp, the time between trains as
;;    seen by a passenger.
;; 2. then we can use the number of passengers to estimate the distribution of x, the elapsed time
;;    since the last train.
;; 3. finally, we use the relation y = zb - x to get the distribution of y.
(defparameter *wtc* (waittime-calc *z*))

(defclass elapsedtime-estimator ()
  ((prior-x :initform nil :accessor prior-x)
   (posterior-x :initform nil :accessor posterior-x)
   (pmf-y :initform nil :accessor pmf-y)))

(defclass elapsed (pmf) ())

(defmethod likelihood ((self elapsed) evidence hypothesis)
  (let ((x hypothesis)
        (l (car evidence))
        (k (cdr evidence)))
    (p (poisson :rate (* l x)) k)))

(defun predict-waittime (pmf-zb pmf-x)
  (let ((pmf-y (subtract pmf-zb pmf-x)))
    (removex pmf-y (lambda (x) (< x 0D0)))
    pmf-y))

(defun elapsedtime-estimator (wtc rate npass)
  (let ((self (make-instance 'elapsedtime-estimator)))
    (setf (prior-x self) (copy (pmf-x wtc) :class 'elapsed))
    (setf (posterior-x self) (copy (prior-x self)))
    (observe (posterior-x self) (cons rate npass))
    (setf (pmf-y self) (predict-waittime (pmf-zb wtc) (posterior-x self)))
    self))

(defparameter *ete* (elapsedtime-estimator *wtc* (/ 2D0 60D0) 15))

(plot (to-cdf (prior-x *ete*)))
(plot (to-cdf (posterior-x *ete*)))

;; predicted wait time
(plot (to-cdf (pmf-y *ete*)))
;; with 80% confidence, we expect the next train in less than ~320 secs or 5 minutes or so.
(percentile (pmf-y *ete*) 80)

;; what if we do not know the arrival rate of passengers, which is more realistic condition.
;; (we're relaxing second condition of above analysis)
;; so, we know only the distribution of gaps
;;
;; we need observation. and following is observed data
;; after 5 days
;; k1:  17  22  23  18   4 - passengers when you arrived
;;  y: 4.6 1.0 1.4 5.4 5.8 - wait time in minutes
;; k2:   9   0   4  12  11 - passenders after you arrived

(defclass arrival-rate (pmf) ()) ;; almost identical to elapsed only different in hypothesis

;; evidence is a wait time and the number of passengers that arrived
(defmethod likelihood ((self arrival-rate) evidence hypothesis)
  (let ((l hypothesis)
        (y (car evidence))
        (k (cdr evidence)))
    (p (poisson :rate (* y l)) k)))

(defclass arrival-rate-estimator ()
  ((prior-rate :initform nil :accessor prior-rate)
   (posterior-rate :initform nil :accessor posterior-rate)))

(defun arrival-rate-estimator (passenger-data)
  (let ((self (make-instance 'arrival-rate-estimator))
        (hypos (mapcar (lambda (n) (/ n 60D0)) (linspace 0 5 51))))
    (setf (prior-rate self) (pmf :class 'arrival-rate :hypotheses hypos))
    (removex (prior-rate self) (lambda (x) (= x 0)))
    (setf (posterior-rate self) (pmf :class 'arrival-rate :hypotheses hypos))
    (removex (posterior-rate self) (lambda (x) (= x 0)))
    (loop :for data :in passenger-data
          :for y = (* 60D0 ($1 data))
          :for k2 = ($2 data)
          :for evidence = (cons y k2)
          :do (observe (posterior-rate self) evidence))
    self))

(defparameter *passenger-data* '((17 4.6 9)
                                 (22 1.0 0)
                                 (23 1.4 4)
                                 (18 5.4 12)
                                 (4 5.8 11)))

(defparameter *are* (arrival-rate-estimator *passenger-data*))

;; prior cdf - arrival rate (passengers / min)
(plot (scale (to-cdf (prior-rate *are*)) 60))

;; posterior cdf - arrival rate (passengers / min)
(plot (scale (to-cdf (posterior-rate *are*)) 60))

;; incorporating uncertainty about one of the inputs
;; 1. implement the analysis based on a deterministic value of the uncertain parameter (λ).
;; 2. compute the distribution of the uncertain parameter.
;; 3. run the analysis for each value of the parameter, and generate a set of predictive
;;    distributions.
;; 4. compute a mixture of the predictive distributions, using the weights from the distribution
;;    of the parameter.

(defclass wait-mixture-estimator ()
  ((metapmf :initform nil :accessor meta-pmf)
   (mixture :initform nil :accessor mixture-pmf)))

(defun wait-mixture-estimator (wtc are &optional (num-passengers 15)))

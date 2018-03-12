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

(in-package :think-bayes)

(defclass euro (pmf) ())

(defun euro () (pmf :class 'euro :hypotheses (xrange 0 101)))

;; better prior
(defun triangular-prior ()
  (let ((suite (pmf :class 'euro)))
    (loop :for x :in (xrange 0 51) :do (assign suite x (coerce x 'double-float)))
    (loop :for x :in (xrange 51 101) :do (assign suite x (- 100D0 x)))
    (normalize suite)
    suite))

;; more efficient likelihood
(defmethod likelihood ((self euro) data hypothesis)
  (let ((faceprob (/ hypothesis 100D0))
        (heads (car data))
        (tails (cdr data)))
    (* (expt faceprob heads) (expt (- 1D0 faceprob) tails))))

;; coin is fair or not
;;
;; two hypotheses to consider: F for the fair, B for biased

;; if the coin is fair, it is easy to compute the likelihood of the data p(D|F).
;; is this fair comparison?
(defparameter *f* (let ((suite (euro)))
                    (likelihood suite (cons 140 110) 50D0)))
(defparameter *b-cheat* (let ((suite (euro)))
                          (likelihood suite (cons 140 110) (/ (* 100D0 140) (+ 140 110)))))
(/ *b-cheat* *f*)

(defparameter *b-two* (let* ((suite (euro))
                             (data (cons 140 110))
                             (like40 (likelihood suite data 40))
                             (like60 (likelihood suite data 60)))
                        (+ (* 0.5 like40) (* 0.5 like60))))
(/ *b-two* *f*)

(defparameter *b-uniform* (let ((suite (euro))
                                (data (cons 140 110)))
                            (removex suite (lambda (x) (eq x 50)))
                            (loop :for xp :in (xps suite)
                                  :for hypo = (car xp)
                                  :for prob = (cdr xp)
                                  :for like = (likelihood suite data hypo)
                                  :summing (* like prob))))
(/ *b-uniform* *f*)

(defparameter *b-uniform* (let ((suite (euro))
                                (data (cons 140 110)))
                            (removex suite (lambda (x) (eq x 50)))
                            (observe suite data)))
(/ *b-uniform* *f*)

(defparameter *b-triangle* (let ((suite (triangular-prior))
                                 (data (cons 140 110)))
                             (removex suite (lambda (x) (eq x 50)))
                             (observe suite data)))
(/ *b-triangle* *f*)

;; IMPORTANT
;;
;; in summary, we can use bayesian hypothesis testing to compare the likelihood of F and B, but
;; we have to do some work to specify precisely what B means. this specification depends on
;; background information about coins and their behavior when spun, so people could reasonably
;; disagree about the right definition.
;;
;; bayes factor
;;
;; 1. 1~3: barely worth mentioning
;; 2. 3~10: substantial
;; 3. 10~30: strong
;; 4. 30~100: very strong
;; 5. >100: decisive
;;
;; if your prior odds are 1:1, and you see evidence with bayes factor 2, your posterior odds are
;; 2:1. in terms of probability, the data changed your degree of belief from 50% to 66%. for most
;; real world problems, that change would small relative to modeling errors and other sources of
;; uncertainty.
;; on the other hand, if you had seen evidence with bayes factor 100, your posterior odds would be
;; 100:1 or more than 99%. whether or not you agree that such evidence is "decisive", it's strong.

(in-package :think-bayes)

(defclass train (pmf) ())

(defmethod likelihood ((self train) data hypo)
  (if (< hypo data)
      0
      (/ 1.0 hypo)))

(defun collect-integers (n) (loop :for i :from 1 :to n :collect i))

(let ((suite (pmf :class 'train :hypotheses (collect-integers 1000))))
  (update suite 60)
  (xmean suite))

(let ((suite (pmf :class 'train :hypotheses (collect-integers 1000))))
  (update suite 60)
  (plot-pmf suite))

;; with different priors
(let ((s1 (pmf :class 'train :hypotheses (collect-integers 500)))
      (s2 (pmf :class 'train :hypotheses (collect-integers 1000)))
      (s3 (pmf :class 'train :hypotheses (collect-integers 2000))))
  (loop :for data :in '(60 30 90)
        :do (progn (update s1 data)
                   (update s2 data)
                   (update s3 data)))
  #{:s1 (xmean s1) :s2 (xmean s2) :s3 (xmean s3)})

;; power law
(defun train2 (hypos &key (alpha 1.0))
  (let ((self (pmf :class 'train)))
    (loop :for h :in hypos :do (assign self h (expt h (- alpha))))
    (normalize self)
    self))

(let ((suite (train2 (collect-integers 1000))))
  (update suite 60)
  (plot-pmf suite))

(let ((s1 (train2 (collect-integers 500)))
      (s2 (train2 (collect-integers 1000)))
      (s3 (train2 (collect-integers 2000))))
  (loop :for data :in '(60 30 90)
        :do (progn (update s1 data)
                   (update s2 data)
                   (update s3 data)))
  #{:s1 (xmean s1) :s2 (xmean s2) :s3 (xmean s3)})

(let ((suite (train2 (collect-integers 1000))))
  (loop :for data :in '(60 30 90) :do (update suite data))
  #{:%5 (percentile suite 5) :%95 (percentile suite 95)})

(let ((suite (train2 (collect-integers 1000))))
  (loop :for data :in '(60 30 90) :do (update suite data))
  (let ((cdf (to-cdf suite)))
    #{:5% (percentile cdf 5) :95% (percentile cdf 95)}))

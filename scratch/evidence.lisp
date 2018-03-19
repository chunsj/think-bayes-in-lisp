(in-package :think-bayes)

;; if alice got 780 and bob got 740, decide whether alice is better prepared than bob, and what
;; the strength of that evidence is.
;; more specifically, how strong is the evidence that alice is better prepared than bob?
;;
;; for simplicity of modeling, pretend that all SAT questions are equally difficult. now, we can
;; define a characteristic, p-correct for each test-taker, which is the probability of answering
;; any question correctly. this simplication makes it easy to compute the likelihood of a given
;; score.

(defun read-scale ()
  (let* ((fname "/Users/Sungjin/Documents/Python/ThinkBayes/code/sat_scale.csv")
         (lines (read-lines-from fname))
         (raws nil)
         (scores nil))
    (loop :for i :from 5 :below (length lines)
          :for line = ($ lines i)
          :for unquoted = (remove #\" line)
          :for comps = (split #\, unquoted)
          :for raw = (parse-integer ($ comps 2))
          :for scomps = (split #\- ($ comps 3))
          :for score = (* 1D0 (/ (reduce #'+ (mapcar #'parse-integer scomps)) (length scomps)))
          :do (progn (push raw raws)
                     (push score scores)))
    (interpolator (sort raws #'<) (sort scores #'<))))

(defun read-score ()
  (let* ((fname "/Users/Sungjin/Documents/Python/ThinkBayes/code/sat_ranks.csv")
         (lines (subseq (read-lines-from fname) 3)))
    (->> (loop :for i :from 0 :below (length lines)
               :for line = ($ lines i)
               :for comps = (split #\, line)
               :when (> (length ($0 comps)) 1)
                 :collect (let ((score (parse-integer ($0 comps)))
                                (freq (parse-integer ($1 comps))))
                            (cons score freq)))
         (reverse))))

(plot (to-cdf (pmf :histogram (read-score))))

;; score-pmf: pmf of scaled scores
;; raw-pmf: pmf of raw scores
;; prior-pmf: pmf of p-correct
(defclass exam ()
  ((scales :initform (read-scale) :accessor scales)
   (scores :initform (read-score) :accessor scores)
   (scorepmf :initform nil :accessor score-pmf)
   (raw :initform nil :accessor raw-pmf)
   (prior :initform nil :accessor prior-pmf)))

(defmethod y ((self exam) x) (y (scales self) x))
(defmethod x ((self exam) y) (max (x (scales self) y) 0D0))

(defun reverse-scale (exam pmf)
  (let ((new (pmf)))
    (loop :for xp :in (xps pmf)
          :for x = (car xp)
          :for p = (cdr xp)
          :for raw = (x exam x)
          :do (increase new raw p))
    new))

(defun exam ()
  (let ((self (make-instance 'exam)))
    (setf (score-pmf self) (pmf :histogram (scores self)))
    (setf (raw-pmf self) (reverse-scale self (score-pmf self)))
    (setf (prior-pmf self) (divide (raw-pmf self) (apply #'max (xs (raw-pmf self)))))
    self))

(plot (score-pmf (exam)))
(plot (raw-pmf (exam)))
(plot (to-cdf (prior-pmf (exam))))

(defclass sat (pmf)
  ((exam :initform nil :accessor exam-data)
   (score :initform nil :accessor score)))

(defun sat (exam score)
  (let ((self (pmf :class 'sat)))
    (setf (exam-data self) exam)
    (setf (score self) score)
    (loop :for xp :in (xps (prior-pmf exam))
          :for p-correct = (car xp)
          :for prob = (cdr xp)
          :do (assign self p-correct prob))
    (observe self score)
    self))

(x (exam) 780)
(apply #'max (xs (raw-pmf (exam))))

(apply #'max (xs (sat (exam) 780)))

(defun binomial (k n x) (gsll:binomial-p (round k) (coerce x 'double-float) (round n)))

(defmethod likelihood ((self sat) evidence hypothesis)
  (let ((p-correct hypothesis)
        (score evidence))
    (binomial (x (exam-data self) score) (apply #'max (xs (raw-pmf (exam-data self)))) p-correct)))

(plot (to-cdf (sat (exam) 780)))
(plot (to-cdf (sat (exam) 740)))

(defclass top-level (pmf) ())

(defmethod observe ((self top-level) evidence &key multiplep)
  (declare (ignore multiplep))
  (let* ((a-sat (car evidence))
         (b-sat (cdr evidence))
         (a-likelihood (p> a-sat b-sat))
         (b-likelihood (p< a-sat b-sat))
         (c-likelihood (p= a-sat b-sat)))
    (mult self :a (+ a-likelihood (/ c-likelihood 2D0)))
    (mult self :b (+ b-likelihood (/ c-likelihood 2D0)))
    (normalize self)))

(let* ((exam (exam))
       (a-sat (sat exam 780))
       (b-sat (sat exam 740))
       (top (pmf :class 'top-level :hypotheses '(:a :b))))
  (observe top (cons a-sat b-sat))
  top)

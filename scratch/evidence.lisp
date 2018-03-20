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
          :for y = (car xp)
          :for p = (cdr xp)
          :for raw = (x exam y)
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
(plot (prior-pmf (exam)))
(plot (to-cdf (prior-pmf (exam))))

(defclass sat (pmf)
  ((exam :initform nil :accessor exam-data)
   (score :initform nil :accessor score)))

(defun sat (exam score)
  (let ((self (pmf :class 'sat)))
    (setf (exam-data self) exam)
    (setf (score self) score)
    (copy (prior-pmf exam) :to self)
    (observe self score)
    self))

(x (exam) 780)
(x (exam) 740)
(apply #'max (xs (raw-pmf (exam))))
(apply #'max (xs (sat (exam) 780)))

(defmethod likelihood ((self sat) evidence hypothesis)
  (let ((p-correct hypothesis)
        (score evidence))
    (p (binomial :k (x (exam-data self) score)
                 :n (apply #'max (xs (raw-pmf (exam-data self)))))
       p-correct)))

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

;; a better model
;; assume that each test-taker has some degree of efficacy, which measures their ability to
;; answer SAT questions.
;; assume that each question has some level of difficulty.
;; assume that the chance that a test-taker answers a question correctly is related to efficacy
;; and difficulty according to this function:
(defun probability-correct (efficacy difficulty &optional (a 1D0))
  "from IRT or item response theory"
  (/ 1D0 (+ 1D0 (exp (* (- a) (- efficacy difficulty))))))

(defun binary-pmf (p)
  (let ((pmf (pmf)))
    (assign pmf 0 (- 1D0 p))
    (assign pmf 1 p)
    pmf))

(defun pmf-correct (efficacy difficulties)
  (let* ((pmf0 (pmf :hypotheses '(0)))
         (ps (mapcar (lambda (difficulty)
                       (probability-correct efficacy difficulty))
                     difficulties))
         (pmfs (mapcar (lambda (p) (binary-pmf p)) ps)))
    (reduce #'add pmfs :initial-value pmf0)))

(defclass exam ()
  ((scales :initform (read-scale) :accessor scales)
   (scores :initform (read-score) :accessor scores)
   (scorepmf :initform nil :accessor score-pmf)
   (raw :initform nil :accessor raw-pmf)
   (prior :initform nil :accessor prior-pmf)
   (difficulties :initform nil :accessor difficulties)))

(defun make-difficulties (center width n)
  (let ((low (- center width))
        (high (+ center width)))
    (linspace low high n)))

(defun exam ()
  (let ((self (make-instance 'exam)))
    (setf (score-pmf self) (pmf :histogram (scores self)))
    (setf (raw-pmf self) (reverse-scale self (score-pmf self)))
    (setf (prior-pmf self) (divide (raw-pmf self) (apply #'max (xs (raw-pmf self)))))
    (setf (difficulties self)
          (make-difficulties -0.05 1.8 (round (apply #'max (xs (raw-pmf self))))))
    self))

(defun raw-score-dist (exam efficacies)
  (let ((pmfs (pmf)))
    (loop :for xp :in (xps efficacies)
          :for efficacy = (car xp)
          :for prob = (cdr xp)
          :for scores = (pmf-correct efficacy (difficulties exam))
          :do (assign pmfs scores prob))
    (mixture pmfs)))

;; calibrate to difficulties: -0.05 1.8
;; efficacies deviation: 1.5
(plot (to-cdf (raw-pmf (exam))))
(plot (to-cdf (raw-score-dist (exam) (gaussian-pmf :sigma 1.5D0 :nsigma 3D0))))

(defclass sat2 (pmf)
  ((exam :initform nil :accessor exam-data)
   (score :initform nil :accessor score)))

(defun sat2 (exam score)
  (let ((self (pmf :class 'sat2)))
    (setf (exam-data self) exam)
    (setf (score self) score)
    (copy (gaussian-pmf :sigma 1.5D0 :nsigma 3D0) :to self)
    (observe self score)
    self))

(defmethod likelihood ((self sat2) evidence hypothesis)
  (let* ((efficacy hypothesis)
         (score evidence)
         (raw (x (exam-data self) score))
         (pmf (pmf-correct efficacy (difficulties (exam-data self)))))
    (p pmf raw)))

(plot (to-cdf (sat2 (exam) 780)))
(plot (to-cdf (sat2 (exam) 740)))

(let* ((exam (exam))
       (a-sat (sat2 exam 780))
       (b-sat (sat2 exam 740))
       (a-pred (raw-score-dist exam a-sat))
       (b-pred (raw-score-dist exam b-sat))
       (a-like (p> a-pred b-pred))
       (b-like (p< a-pred b-pred))
       (c-like (p= a-pred b-pred)))
  (list :a a-like :b (+ b-like c-like)))

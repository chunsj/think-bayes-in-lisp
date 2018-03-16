(in-package :think-bayes)

(defgeneric observe (pmf evidence &key multiplep))
(defgeneric likelihood (pmf evidence hypothesis))

(defgeneric assign (pmf x &optional p))
(defgeneric increase (pmf x &optional term))

(defgeneric init (pmf &key &allow-other-keys))

(defgeneric normalize (pmf &optional fraction))

(defgeneric xs (pmf))
(defgeneric ps (pmf))
(defgeneric xps (pmf))
(defgeneric x (pmf p))
(defgeneric p (pmf x &optional default))

(defgeneric add (pmf other))
(defgeneric mult (pmf x factor))
(defgeneric subtract (pmf other))

(defgeneric pmax (pmf))
(defgeneric xmean (pmf))
(defgeneric xvariance (pmf))
(defgeneric xsd (pmf))
(defgeneric percentile (pmf percentage))
(defgeneric credible-interval (pmf &optional percentage))

(defgeneric rand (pmf))
(defgeneric sample (pmf n))

(defgeneric maximum (pmf k))

(defgeneric maximum-likelihood (pmf))

(defgeneric to-cdf (pmf &key &allow-other-keys))
(defgeneric to-pmf (cdf &key &allow-other-keys))

(defgeneric plot (pmf &key &allow-other-keys))

(defgeneric copy (pmf &key &allow-other-keys))

(defgeneric p> (pmf other))
(defgeneric p>= (pmf other))
(defgeneric p< (pmf other))
(defgeneric p<= (pmf other))
(defgeneric p= (pmf other))

(defgeneric scale (pmf factor))

(defgeneric marginal (pmf i))
(defgeneric conditional (pmf i j val))
(defgeneric maximum-likelihood-interval (pmf &key percentage))

(defgeneric logarithmize (pmf))
(defgeneric exponentiate (pmf))

(defgeneric lobserve (pmf evidence &key multiplep))
(defgeneric llikelihood (pmf evidence hypothesis))

(defclass pmf () ((xpmap :initform #{} :accessor xpmap)))

(defmethod $ ((pmf pmf) x &rest default) ($ (xpmap pmf) x (car default)))

(defmethod (setf $) (p (pmf pmf) x &rest others)
  (declare (ignore others))
  (setf ($ (xpmap pmf) x) p))

(defmethod $count ((pmf pmf)) ($count (xpmap pmf)))

(defmethod print-object ((pmf pmf) stream)
  (format stream "#<PMF ~A>" (xpmap pmf)))

(defmethod init ((pmf pmf) &key hypotheses &allow-other-keys)
  (when hypotheses
    (loop :for h :in hypotheses :do (increase pmf h 1D0))
    (normalize pmf)))

(defun pmf (&key (class 'pmf) (hypotheses nil))
  (let ((instance (make-instance class)))
    (init instance :hypotheses hypotheses)
    instance))

(defun observe-evidence (pmf evidence)
  (loop :for h :in (xs pmf)
        :for l = (likelihood pmf evidence h)
        :do (mult pmf h l)))

(defun lobserve-evidence (pmf evidence)
  (loop :for h :in (xs pmf)
        :for l = (llikelihood pmf evidence h)
        :do (increase pmf h l)))

(defmethod observe ((pmf pmf) evidence &key (multiplep nil))
  (if multiplep
      (loop :for ev :in evidence
            :do (observe-evidence pmf ev))
      (observe-evidence pmf evidence))
  (normalize pmf))

(defmethod lobserve ((pmf pmf) evidence &key (multiplep nil))
  (if multiplep
      (loop :for ev :in evidence
            :do (lobserve-evidence pmf ev))
      (lobserve-evidence pmf evidence)))

(defmethod logarithmize ((pmf pmf))
  (let ((m (pmax pmf)))
    (loop :for xp :in (xps pmf)
          :for x = (car xp)
          :for p = (cdr xp)
          :do (if (zerop p)
                  (removex pmf (lambda (xx) (eq xx x)))
                  (assign pmf x (log (/ p m)))))
    pmf))

(defmethod exponentiate ((pmf pmf))
  (let ((m (pmax pmf)))
    (loop :for xp :in (xps pmf)
          :for x = (car xp)
          :for p = (cdr xp)
          :do (assign pmf x (exp (- p m))))
    pmf))

(defmethod assign ((pmf pmf) x &optional (p 0D0)) (setf ($ pmf x) p))

(defmethod increase ((pmf pmf) x &optional (term 1D0)) (setf ($ pmf x) (+ term ($ pmf x 0D0))))

(defmethod normalize ((pmf pmf) &optional (fraction 1D0))
  (let ((total (reduce #'+ (mapcar #'cdr (xps pmf)))))
    (when (not (zerop total))
      (let ((f (/ fraction total)))
        (maphash (lambda (x p) (setf ($ pmf x) (* f p))) (xpmap pmf))))))

(defmethod xs ((pmf pmf)) (hash-table-keys (xpmap pmf)))

(defmethod ps ((pmf pmf)) (loop :for x :in (xs pmf) :collect ($ pmf x)))

(defmethod xps ((pmf pmf))
  (let ((xps nil))
    (maphash (lambda (x p) (push (cons x p) xps)) (xpmap pmf))
    (reverse xps)))

(defmethod p ((pmf pmf) x &optional default) ($ pmf x default))

(defmethod mult ((pmf pmf) x factor) (setf ($ pmf x) (* factor ($ pmf x 0D0))))

(defmethod xmean ((pmf pmf)) (loop :for xp :in (xps pmf) :sum (* (car xp) (cdr xp))))

(defmethod xvariance ((pmf pmf))
  (let ((mu (xmean pmf)))
    (loop :for xp :in (xps pmf) :sum (* (cdr xp) (expt (- (car xp) mu) 2)))))

(defmethod xsd ((pmf pmf)) (sqrt (xvariance pmf)))

(defmethod percentile ((pmf pmf) percentage)
  (let ((p (/ percentage 100D0))
        (total 0D0))
    (loop :for xp :in (xps pmf)
          :for val = (car xp)
          :for prop = (cdr xp)
          :do (incf total prop)
          :when (>= total p)
            :return val)))

(defmethod maximum-likelihood ((pmf pmf))
  (car (reduce (lambda (xp1 xp2) (if (> (cdr xp1) (cdr xp2)) xp1 xp2)) (xps pmf))))

(defmethod credible-interval ((pmf pmf) &optional (percentage 90))
  (credible-interval (to-cdf pmf) percentage))

(defmethod to-cdf ((pmf pmf) &key &allow-other-keys)
  (let ((runsum 0D0)
        (xs nil)
        (cs nil))
    (maphash (lambda (x c)
               (incf runsum c)
               (push x xs)
               (push runsum cs))
             (xpmap pmf))
    (cdf :xs (reverse xs)
         :ps (mapcar (lambda (c) (/ c runsum)) (reverse cs)))))

(defmethod to-pmf ((pmf pmf) &key &allow-other-keys) pmf)

(defun symbol< (a b) (string< (symbol-name a) (symbol-name b)))

(defun sortxps (xps)
  (when (and xps (> (length xps) 0))
    (let* ((carx (caar xps))
           (lesspfn (cond ((symbolp carx) #'symbol<)
                          ((stringp carx) #'string<)
                          ((numberp carx) #'<))))
      (if lesspfn
          (sort xps (lambda (a b) (funcall lesspfn (car a) (car b))))
          xps))))

(defmethod plot ((pmf pmf) &key (xtics 10) &allow-other-keys)
  (plot-boxes (sortxps (xps pmf)) :xtics xtics))

(defmethod rand ((pmf pmf))
  (let ((target (random 1D0))
        (total 0D0))
    (loop :for xp :in (xps pmf)
          :for x = (car xp)
          :for p = (cdr xp)
          :do (incf total p)
          :when (>= total target)
            :return x)))

(defmethod sample ((pmf pmf) n) (loop :for i :from 0 :below n :collect (rand pmf)))

(defmethod add ((pmf pmf) (n number))
  (let ((instance (make-instance 'pmf)))
    (loop :for xp :in (xps pmf)
          :for v1 = (car xp)
          :for p1 = (cdr xp)
          :do (assign instance (+ v1 n) p1))
    instance))

(defmethod subtract ((pmf pmf) (n number))
  (let ((instance (make-instance 'pmf)))
    (loop :for xp :in (xps pmf)
          :for v1 = (car xp)
          :for p1 = (cdr xp)
          :do (assign instance (- v1 n) p1))
    instance))

(defmethod add ((pmf pmf) (other pmf))
  (let ((instance (make-instance 'pmf)))
    (loop :for xp :in (xps pmf)
          :for v1 = (car xp)
          :for p1 = (cdr xp)
          :do (loop :for xp2 :in (xps other)
                    :for v2 = (car xp2)
                    :for p2 = (cdr xp2)
                    :do (increase instance (+ v1 v2) (* p1 p2))))
    instance))

(defmethod subtract ((pmf pmf) (other pmf))
  (let ((instance (make-instance 'pmf)))
    (loop :for xp :in (xps pmf)
          :for v1 = (car xp)
          :for p1 = (cdr xp)
          :do (loop :for xp2 :in (xps other)
                    :for v2 = (car xp2)
                    :for p2 = (cdr xp2)
                    :do (increase instance (- v1 v2) (* p1 p2))))
    instance))

(defmethod maximum ((pmf pmf) k)
  (let ((cdf (to-cdf pmf)))
    (setf (ps cdf) (mapcar (lambda (p) (expt p k)) (ps cdf)))
    cdf))

(defun mixture (meta-pmf)
  (let ((mix (make-instance 'pmf)))
    (loop :for mxp :in (xps meta-pmf)
          :for pmf = (car mxp)
          :for w = (cdr mxp)
          :do (loop :for xp :in (xps pmf)
                    :for x = (car xp)
                    :for p = (cdr xp)
                    :do (increase mix x (* w p))))
    mix))

(defun mean (xs) (/ (reduce #'+ xs) (length xs)))
(defun variance (xs)
  (let* ((n (length xs))
         (m (/ (reduce #'+ xs) n)))
    (/ (reduce #'+ (mapcar (lambda (x)
                             (expt (abs (- x m)) 2))
                           xs))
       n)))
(defun sd (xs) (sqrt (variance xs)))

(defmethod copy ((pmf pmf) &key class &allow-other-keys)
  (let ((instance (make-instance (or class (type-of pmf)))))
    (loop :for xp :in (xps pmf) :do (assign instance (car xp) (cdr xp)))
    instance))

(defmethod scale ((pmf pmf) factor)
  (let ((instance (make-instance (type-of pmf))))
    (loop :for xp :in (xps pmf) :do (assign instance (* factor (car xp)) (cdr xp)))
    instance))

(defun xrange (low high &optional (step-size 1))
  (loop :for i :from low :below high :by step-size :collect i))

(defun repeat (n data) (loop :for i :from 0 :below n :collect data))

(defmethod p> ((pmf1 pmf) (pmf2 pmf))
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :sum (loop :for xp2 :in (xps pmf2)
                   :for v2 = (car xp2)
                   :for p2 = (cdr xp2)
                   :when (> v1 v2)
                     :sum (* p1 p2))))

(defmethod p> ((pmf1 pmf) v)
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :when (> v1 v)
          :sum p1))

(defmethod p>= ((pmf1 pmf) (pmf2 pmf))
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :sum (loop :for xp2 :in (xps pmf2)
                   :for v2 = (car xp2)
                   :for p2 = (cdr xp2)
                   :when (>= v1 v2)
                     :sum (* p1 p2))))

(defmethod p>= ((pmf1 pmf) v)
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :when (>= v1 v)
          :sum p1))

(defmethod p< ((pmf1 pmf) (pmf2 pmf))
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :sum (loop :for xp2 :in (xps pmf2)
                   :for v2 = (car xp2)
                   :for p2 = (cdr xp2)
                   :when (< v1 v2)
                     :sum (* p1 p2))))

(defmethod p< ((pmf1 pmf) v)
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :when (< v1 v)
          :sum p1))

(defmethod p<= ((pmf1 pmf) (pmf2 pmf))
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :sum (loop :for xp2 :in (xps pmf2)
                   :for v2 = (car xp2)
                   :for p2 = (cdr xp2)
                   :when (<= v1 v2)
                     :sum (* p1 p2))))

(defmethod p<= ((pmf1 pmf) v)
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :when (<= v1 v)
          :sum p1))

(defmethod p= ((pmf1 pmf) (pmf2 pmf))
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :sum (loop :for xp2 :in (xps pmf2)
                   :for v2 = (car xp2)
                   :for p2 = (cdr xp2)
                   :when (= v1 v2)
                     :sum (* p1 p2))))

(defmethod p= ((pmf1 pmf) v)
  (loop :for xp :in (xps pmf1)
        :for v1 = (car xp)
        :for p1 = (cdr xp)
        :when (= v1 v)
          :sum p1))

(defun uniform-pmf (&key (low 0D0) (high 1D0) (skip 0.1))
  (let ((pmf (pmf)))
    (loop :for x :in (xrange low (+ high skip) skip)
          :do (assign pmf x 1D0))
    (normalize pmf)
    pmf))

(defun removex (pmf fcondition)
  (loop :for x :in (xs pmf)
        :when (funcall fcondition x)
          :do (remhash x (xpmap pmf)))
  (normalize pmf))

(defmethod marginal ((self pmf) i)
  (let ((pmf (pmf :class (type-of self))))
    (loop :for xps :in (xps self)
          :for vs = (car xps)
          :for prob = (cdr xps)
          :do (increase pmf ($ vs i) prob))
    (normalize pmf)
    pmf))

(defmethod conditional ((self pmf) i j val)
  (let ((pmf (pmf :class (type-of self))))
    (loop :for xps :in (xps self)
          :for vs = (car xps)
          :for prob = (cdr xps)
          :when (= ($ vs j) val)
            :do (increase pmf ($ vs i) prob))
    (normalize pmf)
    pmf))

(defmethod maximum-likelihood-interval ((self pmf) &key (percentage 90))
  (let ((interval nil)
        (total 0D0)
        (pxs (mapcar (lambda (xp) (cons (cdr xp) (car xp))) (xps self))))
    (setf pxs (sort pxs (lambda (a b) (> (car a) (car b)))))
    (loop :for px :in pxs
          :for p = (car px)
          :for v = (cdr px)
          :do (incf total p)
          :do (push v interval)
          :when (>= total (/ percentage 100D0))
            :return (sort interval (lambda (a b) (< (car a) (car b)))))))

(defmethod pmax ((self pmf))
  (loop :for xp :in (xps self)
        :for p = (cdr xp)
        :maximizing p))

(defun median-inter-percentile-range (xs p)
  (let* ((cdf (to-cdf (empirical xs)))
         (median (percentile cdf 50D0))
         (alpha (/ (- 1 p) 2D0)))
    (cons median (- (x cdf (- 1 alpha)) (x cdf alpha)))))

(defun median-sigma (xs nsigma)
  (let* ((half-p(- (gsll:gaussian-p (coerce nsigma 'double-float) 1D0) 0.5))
         (median-ipr (median-inter-percentile-range xs (* 2D0 half-p)))
         (median (car median-ipr))
         (ipr (cdr median-ipr))
         (s (/ ipr 2D0 nsigma)))
    (cons median s)))

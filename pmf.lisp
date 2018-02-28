
(in-package :think-bayes)

(defgeneric update (pmf evidence))
(defgeneric likelihood (pmf evidence hypothesis))

(defgeneric assign (pmf x &optional p))
(defgeneric increase (pmf x &optional term))

(defgeneric normalize (pmf &optional fraction))

(defgeneric xs (pmf))
(defgeneric ps (pmf))
(defgeneric xps (pmf))
(defgeneric x (pmf p))
(defgeneric p (pmf x &optional default))

(defgeneric mult (pmf x factor))

(defgeneric xmean (pmf))
(defgeneric percentile (pmf percentage))
(defgeneric credible-interval (pmf &optional percentage))

(defgeneric maximum-likelihood (pmf))

(defgeneric to-cdf (pmf &key &allow-other-keys))
(defgeneric to-pmf (cdf &key &allow-other-keys))

(defclass pmf () ((xpmap :initform #{} :accessor xpmap)))

(defmethod $ ((pmf pmf) x &rest default) ($ (xpmap pmf) x (car default)))

(defmethod (setf $) (p (pmf pmf) x &rest others)
  (declare (ignore others))
  (setf ($ (xpmap pmf) x) p))

(defmethod $count ((pmf pmf)) ($count (xpmap pmf)))

(defmethod print-object ((pmf pmf) stream)
  (format stream "#<PMF ~A>" (xpmap pmf)))

(defun pmf (&key (class 'pmf) (hypotheses nil))
  (let ((instance (make-instance class)))
    (when hypotheses
      (loop :for h :in hypotheses :do (increase instance h 1))
      (normalize instance))
    instance))

(defmethod update ((pmf pmf) evidence)
  (loop :for h :in (xs pmf)
        :for l = (likelihood pmf evidence h)
        :do (mult pmf h l))
  (normalize pmf)
  pmf)

(defmethod assign ((pmf pmf) x &optional (p 0)) (setf ($ pmf x) p))

(defmethod increase ((pmf pmf) x &optional (term 1)) (setf ($ pmf x) (+ term ($ pmf x 0))))

(defmethod normalize ((pmf pmf) &optional (fraction 1.0))
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

(defmethod mult ((pmf pmf) x factor) (setf ($ pmf x) (* factor ($ pmf x 0))))

(defmethod xmean ((pmf pmf)) (loop :for xp :in (xps pmf) :sum (* (car xp) (cdr xp))))

(defmethod percentile ((pmf pmf) percentage)
  (let ((p (/ percentage 100.0))
        (total 0))
    (loop :for xp :in (xps pmf)
          :for val = (car xp)
          :for prop = (cdr xp)
          :do (incf total prop)
          :when (>= total p)
            :return val)))

(defmethod maximum-likelihood ((pmf pmf))
  (car (reduce (lambda (xp1 xp2) (if (> (cdr xp1) (cdr xp2)) xp1 xp2)) (xps pmf))))

(defmethod credible-interval ((pmf pmf) &optional (percentage 90))
  (ci (to-cdf pmf) percentage))

(defmethod to-cdf ((pmf pmf) &key &allow-other-keys)
  (let ((runsum 0.0)
        (xs nil)
        (cs nil))
    (maphash (lambda (x c)
               (incf runsum c)
               (push x xs)
               (push runsum cs))
             (xpmap pmf))
    (cdf :xs (reverse xs)
         :ps (mapcar (lambda (c) (/ c runsum)) (reverse cs)))))

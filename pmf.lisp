(in-package :think-bayes)

(defgeneric update (pmf evidence))
(defgeneric update-all (pmf evidences))
(defgeneric likelihood (pmf evidence hypothesis))

(defgeneric assign (pmf x &optional p))
(defgeneric increase (pmf x &optional term))

(defgeneric normalize (pmf &optional fraction))

(defgeneric xs (pmf))
(defgeneric ps (pmf))
(defgeneric xps (pmf))
(defgeneric x (pmf p))
(defgeneric p (pmf x &optional default))

(defgeneric add (pmf other))
(defgeneric mult (pmf x factor))

(defgeneric xmean (pmf))
(defgeneric percentile (pmf percentage))
(defgeneric credible-interval (pmf &optional percentage))

(defgeneric rand (pmf))
(defgeneric sample (pmf n))

(defgeneric maximum (pmf k))

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
      (loop :for h :in hypotheses :do (increase instance h 1D0))
      (normalize instance))
    instance))

(defun update-evidence (pmf evidence)
  (loop :for h :in (xs pmf)
        :for l = (likelihood pmf evidence h)
        :do (mult pmf h l)))

(defmethod update ((pmf pmf) evidence)
  (update-evidence pmf evidence)
  (normalize pmf))

(defmethod update-all ((pmf pmf) evidences)
  (loop :for ev :in evidences
        :do (update-evidence pmf ev))
  (normalize pmf))

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

(defun plot-pmf (pmf &key (xtics 20)) (plot-boxes (sortxps (xps pmf)) :xtics xtics))

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

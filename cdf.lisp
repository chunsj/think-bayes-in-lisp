(in-package :think-bayes)

(defclass cdf ()
  ((xs :initform nil :accessor xs)
   (ps :initform nil :accessor ps)))

(defmethod print-object ((cdf cdf) stream)
  (format stream "#<CDF [~A/~A]>" (length (xs cdf)) (length (ps cdf))))

(defun cdf (&key xs ps)
  (let ((instance (make-instance 'cdf)))
    (when (and xs ps)
      (setf (xs instance) xs
            (ps instance) ps))
    instance))

(defun bisect (xs x &key (low 0) (high nil))
  (if (< low 0)
      (error "low must be non-negative")
      (let ((l low)
            (h (or high ($count xs))))
        (loop :while (< l h)
              :for m = (floor (/ (+ l h) 2))
              :do (if (< x ($ xs m))
                      (setf h m)
                      (setf l (1+ m))))
        l)))

(defmethod x ((cdf cdf) p)
  (when (or (< p 0D0) (> p 1D0)) (error "invalid probability ~A" p))
  (cond ((zerop p) ($0 (xs cdf)))
        ((= p 1D0) ($last (xs cdf)))
        (t (let ((index (bisect (ps cdf) p)))
             (if (= p ($ (ps cdf) (1- index)))
                 ($ (xs cdf) (1- index))
                 ($ (xs cdf) index))))))

(defmethod xps ((self cdf)) (mapcar (lambda (x p) (cons x p)) (xs self) (ps self)))

(defmethod p ((cdf cdf) x &optional default)
  (declare (ignore default))
  (if (< x ($0 (xs cdf)))
      0D0
      (let ((index (bisect (xs cdf) x)))
        ($ (ps cdf) (1- index)))))

(defmethod percentile ((cdf cdf) percentage)
  (x cdf (/ percentage 100D0)))

(defmethod credible-interval ((cdf cdf) &optional (percentage 90))
  (let ((p (/ (- 1D0 (/ percentage 100D0)) 2)))
    (cons (x cdf p) (x cdf (- 1D0 p)))))

(defmethod view ((cdf cdf) &key (xtics 10) &allow-other-keys)
  (plot-boxes (sortxps (xps cdf)) :xtics xtics))

(defmethod rand ((cdf cdf)) (x cdf (random 1D0)))

(defmethod sample ((cdf cdf) n) (loop :for i :from 0 :below n :collect (rand cdf)))

(defmethod maximum ((cdf cdf) k)
  (let ((instance (make-instance 'cdf)))
    (setf (xs instance) (copy-list (xs cdf)))
    (setf (ps instance) (mapcar (lambda (p) (expt p k)) (ps cdf)))
    instance))

(defmethod copy ((cdf cdf) &key class &allow-other-keys)
  (let ((instance (make-instance (or class (type-of cdf)))))
    (setf (xs instance) (copy-list (xs cdf)))
    (setf (ps instance) (copy-list (ps cdf)))
    instance))

(defmethod scale ((cdf cdf) factor)
  (let ((instance (make-instance (type-of cdf))))
    (setf (xs instance) (mapcar (lambda (x) (* x factor)) (xs cdf)))
    (setf (ps instance) (copy-list (ps cdf)))
    instance))

(defmethod to-cdf ((cdf cdf) &key &allow-other-keys) cdf)

(defmethod to-pmf ((cdf cdf) &key &allow-other-keys)
  (let ((pmf (make-instance 'pmf))
        (prev 0D0))
    (loop :for xp :in (xps cdf)
          :for val = (car xp)
          :for prob = (cdr xp)
          :do (increase pmf val (- prob prev))
          :do (setf prev prob))
    pmf))

(defclass interpolator ()
  ((xs :initform nil :accessor xs)
   (ys :initform nil :accessor ys)))

(defgeneric y (interpolator x))

(defun interpolator (xs ys)
  (let ((instance (make-instance 'interpolator)))
    (setf (xs instance) xs
          (ys instance) ys)
    instance))

(defun interpolate (x xs ys)
  (cond ((<= x ($0 xs)) ($0 ys))
        ((>= x ($last xs)) ($last ys))
        (t (let* ((i (bisect xs x))
                  (frac (* 1D0 (/ (- x ($ xs (- i 1)))
                                  (- ($ xs i) ($ xs (- i 1)))))))
             (+ ($ ys (- i 1))
                (* frac 1D0 (- ($ ys i) ($ ys (- i 1)))))))))

(defmethod y ((interpolator interpolator) x)
  (interpolate x (xs interpolator) (ys interpolator)))

(defmethod x ((interpolator interpolator) y)
  (interpolate y (ys interpolator) (xs interpolator)))

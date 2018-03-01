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
        ($ (xs cdf) (1- index)))))

(defmethod percentile ((cdf cdf) percentage)
  (x cdf (/ percentage 100D0)))

(defmethod credible-interval ((cdf cdf) &optional (percentage 90))
  (let ((p (/ (- 1D0 (/ percentage 100D0)) 2)))
    (cons (x cdf p) (x cdf (- 1D0 p)))))

(defun plot-cdf (cdf) (plot-pmf cdf))

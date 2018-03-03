(in-package :think-bayes)

(defclass beta ()
  ((a :initform 1D0 :accessor a)
   (b :initform 1D0 :accessor b)))

(defun beta (&key (alpha 1D0) (beta 1D0))
  (let ((instance (make-instance 'beta)))
    (setf (a instance) (coerce alpha 'double-float)
          (b instance) (coerce beta 'double-float))
    instance))

(defmethod print-object ((beta beta) stream)
  (format stream "#<BETA ~A:~A>" (a beta) (b beta)))

(defmethod update ((beta beta) evidence)
  (incf (a beta) (car evidence))
  (incf (b beta) (cdr evidence)))

(defmethod xmean ((beta beta)) (/ (a beta) (+ (a beta) (b beta))))

(defmethod p ((beta beta) x &optional default)
  (declare (ignore default))
  (gsll:beta-pdf (coerce x 'double-float)
                 (coerce (a beta) 'double-float)
                 (coerce (b beta) 'double-float)))

(defmethod to-pmf ((beta beta) &key (steps 101) &allow-other-keys)
  (if (or (< (a beta) 1D0) (< (b beta) 1D0))
      (let ((cdf (to-cdf beta)))
        (to-pmf cdf))
      (let ((pmf (pmf))
            (u (- steps 1D0)))
        (loop :for i :from 0 :below steps
              :for x = (/ i u)
              :for p = (p beta x)
              :do (assign pmf x p))
        (normalize pmf)
        pmf)))

(defmethod to-cdf ((beta beta) &key (steps 101) &allow-other-keys)
  (let* ((u (- steps 1D0))
         (xs (loop :for i :from 0 :below steps :collect (/ i u)))
         (ps (loop :for x :in xs :collect (gsll:beta-p x (a beta) (b beta)))))
    (cdf :xs xs :ps ps)))

(defmethod plot ((beta beta) &key (steps 101) (xtics 20))
  (plot (to-pmf beta :steps steps) :xtics xtics))

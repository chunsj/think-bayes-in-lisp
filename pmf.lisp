(in-package :think-bayes)

(defclass pmf (distribution) ())

(defmethod print-object ((pmf pmf) stream) (call-next-method))

(defun pmf (&key (name "") (values nil))
  (distribution 'pmf :name name :values values))

(defmethod normalize ((pmf pmf) &optional (fraction 1.0))
  (when (logarithmizedp pmf)
    (error "pmf is under a log transform"))
  (let ((total (ysum pmf)))
    (if (= total 0.0)
        (error "total probability is zero")
        (let ((f (/ (float fraction) total)))
          (doxys pmf (lambda (x v) (setf ($ pmf x) (* f v))))
          total))))

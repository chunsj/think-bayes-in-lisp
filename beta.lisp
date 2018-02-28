(in-package :think-bayes)

(defclass beta ()
  ((name :initform "" :accessor name)
   (alpha :initform 1.0 :accessor a)
   (beta :initform 1.0 :accessor b)))

(defun beta (&key (a 1.0) (b 1.0))
  (let ((ni (make-instance 'beta)))
    (setf (a ni) a)
    (setf (b ni) b)
    ni))

(defmethod print-object ((beta beta) stream)
  (if (> (length (name beta)) 0)
      (format stream "~A BETA[A: ~A, B: ~A]" (name beta) (a beta) (b beta))
      (format stream "BETA[A: ~A, B: ~A]" (a beta) (b beta))))

(defmethod update ((self beta) data)
  (incf (a self) (car data))
  (incf (b self) (cdr data))
  self)

(defmethod xmean ((self beta)) (* 1.0 (/ (a self) (+ (a self) (b self)))))

(defmethod y ((self beta) x &optional default)
  (declare (ignore default))
  (clmath:beta-density x
                       (coerce (a self) 'double-float)
                       (coerce (b self) 'double-float)))

(defmethod to-cdf ((self beta) &key (name "") (steps 101) &allow-other-keys)
  (let ((xs (loop :for i :from 0 :below steps :collect (/ i (- steps 1.0)))))
    (cdf :name name :xs xs :ps (mapcar (lambda (x)
                                         (clmath:beta-cumulative x
                                                                 (coerce (a self) 'double-float)
                                                                 (coerce (b self) 'double-float)))
                                       xs))))

(defmethod to-pmf ((self beta) &key (name "") (steps 101) &allow-other-keys)
  (if (or (< (a self) 1) (< (b self) 1))
      (to-pmf (to-cdf self :name name :steps steps))
      (let* ((xs (loop :for i :from 0 :below steps :collect (/ i (- steps 1.0))))
             (ps (mapcar (lambda (x) (y self x)) xs))
             (pmf (pmf :name name)))
        (loop :for i :from 0 :below steps
              :for x = ($ xs i)
              :for p = ($ ps i)
              :do (setx pmf x p))
        (normalize pmf)
        pmf)))

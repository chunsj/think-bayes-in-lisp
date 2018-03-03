(in-package :think-bayes)

(defclass pdf () ())

(defmethod to-pmf ((pdf pdf) &key xs &allow-other-keys)
  (let ((pmf (make-instance 'pmf)))
    (loop :for x :in xs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod plot ((pdf pdf) &key xs &allow-other-keys)
  (when xs
    (plot (to-pmf pdf :xs xs))))

(defclass gaussian (pdf)
  ((mu :initform 0D0 :accessor mu)
   (sigma :initform 1D0 :accessor sigma)))

(defun gaussian (&key (mu 0D0) (sigma 1D0))
  (let ((instance (make-instance 'gaussian)))
    (setf (mu instance) mu)
    (setf (sigma instance) sigma)
    instance))

(defmethod p ((pdf gaussian) x &optional default)
  (declare (ignore default))
  (+ (mu pdf) (gsll:gaussian-pdf x (sigma pdf))))

(defun gaussian-kde-fn (samples &key h)
  (let* ((n ($count samples))
         (mn (/ (reduce #'+ samples) n))
         (var (/ (reduce #'+ (mapcar (lambda (x) (expt (abs (- x mn)) 2)) samples)) n))
         (sd (sqrt var))
         (h (or h (* 0.5 (expt (* (/ 4D0 3D0) (expt sd 5)) (/ 1D0 5D0)))))
         (cnvfn (lambda (x) (mapcar (lambda (v) (/ (- x v) h)) samples))))
    (lambda (x)
      (* (/ 1D0 (* n h))
         (reduce #'+ (mapcar (lambda (v) (gsll:gaussian-pdf v 1D0))
                             (funcall cnvfn x)))))))

(defun linspace (l h n)
  (let* ((n (max 3 n))
         (s (/ (- h l) (coerce (1- n) 'double-float))))
    (append (loop :for i :from l :below (- h s) :by s :collect i) (list h))))

(defmethod plot ((pdf gaussian) &key xs (steps 101) (xtics 5) &allow-other-keys)
  (let* ((mu (mu pdf))
         (sigma (sigma pdf))
         (ixs (or xs (linspace (- mu (* 4D0 sigma)) (+ mu (* 4D0 sigma)) (1- steps)))))
    (plot (to-pmf pdf :xs ixs) :xtics xtics)))

(defclass empirical (pdf)
  ((kde :initform nil :accessor kde)))

(defun empirical (samples &key h)
  (let ((instance (make-instance 'empirical)))
    (setf (kde instance) (gaussian-kde-fn samples :h h))
    instance))

(defmethod p ((pdf empirical) x &optional default)
  (declare (ignore default))
  (funcall (kde pdf) x))

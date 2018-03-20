(in-package :think-bayes)

(defparameter *gsll-rng* (gsll:make-random-number-generator gsll::+ranlxd2+))

(defclass pdf () ())

(defmethod to-pmf ((pdf pdf) &key xs &allow-other-keys)
  (let ((pmf (make-instance 'pmf)))
    (loop :for x :in xs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf pdf) &key xs &allow-other-keys)
  (when xs
    (view (to-pmf pdf :xs xs))))

(defclass gaussian (pdf)
  ((mu :initform 0D0 :accessor mu)
   (sigma :initform 1D0 :accessor sigma)))

(defun gaussian (&key (mu 0D0) (sigma 1D0))
  (let ((instance (make-instance 'gaussian)))
    (setf (mu instance) (coerce mu 'double-float))
    (setf (sigma instance) (coerce sigma 'double-float))
    instance))

(defmethod p ((pdf gaussian) x &optional default)
  (declare (ignore default))
  (gsll:gaussian-pdf (coerce (- x (mu pdf)) 'double-float) (sigma pdf)))

(defun gaussian-probability (x &key (mu 0D0) (sigma 1D0))
  (gsll:gaussian-pdf (coerce (- x mu) 'double-float) sigma))

(defun gaussian-random (&key (mu 0D0) (sigma 1D0))
  (+ mu (gsll:sample *gsll-rng* :gaussian :sigma (coerce sigma 'double-float))))

;; h will be specified
(defun gaussian-kde-fn (samples &key (h :silverman))
  (let* ((n ($count samples))
         (mn (/ (reduce #'+ samples) n))
         (var (/ (reduce #'+ (mapcar (lambda (x) (expt (abs (- x mn)) 2)) samples)) n))
         (sd (sqrt var))
         (h (cond ((eq h :silverman) (expt (/ (* 4D0 (expt sd 5D0)) (* 3D0 n)) (/ 1D0 5D0)))
                  ((eq h :scott) (/ (* 3.5D0 sd) (expt n (/ 1D0 3D0))))
                  (t (or h 1000D0))))
         (cnvfn (lambda (x) (mapcar (lambda (v) (* 1D0 (/ (- x v) h))) samples))))
    (lambda (x)
      (* (/ 1D0 (* n h))
         (reduce #'+ (mapcar (lambda (v) (gsll:gaussian-pdf v 1D0))
                             (funcall cnvfn x)))))))

(defun linspace (l h n)
  (let* ((n (max 3 n))
         (s (/ (- h l) (coerce (1- n) 'double-float))))
    (append (loop :for i :from l :below (- h s) :by s :collect i) (list h))))

(defmethod to-pmf ((pdf gaussian) &key xs (steps 101) &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (mu (mu pdf))
         (sigma (sigma pdf))
         (ixs (or xs (linspace (- mu (* 4D0 sigma)) (+ mu (* 4D0 sigma)) (1- steps)))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf gaussian) &key xs (steps 101) (xtics 10) &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps) :xtics xtics))

(defun gaussian-pmf (&key (mu 0D0) (sigma 1D0) (nsigma 4D0) (n 101))
  (let* ((pmf (make-instance 'pmf))
         (pdf (gaussian :mu mu :sigma sigma))
         (low (- mu (* nsigma sigma)))
         (high (+ mu (* nsigma sigma)))
         (xs (linspace low high n)))
    (loop :for x :in xs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defclass empirical (pdf)
  ((kde :initform nil :accessor kde)
   (m :initform nil :accessor xmean)
   (v :initform nil :accessor xvariance)))

(defun empirical (samples &key (h :silverman))
  (let ((instance (make-instance 'empirical)))
    (setf (kde instance) (gaussian-kde-fn samples :h h))
    (setf (xmean instance) (mean samples))
    (setf (xvariance instance) (variance samples))
    instance))

(defmethod p ((pdf empirical) x &optional default)
  (declare (ignore default))
  (funcall (kde pdf) x))

(defmethod to-pmf ((pdf empirical) &key xs (steps 101) &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (mu (xmean pdf))
         (sigma (sqrt (xvariance pdf)))
         (ixs (or xs (linspace (- mu (* 4D0 sigma)) (+ mu (* 4D0 sigma)) steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defun empirical-pmf (samples &key xs (steps 101) (h :silverman))
  (to-pmf (empirical samples :h h) :xs xs :steps steps))

(defmethod to-cdf ((pdf empirical) &key xs (steps 101) &allow-other-keys)
  (to-cdf (to-pmf pdf :xs xs :steps steps)))

(defclass poisson (pdf)
  ((l :initform 1D0 :accessor rate)))

(defun factorial (n)
  (loop :for result = 1 :then (* result i)
        :for i :from 2 :to n
        :finally (return result)))

(defun poisson (&key (rate 1D0))
  (let ((instance (make-instance 'poisson)))
    (setf (rate instance) rate)
    instance))

(defmethod p ((pdf poisson) x &optional default)
  (declare (ignore default))
  (gsll:poisson-pdf (round x) (coerce (rate pdf) 'double-float)))

(defun poisson-probability (x &key (rate 1D0))
  (gsll:poisson-pdf (round x) (coerce rate 'double-float)))

(defun poisson-random (&key (rate 1D0))
  (gsll:sample *gsll-rng* :poisson :mu (coerce rate 'double-float)))

(defmethod to-pmf ((pdf poisson) &key xs (steps 21) &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (ixs (or xs (linspace 0 (1- steps) steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf poisson) &key xs (steps 21) (xtics 10) &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps) :xtics xtics))

(defun poisson-pmf (&key (rate 1D0) (n 21)) (to-pmf (poisson :rate rate) :steps n))

(defclass exponential (poisson) ())

(defun exponential (&key (rate 1D0))
  (let ((instance (make-instance 'exponential)))
    (setf (rate instance) rate)
    instance))

(defmethod p ((pdf exponential) x &optional default)
  (declare (ignore default))
  (gsll:exponential-pdf (coerce x 'double-float) (coerce (/ 1D0 (rate pdf)) 'double-float)))

(defun exponential-probability (x &key (rate 1D0))
  (gsll:exponential-pdf (coerce x 'double-float) (coerce (/ 1D0 rate) 'double-float)))

(defun exponential-random (&key (rate 1D0))
  (gsll:sample *gsll-rng* :exponential :mu (coerce (/ 1D0 rate) 'double-float)))

(defmethod to-pmf ((pdf exponential) &key xs (steps 101) high &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (h (or high 10))
         (ixs (or xs (linspace 0 h steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf exponential) &key xs (steps 101) (xtics 10) high &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps :high high) :xtics xtics))

(defun exponential-pmf (&key (rate 1D0) (n 101) high)
  (to-pmf (exponential :rate rate) :steps n :high high))

(defclass binomial (pdf)
  ((k :initform 1D0 :accessor k)
   (n :initform 2D0 :accessor n)))

(defun binomial (&key (k 1) (n 2))
  (let ((instance (make-instance 'binomial)))
    (setf (k instance) (round k))
    (setf (n instance) (round n))
    instance))

(defmethod p ((pdf binomial) x &optional default)
  (declare (ignore default))
  (gsll:binomial-pdf (k pdf) (coerce x 'double-float) (n pdf)))

(defun binomial-probability (p &key (k 1) (n 2))
  (gsll:binomial-pdf (round k) (coerce p 'double-float) (round n)))

(defmethod to-pmf ((pdf binomial) &key xs (steps 101) &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (ixs (or xs (linspace 0 1 (1- steps)))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defun binomial-pmf (&key (k 1) (n 2) (steps 101))
  (to-pmf (binomial :k k :n n) :steps steps))

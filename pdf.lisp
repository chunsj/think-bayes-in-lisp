(in-package :think-bayes)

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
  (dnorm x :mean (mu pdf) :sd (sigma pdf)))

(defun gaussian-probability (x &key (mu 0D0) (sigma 1D0))
  (dnorm x :mean mu :sd sigma))

(defun gaussian-random (&key (mu 0D0) (sigma 1D0))
  (car (rnorm 1 :mean mu :sd sigma)))

;; h will be specified
(defun gaussian-kde-fn (samples &key (h :silverman))
  (let* ((n ($count samples))
         (mn (/ (reduce #'+ samples) n))
         (var (/ (reduce #'+ (mapcar (lambda (x) (expt (abs (- x mn)) 2)) samples)) n))
         (sd (sqrt var))
         (hv (cond ((eq h :silverman) (expt (/ (* 4D0 (expt sd 5D0)) (* 3D0 n)) (/ 1D0 5D0)))
                   ((eq h :scott) (/ (* 3.5D0 sd) (expt n (/ 1D0 3D0))))
                   (t (or (when (numberp h) h) 1000D0))))
         (cnvfn (lambda (x) (mapcar (lambda (v) (* 1D0 (/ (- x v) hv))) samples))))
    (lambda (x)
      (* (/ 1D0 (* n hv))
         (reduce #'+ (mapcar (lambda (v) (dnorm v))
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
  (dpois (round x) (rate pdf)))

(defun poisson-probability (x &key (rate 1D0))
  (dpois (round x) rate))

(defun poisson-random (&key (rate 1D0))
  (rpois rate))

(defmethod to-pmf ((pdf poisson) &key xs (steps 21) &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (ixs (or xs (linspace 0 (1- steps) steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf poisson) &key xs (steps 21) (xtics 10) &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps) :xtics xtics))

(defun poisson-pmf (&key (rate 1D0) (n 21) xs) (to-pmf (poisson :rate rate) :xs xs :steps n))

(defclass exponential (poisson) ())

(defun exponential (&key (rate 1D0))
  (let ((instance (make-instance 'exponential)))
    (setf (rate instance) rate)
    instance))

(defmethod p ((pdf exponential) x &optional default)
  (declare (ignore default))
  (dexp x :rate (rate pdf)))

(defun exponential-probability (x &key (rate 1D0))
  (dexp x :rate rate))

(defun exponential-random (&key (rate 1D0))
  (rexp rate))

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

(defun binomial-probability (p &key (k 1) (n 2))
  (dbinom (round k) (round n) p))

(defun negbinomial-probability (p &key (k 1) (n 2))
  (if (< p 1E-8)
      0D0
      (dnbinom (round k) (round n) p)))

(defclass gamma (pdf)
  ((shape :initform 1D0 :accessor k)
   (rate :initform 1D0 :accessor r)))

(defun gamma (&key (alpha 1D0) (beta 1D0))
  (let ((instance (make-instance 'gamma)))
    (setf (k instance) alpha)
    (setf (r instance) beta)
    instance))

(defmethod p ((pdf gamma) x &optional default)
  (declare (ignore default))
  (dgamma x (k pdf) :rate (r pdf)))

(defun gamma-probability (x &key (alpha 1D0) (beta 1D0))
  (dgamma x alpha :rate beta))

(defmethod to-pmf ((pdf gamma) &key xs (steps 101) high &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (h (or high 20))
         (ixs (or xs (linspace 0 h steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf gamma) &key xs (steps 101) (xtics 10) high &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps :high high) :xtics xtics))

(defun gamma-pmf (&key (alpha 1D0) (beta 1D0) (n 101) high)
  (to-pmf (gamma :alpha alpha :beta beta) :steps n :high high))

(defclass invgamma (pdf)
  ((shape :initform 1D0 :accessor k)
   (rate :initform 1D0 :accessor r)))

(defun invgamma (&key (alpha 1D0) (beta 1D0))
  (let ((instance (make-instance 'invgamma)))
    (setf (k instance) alpha)
    (setf (r instance) beta)
    instance))

(defmethod p ((pdf invgamma) x &optional default)
  (declare (ignore default))
  (if (< x 1E-8)
      0D0
      (dinvgamma x (k pdf) :rate (r pdf))))

(defun invgamma-probability (x &key (alpha 1D0) (beta 1D0))
  (if (< p 1E-8)
      0D0
      (dinvgamma x alpha :rate beta)))

(defmethod to-pmf ((pdf invgamma) &key xs (steps 101) high &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (h (or high 10))
         (ixs (or xs (linspace 0 h steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf invgamma) &key xs (steps 101) (xtics 10) high &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps :high high) :xtics xtics))

(defun invgamma-pmf (&key (alpha 1D0) (beta 1D0) (n 101) high)
  (to-pmf (invgamma :alpha alpha :beta beta) :steps n :high high))

(defclass chisq (pdf)
  ((df :initform 2D0 :accessor k)))

(defun chisq (&key (df 2D0))
  (let ((instance (make-instance 'chisq)))
    (setf (k instance) df)
    instance))

(defmethod p ((pdf chisq) x &optional default)
  (declare (ignore default))
  (dchisq x (k pdf)))

(defun chisq-probability (x &key (df 2D0))
  (dchisq x df))

(defmethod to-pmf ((pdf chisq) &key xs (steps 101) high &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (h (or high 10))
         (ixs (or xs (linspace 0 h steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf chisq) &key xs (steps 101) (xtics 10) high &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps :high high) :xtics xtics))

(defun chisq-pmf (&key (df 2D0) (n 101) high)
  (to-pmf (chisq :df df) :steps n :high high))

(defclass invchisq (pdf)
  ((df :initform 2D0 :accessor k)))

(defun invchisq (&key (df 2D0))
  (let ((instance (make-instance 'invchisq)))
    (setf (k instance) df)
    instance))

(defmethod p ((pdf invchisq) x &optional default)
  (declare (ignore default))
  (if (< x 1E-8)
      0D0
      (dinvchisq x (k pdf))))

(defun invchisq-probability (x &key (df 2D0))
  (if (< x 1E-8)
      0D0
      (dinvchisq x df)))

(defmethod to-pmf ((pdf invchisq) &key xs (steps 101) high &allow-other-keys)
  (let* ((pmf (make-instance 'pmf))
         (h (or high 1))
         (ixs (or xs (linspace 0 h steps))))
    (loop :for x :in ixs :do (assign pmf x (p pdf x)))
    (normalize pmf)
    pmf))

(defmethod view ((pdf invchisq) &key xs (steps 101) (xtics 10) high &allow-other-keys)
  (view (to-pmf pdf :xs xs :steps steps :high high) :xtics xtics))

(defun invchisq-pmf (&key (df 2D0) (n 101) high)
  (to-pmf (invchisq :df df) :steps n :high high))

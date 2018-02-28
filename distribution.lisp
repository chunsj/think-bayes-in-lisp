(in-package :think-bayes)

(defclass distribution ()
  ((name :initform "" :accessor name)
   (attrs :initform #{} :accessor attrs)
   (logp :initform nil :accessor logarithmizedp)))

(defmethod $ ((d distribution) v &rest default) ($ (attrs d) v (car default)))

(defmethod (setf $) (p (d distribution) v &rest others)
  (declare (ignore others))
  (setf ($ (attrs d) v) p))

(defmethod $count ((d distribution)) ($count (attrs d)))

(defmethod print-object ((d distribution) stream)
  (if (> (length (name d)) 0)
      (format stream "<~A>:~A" (name d) (attrs d))
      (format stream "~A" (attrs d))))

(defgeneric assign(distribution v &optional y))
(defgeneric increase (distribution v &optional term))

(defgeneric normalize (distribution &optional fraction))

(defgeneric xs (distribution))
(defgeneric xys (distribution))

(defgeneric x (distribution p))
(defgeneric y (distribution x &optional default))

(defgeneric setx (distribution x &optional y))
(defgeneric remx (distribution x))

(defgeneric ysum (distribution))
(defgeneric ymax (distribution))

(defgeneric <*> (distribution x factor))

(defgeneric xmean (distribution))

(defgeneric percentile (distribution percentage))

(defgeneric to-cdf (distribution &key name))
(defgeneric to-pmf (distribution &key name))

(defgeneric maximum-likelihood (distribution))

(defgeneric credible-interval (distribution &optional percentage))

(defgeneric initialize (distribution values))

(defun distribution (cls &key (name "") (values nil))
  (let ((ni (make-instance cls)))
    (setf (name ni) name)
    (when values
      (initialize ni values)
      (when (> ($count ni) 0)
        (normalize ni)))
    ni))

(defmethod initialize ((d distribution) other)
  (error "cannot initialize a distribution with ~A" other)
  nil)

(defmethod initialize ((d distribution) (values list))
  (loop :for v :in values :do (increase d v))
  d)

(defmethod initialize ((d distribution) (ht hash-table))
  (maphash (lambda (v p)
             (assign d v p))
           ht)
  d)

(defmethod initialize ((d distribution) (di distribution))
  (initialize d (attrs di)))

(defmethod assign ((d distribution) v &optional (y 0))
  (setf ($ d v) y)
  d)

(defmethod increase ((d distribution) v &optional (term 1))
  (setf ($ d v) (+ term ($ d v 0)))
  d)

(defmethod xs ((d distribution)) (hash-table-keys (attrs d)))

(defmethod xys ((d distribution))
  (loop :for v :in (xs d) :collect (cons v ($ d v))))

(defmethod x ((d distribution) p))

(defmethod y ((d distribution) x &optional (default 0)) ($ d x default))

(defmethod setx ((d distribution) x &optional (y 0)) (setf ($ d x) y))

(defmethod remx ((d distribution) x) (remhash x (attrs d)))

(defun doxys (d function)
  (maphash function (attrs d))
  d)

(defmethod ysum ((d distribution)) (reduce #'+ (mapcar #'cdr (xys d))))
(defmethod ymax ((d distribution)) (apply #'max (mapcar #'cdr (xys d))))

(defmethod <*> ((d distribution) x factor)
  (setf ($ d x) (* factor ($ d x 0)))
  d)

(defmethod xmean ((d distribution))
  (loop :for xy :in (xys d) :sum (* (car xy) (cdr xy))))

(defmethod percentile ((d distribution) percentage)
  (let ((p (/ percentage 100.0))
        (total 0))
    (loop :for xy :in (xys d)
          :for val = (car xy)
          :for prop = (cdr xy)
          :do (incf total prop)
          :when (>= total p)
            :return val)))

(defmethod maximum-likelihood ((d distribution))
  (car (reduce (lambda (iv rv) (if (> (cdr iv) (cdr rv)) iv rv)) (xys d))))

(defmethod credible-interval ((d distribution) &optional (percentage 90))
  (let ((cdf (to-cdf d)))
    (credible-interval cdf percentage)))

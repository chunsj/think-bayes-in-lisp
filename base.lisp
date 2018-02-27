(in-package :think-bayes)

(defclass distribution ()
  ((name :initform "" :accessor name)
   (attributes :initform #{} :accessor attributes)
   (logp :initform nil :accessor logarithmizedp)))

(defmethod $ ((d distribution) v &rest default) ($ (attributes d) v (car default)))
(defmethod (setf $) (p (d distribution) v &rest others)
  (declare (ignore others))
  (setf ($ (attributes d) v) p))
(defmethod $count ((d distribution)) ($count (attributes d)))
(defmethod print-object ((d distribution) stream) (format stream "~A" (attributes d)))

(defgeneric initialize (distribution values))
(defgeneric normalize (distribution &optional fraction))
(defgeneric scale (distribution factor))
(defgeneric logarithmized (distribution &key max))
(defgeneric exponentiated (distribution &key max))
(defgeneric assign(distribution v &optional y))
(defgeneric increase (distribution v &optional term))
(defgeneric variable-values (distribution))
(defgeneric variable-pairs (distribution))
(defgeneric remove-value (distribution v))
(defgeneric copy (distribution &key name))
(defgeneric do-attributes (distribution function))
(defgeneric attribute-total (distribution))
(defgeneric attribute-maximum (distribution))

(defgeneric add (distribution other))
(defgeneric subtract (distribution other))
(defgeneric multiply (distribution v factor))

(defgeneric make-histogram-from (data &key name))
(defgeneric make-pmf-from (data &key name))
(defgeneric make-cdf-from (data &key name))

(defun create-distribution (cls &key (name "") (values nil))
  (let ((ni (make-instance cls)))
    (setf (name ni) name)
    (when values
      (initialize ni values)
      (when (> ($count ni) 0)
        (normalize ni)))
    ni))

(defun distribution (&key (name "") (values nil))
  (create-distribution 'distribution :name name :values values))

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
  (initialize d (attributes di)))

(defmethod copy ((d distribution) &key name)
  (let ((ni (make-instance 'distribution)))
    (when name (setf (name ni) name))
    (setf (attributes ni) (copy-hash-table (attributes d)))
    ni))

(defmethod do-attributes ((d distribution) function)
  (maphash function (attributes d))
  d)

(defmethod scale ((d distribution) factor)
  (let ((ni (copy d)))
    (do-attributes ni (lambda (v p) (setf ($ ni v) (* factor p))))))

(defmethod logarithmized ((d distribution) &key max)
  (when (logarithmizedp d)
    (error "already under a log transform"))
  (let ((m (or max (attribute-maximum d)))
        (to-be-removed nil))
    (setf (logarithmizedp d) t)
    (do-attributes d (lambda (v p)
                       (cond ((= 0 p) (push v to-be-removed))
                             (t (setf ($ d v) (log (/ p m)))))))
    (loop :for v :in to-be-removed :do (remhash v (attributes d)))
    d))

(defmethod exponentiated ((d distribution) &key max)
  (when (not (logarithmizedp d))
    (error "not under a log transform"))
  (let ((m (or max (attribute-maximum d))))
    (setf (logarithmizedp d) nil)
    (do-attributes d (lambda (v p) (setf ($ d v) (exp (- p m)))))))

(defmethod variable-values ((d distribution)) (hash-table-keys (attributes d)))
(defmethod variable-pairs ((d distribution))
  (loop :for v :in (variable-values d) :collect (cons v ($ d v))))
(defmethod assign ((d distribution) v &optional (y 0))
  (setf ($ d v) y)
  d)
(defmethod increase ((d distribution) v &optional (term 1))
  (setf ($ d v) (+ term ($ d v 0)))
  d)
(defmethod multiply ((d distribution) v factor)
  (setf ($ d v) (* factor ($ d v 0)))
  d)
(defmethod remove-value ((d distribution) v)
  (remhash v (attributes d))
  d)
(defmethod attribute-total ((d distribution)) (reduce #'+ (mapcar #'cdr (variable-pairs d))))
(defmethod attribute-maximum ((d distribution)) (apply #'max (mapcar #'cdr (variable-pairs d))))

(defun to-odds (p) (if (= p 1.0) sb-ext:double-float-positive-infinity (/ p (- 1.0 p))))
(defun to-probability (o) (/ 0 (+ 1.0 o)))

(in-package :think-bayes)

(defclass monty (pmf) ())

(defun monty (hypos &key (name "") (values nil))
  (let ((ni (distribution 'monty :name name :values values)))
    (loop :for h :in hypos :do (assign ni h 1))
    (normalize ni)
    ni))

(defmethod update ((pmf monty) data)
  (loop :for h :in (xs pmf)
        :for like = (likelihood pmf data h)
        :do (<*> pmf h like))
  (normalize pmf))

(defmethod likelihood ((pmf monty) data h)
  (cond ((eq h data) 0)
        ((eq h :a) 0.5)
        (t 1.0)))

(let ((pmf (monty '(:a :b :c))))
  (update pmf :b)
  pmf)

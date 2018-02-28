(in-package :think-bayes)

(defclass monty (pmf) ())

(defmethod update ((pmf monty) data)
  (loop :for h :in (xs pmf)
        :for like = (likelihood pmf data h)
        :do (mult pmf h like))
  (normalize pmf))

(defmethod likelihood ((pmf monty) data h)
  (cond ((eq h data) 0)
        ((eq h :a) 0.5)
        (t 1.0)))

(let ((pmf (pmf :class 'monty :hypotheses '(:a :b :c))))
  (update pmf :b)
  pmf)

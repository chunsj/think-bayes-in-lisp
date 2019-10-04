(in-package :think-bayes)

(defclass monty2 (pmf) ())

(defmethod likelihood ((pmf monty2) data hypothesis)
  (cond ((eq hypothesis data) 0.0)
        ((eq hypothesis :a) 0.5)
        (t 1.0)))

(let ((suite (pmf :class 'monty2 :hypotheses '(:a :b :c))))
  (observe suite :b)
  suite)

(let ((suite (pmf :class 'monty2 :hypotheses '(:a :b :c))))
  (observe suite :b)
  (view suite))

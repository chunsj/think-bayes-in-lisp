(in-package :think-bayes)

(defclass monty2 (suite) ())

(defmethod likelihood ((pmf monty2) data hypothesis)
  (cond ((eq hypothesis data) 0.0)
        ((eq hypothesis :a) 0.5)
        (t 1.0)))

(let ((suite (suite 'monty2 '(:a :b :c))))
  (update suite :b)
  suite)

(in-package :think-bayes)

(defclass dice (pmf) ())

(defmethod likelihood ((self dice) data hypothesis)
  (if (< hypothesis data)
      0.0
      (/ 1.0 hypothesis)))

(let ((suite (pmf :class 'dice :hypotheses '(4 6 8 12 20))))
  (update suite 6)
  suite)

(let ((suite (pmf :class 'dice :hypotheses '(4 6 8 12 20))))
  (update suite 6)
  (loop :for roll :in '(6 8 7 7 5 4) :do (update suite roll))
  suite)

(in-package :think-bayes)

(defclass bias (pmf) ())

(defun bias-pmf (pmf)
  (let ((new-pmf (pmf)))
    (setf (xpmap new-pmf) (copy-hash-table (xpmap pmf)))
    (loop :for xp :in (xps pmf)
          :for x = (car xp)
          :do (mult new-pmf x x))
    (normalize new-pmf)
    new-pmf))

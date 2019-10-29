(in-package :think-bayes)

(defun flip (&optional (prob 0.5D0))
  (if (< (random 1D0) prob)
      1
      0))

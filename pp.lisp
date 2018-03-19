(in-package :think-bayes)

(defun flip (&optional (prob 0.5D0))
  (if (< (random 1D0) prob)
      1
      0))

(defun gaussian-random (&optional (mu 0D0) (sigma 1D0))
  (car (sample (gaussian-pmf :mu mu :sigma sigma) 1)))

(defun histogram (xs &key (nbins 10))
  (let ((sx (sort (copy-list xs))))))

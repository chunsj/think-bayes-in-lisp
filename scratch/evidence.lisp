(in-package :think-bayes)

;; 1. need a histogram creator
;; 2. flip
;; 3. density (which is normalized (0 to 1) histogram (related to think bayes style)
(defun flip (&optional (prob 0.5D0))
  (if (< (random 1D0) prob)
      1
      0))

(defun normal-random (&optional (mu 0D0) (sigma 1D0))
  (car (sample (gaussian-pmf :mu mu :sigma sigma) 1)))

(repeat 10 (lambda () (flip 0.1)))
(repeat 100 (lambda () (normal-random)))

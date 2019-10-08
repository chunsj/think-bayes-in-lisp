(in-package :think-bayes)

;; 1. need a histogram creator
;; 2. flip
;; 3. density (which is normalized (0 to 1) histogram (related to think bayes style)

(let ((xs (repeat 100 (lambda () (flip 0.1)))))
  (view (pmf :hypotheses xs)))

(let ((xs (repeat 100 (lambda () (gaussian-random)))))
  (view (empirical-pmf xs :steps 11)))

(view (gaussian-pmf))

(defun two-gaussian () (* (gaussian-random) (gaussian-random)))
(view (empirical-pmf (repeat 100 (lambda () (two-gaussian))) :steps 11))

(defun bend-coin (p)
  (if (eq (flip p) 1)
      (lambda () (flip 0.7))
      (lambda () (flip 0.1))))

(view (pmf :hypotheses (repeat 100 (bend-coin 0.5))))

(->> (repeat 1000 (lambda () (reduce #'+ (repeat 10 (lambda () (flip 0.8))))))
     (pmf :hypotheses)
     (view))

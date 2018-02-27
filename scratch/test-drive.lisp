(in-package :think-bayes)

(let ((pmf (pmf)))
  (loop :for i :in '(1 2 3 4 5 6) :do (assign pmf i (/ 1 6.0)))
  pmf)

(let ((pmf (pmf)))
  (assign pmf "Bowl 1" 0.5)
  (assign pmf "Bowl 2" 0.5)
  (multiply pmf "Bowl 1" 0.75)
  (multiply pmf "Bowl 2" 0.5)
  (normalize pmf)
  (probability pmf "Bowl 1"))

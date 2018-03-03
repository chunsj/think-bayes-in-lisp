(in-package :think-bayes)

(let ((mix94 #{:brown 30 :yellow 20 :red 20 :green 10 :orange 10 :tan 10})
      (mix96 #{:blue 24 :green 20 :orange 16 :yellow 14 :red 13 :brown 13}))
  (let ((hypoa #{:bag1 mix94 :bag2 mix96})
        (hypob #{:bag1 mix96 :bag2 mix94}))
    (let ((hypos #{:a hypoa :b hypob}))
      (defclass m-and-m (pmf)
        ((hypotheses :initform hypos :reader hypotheses))))))

(defmethod likelihood ((self m-and-m) data hypo)
  (let* ((bag ($0 data))
         (color ($1 data))
         (mix ($ ($ (hypotheses self) hypo) bag))
         (like ($ mix color)))
    like))

(let ((suite (pmf :class 'm-and-m :hypotheses '(:a :b))))
  (update suite '(:bag1 :yellow))
  (update suite '(:bag2 :green))
  suite)

(let ((suite (pmf :class 'm-and-m :hypotheses '(:a :b))))
  (update suite '(:bag1 :yellow))
  (update suite '(:bag2 :green))
  (plot suite))

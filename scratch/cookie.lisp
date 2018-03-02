(in-package :think-bayes)

(defclass cookie (pmf) ())

(defmethod update ((cookie cookie) data)
  (loop :for h :in (xs cookie)
        :for like = (likelihood cookie data h)
        :do (mult cookie h like))
  (normalize cookie)
  cookie)

(defmethod likelihood ((cookie cookie) data hypothesis)
  (let* ((mixes #{"Bowl 1" #{:vanilla 0.75 :chocolate 0.25}
                  "Bowl 2" #{:vanilla 0.5 :chocolate 0.5}})
         (mix ($ mixes hypothesis)))
    ($ mix data)))

(let ((pmf (pmf :class 'cookie :hypotheses '("Bowl 1" "Bowl 2"))))
  (update pmf :vanilla)
  pmf)

(let ((pmf (pmf :class 'cookie :hypotheses '("Bowl 1" "Bowl 2"))))
  (loop :for data :in '(:vanilla :chocolate :vanilla) :do (update pmf data))
  pmf)

(let ((pmf (pmf :class 'cookie :hypotheses '("Bowl 1" "Bowl 2"))))
  (update pmf :vanilla)
  (plot-pmf pmf))

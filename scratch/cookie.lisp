(in-package :think-bayes)

(defclass cookie (pmf) ())

(defun cookie (hypos &key (name "") (values nil))
  (let ((ni (create-distribution 'cookie :name name :values values)))
    (loop :for h :in hypos :do (set-value ni h 1))
    (normalize ni)
    ni))

(defmethod update ((cookie cookie) data)
  (loop :for h :in (variable-values cookie)
        :for like = (likelihood cookie data h)
        :do (multiply cookie h like))
  (normalize cookie)
  cookie)

(defmethod likelihood ((cookie cookie) data hypothesis)
  (let* ((mixes #{"Bowl 1" #{:vanilla 0.75 :chocolate 0.25}
                  "Bowl 2" #{:vanilla 0.5 :chocolate 0.5}})
         (mix ($ mixes hypothesis)))
    ($ mix data)))

(let ((pmf (cookie '("Bowl 1" "Bowl 2"))))
  (update pmf :vanilla)
  pmf)

(let ((pmf (cookie '("Bowl 1" "Bowl 2"))))
  (loop :for data :in '(:vanilla :chocolate :vanilla) :do (update pmf data))
  pmf)

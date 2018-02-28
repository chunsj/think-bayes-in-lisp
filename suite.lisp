(in-package :think-bayes)

(defclass suite (pmf) ())

(defgeneric update (distribution data))
(defgeneric likelihood (distribution data hypothesis))

(defun suite (cls hypotheses &key (name "") (values nil))
  (let ((ni (distribution cls :name name :values values)))
    (loop :for h :in hypotheses :do (setx ni h 1))
    (normalize ni)
    ni))

(defmethod update ((pmf suite) data)
  (loop :for h :in (xs pmf)
        :for l = (likelihood pmf data h)
        :do (<*> pmf h l))
  (normalize pmf))

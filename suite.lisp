(in-package :think-bayes)

(defclass suite (pmf) ())

(defgeneric update (distribution data))
(defgeneric update-set (distribution dataset))
(defgeneric log-update (distribution data))
(defgeneric log-update-set (distribution dataset))
(defgeneric likelihood (distribution data hypothesis))
(defgeneric log-likelihood (distribution data hypothesis))
(defgeneric convert-to-odds (distribution))
(defgeneric convert-to-probabilities (distribution))

(defun create-suite (cls hypotheses &key (name "") (values nil))
  (let ((ni (create-distribution cls :name name :values values)))
    (loop :for h :in hypotheses :do (set-value ni h 1))
    (normalize ni)
    ni))

(defmethod update ((pmf suite) data)
  (loop :for h :in (variable-values pmf)
        :for l = (likelihood pmf data h)
        :do (multiply pmf h l))
  (normalize pmf))

(defmethod log-update ((pmf suite) data)
  (loop :for h :in (variable-values pmf)
        :for l = (log-likelihood pmf data h)
        :do (increase pmf h l))
  pmf)

(defmethod update-set ((pmf suite) dataset)
  (loop :for data :in dataset
        :do (loop :for h :in (variable-values pmf)
                  :for l = (likelihood pmf data h)
                  :do (multiply pmf h l)))
  (normalize pmf))

(defmethod log-update-set ((pmf suite) dataset)
  (loop :for data :in dataset :do (log-update pmf data))
  pmf)

(defmethod convert-to-odds ((pmf suite))
  (loop :for pair :in (variable-pairs pmf)
        :for hypo = (car pair)
        :for prob = (cdr pair)
        :do (if (= prob 0)
                (remove-value pmf hypo)
                (set-value pmf hypo (to-odds prob)))))

(defmethod convert-to-probabilities ((pmf suite))
  (do-attributes pmf (lambda (h o) (set-value pmf h (to-probability o)))))

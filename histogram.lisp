(in-package :think-bayes)

(defclass histogram (distribution) ())

(defmethod print-object ((h histogram) stream) (call-next-method))

(defgeneric frequency (histogram x))
(defgeneric frequencies (histogram xs))
(defgeneric subp (histogram other))

(defmethod frequency ((h histogram) x) ($ h x 0))
(defmethod frequencies ((h histogram) xs) (loop :for x :in xs :collect (frequency h x)))
(defmethod subp ((h histogram) (o histogram))
  (and (loop :for pair :in (variable-pairs h)
             :for val = (car pair)
             :for freq = (cdr pair)
             :when (> freq (frequency o val))
               :return nil
             :finally (return t))
       t))
(defmethod subtract ((h histogram) (o histogram))
  (do-attributes o (lambda (val freq) (increase h val (* -1 freq))))
  h)

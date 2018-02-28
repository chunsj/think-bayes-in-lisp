(in-package :think-bayes)

(defclass euro (pmf) ())

(defmethod likelihood ((self euro) data hypo)
  (let ((x (/ hypo 100D0)))
    (if (eq data :h) x (- 1D0 x))))

(let ((suite (pmf :class 'euro :hypotheses (loop :for i :from 1 :to 100 :collect i)))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  (tb.plot::gnuplot-pmf suite))

(let ((suite (pmf :class 'euro :hypotheses (loop :for i :from 1 :to 100 :collect i)))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (update-all suite dataset)
  (tb.plot::gnuplot-pmf suite))

(let ((suite (pmf :class 'euro :hypotheses (loop :for i :from 0 :to 100 :collect i)))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  #{:maximum-likelihood (maximum-likelihood suite)
    :mean (xmean suite)
    :median (percentile suite 50)
    :credible-interval (credible-interval suite)})

(defun triangle-prior ()
  (let ((suite (pmf :class 'euro)))
    (loop :for i :from 1 :to 50 :do (assign suite i i))
    (loop :for i :from 51 :to 100 :do (assign suite i (- 100 i)))
    (normalize suite)
    suite))

(tb.plot::gnuplot-pmf (triangle-prior))

(let ((suite (triangle-prior))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  (tb.plot::gnuplot-pmf suite))

(let ((suite (triangle-prior))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (update-all suite dataset)
  (tb.plot::gnuplot-pmf suite))

;; faster one but overflows XXX
(defmethod likelihood ((self euro) data hypo)
  (let ((x (/ hypo 100D0))
        (heads (car data))
        (tails (cdr data)))
    (cond ((= x 0) 0D0)
          ((= x 1) 0D0)
          (t (* (expt x heads) (expt (- 1 x) tails))))))

(let ((suite (triangle-prior)))
  (update suite (cons 140 110))
  (tb.plot::gnuplot-pmf suite))

;; todo - should i need to use gsll?
(let ((beta (beta)))
  (update beta (cons 140 110))
  (xmean beta))

(let ((beta (beta)))
  (update beta (cons 140 110))
  (let ((pmf (to-pmf beta)))
    (gnuplot-lines (mapcar #'cdr (xys pmf)))))

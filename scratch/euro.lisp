(in-package :think-bayes)

(defclass euro (suite) ())

(defmethod likelihood ((self euro) data hypo)
  (let ((x (/ hypo 100.0)))
    (if (eq data :h) x (- 1.0 x))))

(let ((suite (suite 'euro (loop :for i :from 1 :to 100 :collect i)))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  (gnuplot-distribution suite))

(let ((suite (suite 'euro (loop :for i :from 0 :to 100 :collect i)))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  #{:maximum-likelihood (maximum-likelihood suite)
    :mean (xmean suite)
    :median (percentile suite 50)
    :credible-interval (credible-interval suite)})

(defun triangle-prior ()
  (let ((suite (make-instance 'euro)))
    (loop :for i :from 1 :to 50 :do (setx suite i i))
    (loop :for i :from 51 :to 100 :do (setx suite i (- 100 i)))
    (normalize suite)
    suite))

(gnuplot-distribution (triangle-prior))

(let ((suite (triangle-prior))
      (dataset (append (loop :for i :from 0 :below 140 :collect :h)
                       (loop :for i :from 0 :below 110 :collect :t))))
  (loop :for data :in dataset :do (update suite data))
  (gnuplot-distribution suite))

;; faster one but overflows XXX
(defmethod likelihood ((self euro) data hypo)
  (let ((x (/ hypo 100D0))
        (heads (car data))
        (tails (cdr data)))
    (cond ((= x 0) 0)
          ((= x 1) 0)
          (t (exp (+ (* heads (log x)) (* tails (log (- 1 x)))))))))

(let ((suite (triangle-prior)))
  (update suite (cons 140 110))
  (gnuplot-distribution suite))

(let ((beta (beta)))
  (update beta (cons 140 110))
  (xmean beta))

(let ((beta (beta)))
  (update beta (cons 140 110))
  (let ((pmf (to-pmf beta)))
    (gnuplot-lines (mapcar #'cdr (xys pmf)))))

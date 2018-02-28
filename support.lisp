(in-package :think-bayes)

(defparameter *xt-gnuplot-debug* nil)
(defparameter *xt-gnuplot-emacs-file* "/tmp/emacs-chart.svg")

(defmacro with-gnuplot (&body body)
  `(with-plots (*standard-output* :debug *xt-gnuplot-debug*)
     (gp-setup :terminal '(:svg :enhanced :background :rgb "white")
               :tics '(:font "Candara,10" :scale 0.5)
               :key '(:font "Candara,10")
               :output *xt-gnuplot-emacs-file*)
     (gp :set :grid)
     ,@body))

(defun gnuplot-points (pts)
  (when (and pts (> (length pts) 1))
    (with-gnuplot
      (gp :unset :key)
      (gp :set :xrange (list (format nil "-1:~A" (1+ (length pts)))))
      (plot (lambda ()
              (loop :for pt :in pts :do (format T "~&~{~A~^ ~}" pt)))
            :with '(points)))))

(defun gnuplot-xys (xys)
  (when (and xys (> (length xys) 1))
    (with-gnuplot
      (gp :unset :key)
      (gp :set :xrange (list (format nil "-1:~A" (1+ (length xys)))))
      (plot (lambda ()
              (loop :for xy :in xys :do (format T "~&~{~A~^ ~}" (list (car xy) (cdr xy)))))
            :with '(points)))))

(defun gnuplot-distribution (d) (gnuplot-xys (xys d)))

(defun gnuplot-lines (vs)
  (when (and vs (> (length vs) 1))
    (with-gnuplot
      (gp :unset :key)
      (gp :set :xrange (list (format nil "-1:~A" (1+ (length vs)))))
      (plot (lambda ()
              (loop :for v :in vs :do (format T "~&~A" v)))
            :with '(lines)))))

(in-package :think-bayes)

(defun datafn ()
  (concatenate 'string "/Users/Sungjin/Documents/Lisp/think-bayes/scratch/"
               "light.txt"))

(defparameter *light-data* (read-lines-from (datafn) :txfn (lambda (line) (parse-integer line))))

(coerce (mean *light-data*) 'double-float)
(coerce (variance *light-data*) 'double-float)
(sd *light-data*)

(in-package :think-bayes)

(defun read-showcase-data (fname)
  (let ((lines (read-lines-from fname)))
    #{:showcase1 (mapcar #'parse-integer (cdr (split #\, ($3 lines))))
      :showcase2 (mapcar #'parse-integer (cdr (split #\, ($4 lines))))
      :bid1 (mapcar #'parse-integer (cdr (split #\, ($6 lines))))
      :bid2 (mapcar #'parse-integer (cdr (split #\, ($7 lines))))
      :difference1 (mapcar #'parse-integer (cdr (split #\, ($9 lines))))
      :difference2 (mapcar #'parse-integer (cdr (split #\, ($ lines 10))))}))

(defun read-showcase-data2 (fname)
  (let ((lines (read-lines-from fname)))
    #{:showcase1 (mapcar #'parse-integer (cdr (split #\, ($2 lines))))
      :showcase2 (mapcar #'parse-integer (cdr (split #\, ($3 lines))))
      :bid1 (mapcar #'parse-integer (cdr (split #\, ($5 lines))))
      :bid2 (mapcar #'parse-integer (cdr (split #\, ($6 lines))))
      :difference1 (mapcar #'parse-integer (cdr (split #\, ($8 lines))))
      :difference2 (mapcar #'parse-integer (cdr (split #\, ($9 lines))))}))

(defparameter *showcase2011* (read-showcase-data
                              "/Users/Sungjin/Documents/Python/ThinkBayes/code/showcases.2011.csv"))
(defparameter *showcase2012* (read-showcase-data2
                              "/Users/Sungjin/Documents/Python/ThinkBayes/code/showcases.2012.csv"))

(defun linspace (l h n)
  (let* ((n (max 3 n))
         (s (/ (- h l) (coerce (1- n) 'double-float))))
    (append (loop :for i :from l :below (- h s) :by s :collect i) (list h))))

(-> (to-pmf (empirical ($ *showcase2011* :showcase1))
            :xs (linspace 0 75000 101))
    (plot-pmf :xtics 10))

(-> (to-pmf (empirical ($ *showcase2012* :showcase1))
            :xs (linspace 0 75000 101))
    (plot-pmf :xtics 10))

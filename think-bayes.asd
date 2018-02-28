(defsystem think-bayes
  :name "think-bayes"
  :author "Sungjin Chun <chunsj@gmail.com>"
  :version "0.1"
  :maintainer "Sungjin Chun <chunsj@gmail.com>"
  :description "support code for the book think bayes"
  :long-description "read description"
  :depends-on ("mu"
               "eazy-gnuplot")
  :components ((:file "package")
               (:file "distribution")
               (:file "pmf")
               (:file "suite")
               (:file "support")))

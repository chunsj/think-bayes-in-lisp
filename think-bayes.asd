(defsystem think-bayes
  :name "think-bayes"
  :author "Sungjin Chun <chunsj@gmail.com>"
  :version "0.1"
  :maintainer "Sungjin Chun <chunsj@gmail.com>"
  :description "support code for the book think bayes"
  :long-description "read description"
  :depends-on ("mu"
               "mplot"
               "gsll")
  :components ((:file "package")
               (:file "pmf")
               (:file "cdf")
               (:file "beta")
               (:file "pdf")))

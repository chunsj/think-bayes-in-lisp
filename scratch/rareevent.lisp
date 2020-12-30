(in-package :think-bayes)

;; a small random sample of 20 individuals will be checked for infection
;; the fraction of infected individuals are not known, however, if we assume
;; the rate as 0.05 or 0.10 or 0.20, then the distribution will be...
(view (binomial-pmf :n 20 :p 0.05))
(view (binomial-pmf :n 20 :p 0.10))
(view (binomial-pmf :n 20 :p 0.20))

;; the probability that there's no infection
(p (binomial-pmf :n 20 :p 0.05) 0) ;; 36%
(p (binomial-pmf :n 20 :p 0.10) 0) ;; 12%
(p (binomial-pmf :n 20 :p 0.20) 0) ;; 1%

;; the beta distribution - will be a prior
(view (beta-pmf :alpha 2 :beta 20))
(p (to-cdf (beta :alpha 2 :beta 20)) 0.10) ;; rate < 0.10 is 64%
(xmean (beta-pmf :alpha 2 :beta 20)) ;; mean is 0.09

;; the posterior
(view (beta-pmf :alpha 2 :beta 40))
(xmean (beta-pmf :alpha 2 :beta 40)) ;; posterior mean value of the rate
(p (to-cdf (beta :alpha 2 :beta 40)) 0.10) ;; rate < 0.10|Y=0 is 93%

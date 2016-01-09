;;;; bayes-filter.asd

(asdf:defsystem #:bayes-filter
  :description "Simple Bayesian filtering."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:bayes-filter/bayes-filter))

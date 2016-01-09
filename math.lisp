(defpackage :bayes-filter/math
  (:use :cl :alexandria :serapeum)
  (:export :fisher))
(in-package :bayes-filter/math)

(declaim (optimize (speed 3) (safety 1) (debug 0)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
         for i below (/ degrees-of-freedom 2)
         for prob = (exp (- m)) then (* prob (/ m i))
         summing prob)
   1.0))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson."
  (inverse-chi-square
   (* -2 (loop for prob in probs sum (log prob)))
   (* 2 number-of-probs)))

This is a mostly-trivial implementation of the algorithm for creating
Bayes filters from [Practical Common Lisp][PCL].

``` lisp
(def db ()
  #.(lret ((db (bayes-filter:make-db :extract-features-by 'extract-url-ngrams)))
          (loop for (url . article?) in (training-data)
                for type = (if article? :yes :no)
                do (bayes-filter:train db url type))))

(defun classify-link (link)
  (bayes-filter:classify db link :min-score 0.6))

(defun article-link? (link)
  (let ((class (classify-link link)))
    (values (eql class :yes) class)))
```

The above illustrates one non-trivial feature of this implementation:
the database is serializable and can be embedded directly into FASLs.

[PCL]: http://www.gigamonkeys.com/book/

;;;; bayes-filter.lisp

(defpackage :bayes-filter/bayes-filter
  (:nicknames :bayes-filter)
  (:use :cl :alexandria :serapeum :bayes-filter/math)
  (:export :make-db :clear-db :train :classify))

(in-package #:bayes-filter)

;;; "bayes-filter" goes here. Hacks and glory await!

(deftype classification ()
  '(member :yes :no :unsure))

(deftype probability ()
  '(float 0d0 1d0))

(defstruct db
  (dict (dict) :type hash-table :read-only t)
  (total-spams 0)
  (total-hams 0)
  (max-ham-score 0.4 :type probability :read-only t)
  (min-spam-score 0.9 :type probability :read-only t)
  (extract-features-by #'words :type (or function symbol) :read-only t))

(defun clear-db (db)
  (prog1 db
    (clrhash (db-dict db))
    (setf (db-total-spams db) 0
          (db-total-hams db) 0)))

(defstruct (feature (:constructor make-feature (string)))
  (string (error "No feature") :type string :read-only t)
  (spam-count 0 :type unsigned-byte)
  (ham-count 0 :type unsigned-byte))

(defun classify (db text)
  (~>> text
       (extract-features db)
       (score db)
       (classify-score db)))

(defun classify-score (db score)
  (values
   (cond
     ((<= score (db-max-ham-score db)) :no)
     ((>= score (db-min-spam-score db)) :yes)
     (t :unsure))
   score))

(defun intern-feature (db feature)
  (ensure2 (gethash feature (db-dict db))
    (make-feature feature)))

(defun extract-features (db text)
  ;; Delete duplicates, or not?
  (mapcar (op (intern-feature db _))
          (funcall (db-extract-features-by db) text)))

(defun train (db text type)
  (prog1 db
    (dolist (feature (extract-features db text))
      (increment-count feature type))
    (increment-total-count db type)))

(defun increment-count (feature type)
  (ecase-of classification type
    (:no (incf (feature-ham-count feature)))
    (:yes (incf (feature-spam-count feature)))
    (:unsure (error "This shouldn't happen."))))

(defun increment-total-count (db type)
  (ecase-of classification type
    (:no (incf (db-total-hams db)))
    (:yes (incf (db-total-spams db)))
    (:unsure (error "This shouldn't happen."))))

(defun spam-probability (db feature)
  (let* ((spam-count     (feature-spam-count feature))
         (ham-count      (feature-ham-count feature))
         (spam-frequency (/ spam-count (max 1 (db-total-spams db))))
         (ham-frequency  (/ ham-count  (max 1 (db-total-hams db)))))
    (/ spam-frequency (+ spam-frequency ham-frequency))))

(defun bayesian-spam-probability (db feature &key
                                               (assumed-probability 1/2)
                                               (weight 1))
  (let ((basic-probability (spam-probability db feature))
        (data-points (+ (feature-spam-count feature)
                        (feature-ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (db features)
  (let ((spam-probs ()) (ham-probs ()) (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained? feature)
        (let ((spam-prob (float (bayesian-spam-probability db feature) 0d0)))
          (push spam-prob spam-probs)
          (push (- 1d0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2d0))))

(defun untrained? (feature)
  (and (zerop (feature-spam-count feature))
       (zerop (feature-ham-count feature))))

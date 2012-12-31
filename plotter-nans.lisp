
(in-package :plotter)

;; ------------------------------------------
;; infinitep true if non-zero numeric arg with zero reciprocal
;; works for plus or minus infinity. As a secondary effect,
;; the truth value will be the largest double precision value.
(defun infinitep (v)
  (and (not (zerop v))
       (zerop (/ v))
       (if (plusp v)
           most-positive-double-float
         most-negative-double-float)))

;; nanp true if numeric v not equal to itself
(defun nanp (v)
  (/= v v))

(defun inf-nan-p (v)
  (or (infinitep v)
      (nanp v)))

(defun simple-real-number (v)
  (and (realp v)
       (not (inf-nan-p v))))

(defun real-eval-with-nans (fn &rest args)
  (handler-case
      (let ((v (apply fn args)))
        (if (simple-real-number v)
            v
          :nan))
    (arithmetic-error (err)
      (declare (ignore err))
      :nan)))

(defun nan-or-infinite-p (v)
  (not (simple-real-number v)))

;; ---------------------------------------------
;; filtering out nans and infinities
;;
(defmethod filter-x-y-nans-and-infinities ((xs cons) (ys cons))
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x in xs
                     for y in ys
                     when (and (simple-real-number x)
                               (simple-real-number y))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-x-y-nans-and-infinities ((xs vector) (ys vector))
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x across xs
                     for y across ys
                     when (and (simple-real-number x)
                               (simple-real-number y))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-x-y-nans-and-infinities (xs ys)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (filter-x-y-nans-and-infinities (coerce-to-vector xs) (coerce-to-vector ys)))


(defun filter-nans-and-infinities (xs)
  ;; remove values from the sequence if they are nans or infinities
  (remove-if (complement #'simple-real-number) xs))

;; ----------------------------------------------------------------------
;; filter out potential nans and infinities for logarithmic axes
(defun acceptable-value (v islog)
  (and (simple-real-number v)
       (or (not islog)
           (and islog
                (plusp v)))))

(defmethod filter-potential-x-y-nans-and-infinities ((xs cons) (ys cons) xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x in xs
                     for y in ys
                     when (and (acceptable-value x xlog)
                               (acceptable-value y ylog))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-potential-x-y-nans-and-infinities ((xs vector) (ys vector) xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (let ((pairs (loop for x across xs
                     for y across ys
                     when (and (acceptable-value x xlog)
                               (acceptable-value y ylog))
                     collect (list x y))))
    (values (mapcar #'first  pairs)
            (mapcar #'second pairs))
    ))

(defmethod filter-potential-x-y-nans-and-infinities (xs ys xlog ylog)
  ;; remove paired values if either of the (x,y) pair is nan or infinite
  (filter-x-y-nans-and-infinities (coerce-to-vector xs) (coerce-to-vector ys)))


(defun filter-potential-nans-and-infinities (xs islog)
  ;; remove values from the sequence if they are nans or infinities
  (remove-if (complement (um:rcurry #'acceptable-value islog)) xs))


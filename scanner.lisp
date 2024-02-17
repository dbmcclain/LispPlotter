
(in-package :plotter)

;;-------------------------------------------------------------------
;; Abstract superclass <scanner> represent objects that respond to the NEXT-ITEM method
;;

(defclass <scanner> ()
  ())

(defclass <limited-scanner> (<scanner>)
  ((limit  :accessor scanner-limit    :initarg :limit)
   (pos    :accessor scanner-position :initform 0)))

(defclass <counting-scanner> (<limited-scanner>)
  ())

(defclass <vector-scanner> (<limited-scanner>)
  ((vec  :accessor scanner-vector :initarg :vector)))

(defclass <list-scanner> (<limited-scanner>)
  ((lst        :accessor scanner-list :initarg :list)
   (lst-backup :accessor scanner-list-backup)))

(defclass <array-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

(defclass <carray-scanner> (<limited-scanner>)
  ((arr  :accessor scanner-array :initarg :array)))

;; ===============

(defmethod make-scanner ((limit integer) &key (max-items limit))
  (make-instance '<counting-scanner>
                 :limit (min limit max-items)))

(defmethod make-scanner ((vec vector) &key (max-items (length vec)))
  (make-instance '<vector-scanner>
                 :limit   (min (length vec) max-items)
                 :vector  vec))

(defmethod make-scanner ((lst list) &key (max-items (length lst)))
  (make-instance '<list-scanner>
                 :list  lst
                 :limit (min (length lst) max-items)))

(defmethod initialize-instance :after ((self <list-scanner>)
                                       &rest args &key &allow-other-keys)
  (setf (scanner-list-backup self) (scanner-list self)))

(defmethod make-scanner ((arr array) &key (max-items (array-total-size arr)))
  (make-instance '<array-scanner>
                 :array  arr
                 :limit  (min (array-total-size arr) max-items)))

(defmethod make-scanner ((arr ca:<carray>) &key (max-items (ca:carray-total-size arr)))
  (make-instance '<carray-scanner>
                 :array  arr
                 :limit  (min (ca:carray-total-size arr) max-items)))

;; ===============

(defmethod reset-scanner ((scanner <limited-scanner>))
  (setf (scanner-position scanner) 0))

(defmethod reset-scanner :after ((scanner <list-scanner>))
  (setf (scanner-list scanner) (scanner-list-backup scanner)))

;; ===============
;; With an eye toward parallel processing...

(defmethod copy-scanner ((scanner <scanner>))
  (clos:copy-standard-object scanner))

(defmethod make-scanner ((scanner <limited-scanner>) &key max-items)
  (let* ((new-scanner  (copy-scanner scanner))
         (old-limit    (scanner-limit scanner))
         (new-limit    (min (or max-items old-limit)
                            old-limit)))
    (setf (scanner-limit new-scanner) new-limit)
    (reset-scanner new-scanner)
    new-scanner))

(defmethod set-position ((scanner <limited-scanner>) pos)
  (let* ((new-pos (min pos (scanner-limit scanner))))
    (setf (scanner-position scanner) new-pos)
    ))

(defmethod set-position :after ((scanner <list-scanner>) pos)
  (declare (ignore pos))
  (setf (scanner-list scanner) (nthcdr (scanner-position scanner)
                                       (scanner-list-backup scanner))))

;; ===============
;; All scanners pass through NIL as the terminal value
(defmethod next-item ((cscanner <counting-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )) cscanner
    (let ((ans position))
      (when (< ans limit)
        (incf position)
        ans)
      )))

(defmethod next-item ((lscanner <list-scanner>))
  (with-accessors ((limit    scanner-limit   )
                   (position scanner-position)
                   (its-list scanner-list    )) lscanner
    (when (< position limit)
      (incf position)
      (pop its-list))
    ))

(defmethod next-item ((vscanner <vector-scanner>))
  (with-accessors ((position   scanner-position)
                   (limit      scanner-limit   )
                   (its-vector scanner-vector  )) vscanner
  
  (when (< position limit)
    (prog1
        (aref its-vector position)
      (incf position)))
  ))

(defmethod next-item ((ascanner <array-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) ascanner
    (when (< position limit)
      (prog1
          (row-major-aref its-array position)
        (incf position)))
    ))

(defmethod next-item ((cascanner <carray-scanner>))
  (with-accessors ((position  scanner-position)
                   (limit     scanner-limit   )
                   (its-array scanner-array   )) cascanner
    (when (< position limit)
      (prog1
          (ca:row-major-caref its-array position)
        (incf position)))
    ))

;; ===============
(defclass <transformer> (<scanner>)
  ((src   :accessor transformer-source  :initarg :source)
   (xform :accessor transformer-xform   :initarg :xform)))

(defmethod make-transformer ((src <scanner>) (xform function))
  (make-instance '<transformer>
                 :source src
                 :xform  xform))

(defmethod next-item ((xf <transformer>))
  ;; pass along NIL as a terminal value
  (with-accessors  ((source   transformer-source)
                    (xform    transformer-xform )) xf
    (let ((item (next-item source)))
      (when item
        (funcall xform item)))
    ))

(defmethod reset-scanner ((xf <transformer>))
  (reset-scanner (transformer-source xf)))

;; ===============

(defclass <pair-scanner> (<scanner>)
  ((xsrc   :accessor pair-scanner-xsrc   :initarg :xsrc)
   (ysrc   :accessor pair-scanner-ysrc   :initarg :ysrc)
   (pair   :accessor pair-scanner-values :initform (make-array 2))
   ))

(defmethod make-pair-scanner ((xs <scanner>) (ys <scanner>))
  (make-instance '<pair-scanner>
                 :xsrc  xs
                 :ysrc  ys
                 ))

(defmethod next-item ((pairs <pair-scanner>))
  (with-accessors ((xs    pair-scanner-xsrc  )
                   (ys    pair-scanner-ysrc  )
                   (pair  pair-scanner-values)) pairs
    (let* ((x (next-item xs))
           (y (next-item ys)))
      (when (and x y)
        (setf (aref pair 0) x
              (aref pair 1) y)
        pair))
    ))

(defmethod reset-scanner ((pairs <pair-scanner>))
  (reset-scanner (pair-scanner-xsrc pairs))
  (reset-scanner (pair-scanner-ysrc pairs)))

;; -----------------------------------------------------
;; In order to allow for line thickness scaling, we need to plot
;; things in pixel space. Hence we need to pre-transform data coords
;; to pixel coords using a GP:TRANSFORM-POINT.

(defclass <gpxform-pair-scanner> (<pair-scanner>)
  ((gpxform  :accessor pair-scanner-gpxform :initarg :gpxform)))

(defmethod make-gpxform-pair-scanner (xform (xs <scanner>) (ys <scanner>))
  (make-instance '<gpxform-pair-scanner>
                 :xsrc    xs
                 :ysrc    ys
                 :gpxform xform
                 ))

(defmethod make-gpxform-pairs (xform (pairs <pair-scanner>))
  (make-instance '<gpxform-pair-scanner>
                 :xsrc  (pair-scanner-xsrc pairs)
                 :ysrc  (pair-scanner-ysrc pairs)
                 :gpxform xform))

(defmethod next-item ((pairs <gpxform-pair-scanner>))
  (with-accessors ((xs      pair-scanner-xsrc  )
                   (ys      pair-scanner-ysrc  )
                   (pair    pair-scanner-values)
                   (gpxform pair-scanner-gpxform)) pairs
    (let* ((x  (next-item xs))
           (y  (next-item ys)))
      (when (and x y)
        (multiple-value-bind (xt yt)
            (gp:transform-point gpxform x y)
          (setf (aref pair 0) xt
                (aref pair 1) yt)
          pair)
        ))
    ))
           
;; -----------------------------------------------------

(defmethod do-with-pairs ((pairs <pair-scanner>) fn)
  (reset-scanner pairs)
  (loop for pair = (next-item pairs)
        while pair
        do
        (funcall fn (aref pair 0) (aref pair 1))))

(defmacro with-pairs ((pairs x y) &body body)
  `(do-with-pairs ,pairs
                  (lambda (,x ,y)
                    ,@body)))


(defmethod do-with-scanner ((scanner <scanner>) fn)
  (reset-scanner scanner)
  (loop for x = (next-item scanner)
        while x
        do
        (funcall fn x)))

(defmacro with-scanner ((scanner x) &body body)
  `(do-with-scanner ,scanner
                    (lambda (,x)
                      ,@body)))

;; ----------------------------------------------------------
;; For transformed points, it is more efficient to transform them in
;; bulk.

(defmethod collect-pairs ((pairs <pair-scanner>))
  (reset-scanner pairs)
  (let (xprev yprev)
    (loop for pair = (next-item pairs)
          while pair
          when (or (null xprev)
                   ;; weed out adjacent duplicate points
                   (/= (aref pair 0) xprev)
                   (/= (aref pair 1) yprev))
            nconc (list (setf xprev (aref pair 0))
                        (setf yprev (aref pair 1))))
    ))

(defmethod collect-pairs ((pairs <gpxform-pair-scanner>))
  (let* ((sub-scanner (make-pair-scanner
                       (pair-scanner-xsrc pairs)
                       (pair-scanner-ysrc pairs)))
         (xys  (collect-pairs sub-scanner)))
    (gp:transform-points (pair-scanner-gpxform pairs) xys xys)
    ))

;; --------------------------------------------------------

(defun generic-histo-pairs (pairs start-fn mid-fn end-fn)
  ;; Generic driver shared by several variant functions.
  ;;
  ;; In prep for draw-polygon point pairs. START-FN, MID-FN, END-FN
  ;; should furnish LIST objects that can be spliced into the
  ;; accumlating answer.
  ;;
  ;; NOTE: After COLLECT-PAIRS, we are working in unscaled pixel
  ;; space. Rightward and Downward are positive X and Y directions.
  ;;
  (let ((xys (collect-pairs pairs)))
    (when xys
      (destructuring-bind (x1 y1 . rest) xys
        (let ((ans (funcall start-fn x1 y1)))
          (um:nlet iter ((xys  rest)
                         (x1   x1)
                         (y1   y1)
                         (tl   (last ans)))
            (cond ((endp xys)
                   (rplacd tl (funcall end-fn x1 y1))
                   ans)
                  (t
                   (destructuring-bind (x2 y2 . rest) xys
                     (go-iter rest x2 y2
                              (last
                               (rplacd tl
                                       (funcall mid-fn x1 y1 x2 y2)
                                       )))
                     ))
                  ))
          )))
    ))

(defmethod stairstep-pairs ((pairs <pair-scanner>))
  ;;            x2,y2
  ;;             +
  ;;             |
  ;;    +--------+
  ;;  x1,y1     x2,y1
  (generic-histo-pairs pairs
                       #'list
                       (lambda (x1 y1 x2 y2)
                         (declare (ignore x1))
                         (list x2 y1 x2 y2))
                       #'lw:false))

(defmethod histo-pairs ((pairs <pair-scanner>))
  ;;            xm,y2  x2,y2
  ;;             +------
  ;;             |
  ;;    +--------+
  ;;  x1,y1     xm,y1
  (generic-histo-pairs pairs
                       #'list
                       (lambda (x1 y1 x2 y2)
                         (let ((xmid (/2 (+ x1 x2))))
                           (list xmid y1 xmid y2 x2 y2)))
                       #'lw:false))

(defmethod histo-vbars-pairs ((pairs <pair-scanner>) ybase)
  ;; (x,y) pairs converted to vert bars. X-axis values should be
  ;; monotonic.
  ;;
  ;;            xm,y2  x2,y2
  ;;             +-----+
  ;;             |     |
  ;;    +--------+     |
  ;;  x1,y1     xm,y1  |
  ;;    |              |
  ;;    +--------------+
  ;;   x1,0           x2,0
  ;;
  ;; In prep for draw-polygon with :filled t, we need to record the
  ;; baseline point at start and finish.
  (generic-histo-pairs pairs
                       (lambda (x1 y1)
                         (list x1 ybase x1 y1))
                       (lambda (x1 y1 x2 y2)
                         (let ((xmid (/2 (+ x1 x2))))
                           (list xmid y1 xmid y2 x2 y2)))
                       (lambda (x1 y1)
                         (declare (ignore y1))
                         (list x1 ybase))))

(defmethod histo-hbars-pairs ((pairs <pair-scanner>) xbase)
  ;; Here pairs of (x,y) need to be given a similar treatment along
  ;; the y-axis, as we did for histo-vbars-pairs along the x-axis.
  ;;
  ;; Y-axis vals should be monotonic.
  ;;
  ;; 0,y1 +---------------+ x1,y1
  ;;      |               |
  ;;      |    x2,ym      |
  ;;      |       +-------+ x1,ym
  ;;      |       |
  ;;      |       |
  ;; 0,y2 +-------+ x2,y2
  ;;
  ;; In prep for draw-polygon with :filled t, we need to record the
  ;; baseline point at start and finish.
  (generic-histo-pairs pairs
                       (lambda (x1 y1)
                         (list xbase y1 x1 y1))
                       (lambda (x1 y1 x2 y2)
                         (let ((ymid (/2 (+ y1 y2))))
                           (list x1 ymid x2 ymid x2 y2)))
                       (lambda (x1 y1)
                         (declare (ignore x1))
                         (list xbase y1))))

#|
(let* ((xs  '(1 2 3))
       (ys  '(10 20 30))
       (pairs (make-pair-scanner (make-scanner xs) (make-scanner ys))))
  (histo-pairs pairs))

(loop for ix from 1 to 10 nconc (list ix))
 |#
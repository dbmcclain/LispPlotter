
(in-package :plotter)

;; ------------------------------------------
;; Dealing with frame geometry.
;;
;; NOTE: the actual %X% and %Y% reported by CAPI has nothing to do
;; with our own drawing. As far as we are concerned our OUTPUT-PANE
;; starts at (0,0) in the upper-left corner.
;; -----------------------------------------

(defun qrange (rng &optional (default 0.1))
  (if (zerop rng)
      default
    rng))

(defun qdiv (a b &optional (default 0.1))
  (/ a (qrange b default)))

;; ------------------------------------------
(defun get-range (range v islog)
  (if (and range
           (/= (first range) (second range)))
      range
    (let ((v (if islog
                 (remove-if (complement #'plusp) v)
               v)))
      (if (plusp (length-of v))
          (let* ((vmin (vmin-of v))
                 (vmax (vmax-of v)))
            (if (= vmin vmax)
                (setf vmax (if (zerop vmin)
                               0.1
                             (* 1.1 vmin))))
            (list vmin vmax))
        (list (if islog 0.1 0) 1))
      )))

(defun nominal-box (pane box)
  (with-accessors ((nom-wd  plotter-nominal-width)
                   (nom-ht  plotter-nominal-height)) pane
    (or box
        (inset-box-sides
         (list 0 0 nom-wd nom-ht)
         +LEFT-INSET+ +TOP-INSET+ +RIGHT-INSET+ +BOTTOM-INSET+)
        )))

(defmethod pw-init-xv-yv ((pane <plotter-mixin>) xv yv
                          &key xrange yrange box xlog ylog aspect
                          &allow-other-keys)
  ;; initialize basic plotting parameters -- log scale axes, axis ranges,
  ;; plotting interior region (the box), and the graphic transforms to/from
  ;; data space to "pixel" space.  Pixel in quotes because they are real pixels
  ;; on Win/XP, but something altogether different on OS/X Display PDF.
  (destructuring-bind (xmin xmax)
      (if xv
          (get-range xrange xv xlog)
        (get-range xrange (list 0 (1- (length-of yv))) xlog))
    (destructuring-bind (ymin ymax) (get-range yrange yv ylog)
      
      (setf xmin (funcall (logfn xlog) xmin)
            xmax (funcall (logfn xlog) xmax)
            ymin (funcall (logfn ylog) ymin)
            ymax (funcall (logfn ylog) ymax))
      
      (unless yrange
        (let ((dy (/ (qrange (- ymax ymin)) 18)))
          (setf ymin (max (- ymin dy) (- $largest-permissible-value)))
          (setf ymax (min (+ ymax dy) $largest-permissible-value))
          ))
      
      (unless xrange
        (let ((dx (/ (qrange (- xmax xmin)) 18)))
          (setf xmin (max (- xmin dx) (- $largest-permissible-value)))
          (setf xmax (min (+ xmax dx) $largest-permissible-value))
          ))
      
      (let* ((box    (nominal-box pane box))
             (dx     (- xmax xmin))
             (dy     (- ymin ymax))
             (xscale (qdiv (box-width  box) dx))
             (yscale (qdiv (box-height box) dy))
             (xform  (gp:make-transform)))
        
        (when (and (numberp aspect)
                   (plusp aspect))
          (let* ((x-squeeze (<= aspect 1))
                 (scale     (if x-squeeze
                                (max xscale yscale)
                              (min xscale yscale))))
            (setf xscale (if x-squeeze
                             (* aspect scale)
                           scale)
                  yscale (if x-squeeze
                             scale
                           (/ scale aspect)))
            ))
        
        (setf (box-right  box) (+ (box-left box) (* xscale dx))
              (box-bottom box) (+ (box-top  box) (* yscale dy)))
      
        ;; NOTE: LW Docs imply that these operations pre-mult the
        ;; transform matrix - which I take to mean from-the-left.
        ;;
        ;; But LW states that the transforms are applied on the right
        ;; of a row-vector.  So, according to LW Docs, we should have
        ;; last-applied transform act first on the vector.
        ;;
        ;; But I find the opposite actually happens. The first applied
        ;; transform acts first on the vector. This means that the
        ;; successive transforms are actually applied from the left,
        ;; and that the transform acts from the left against a column
        ;; vector. (Leave it to the Brits to use a Left-hand rule...)
        ;;
        ;; Initial xform is: (1 0 0 1 0 0)
        ;; Read as: ((1 0)
        ;;           (0 1) 
        ;;           (0 0))
        ;; for application from the right on a row vector.
        ;;
        ;; Or else, if you like: ((1 0 0)
        ;;                        (1 0 0))
        ;; for appliction from the left on a column vector.
        ;;
        ;; Translate by: -1 -1 => (( 1  0)
        ;;                         ( 0  1)
        ;;                         (-1 -1))
        ;;
        ;; Scale by: 2 3 => (( 2  0)  ;; scaling from the right with ((2  0)
        ;;                   ( 0  3)                                  (0  3))
        ;;                   (-2 -3))
        ;;
        ;; Translate by: 1 1 => (( 2  0)
        ;;                       ( 0  3)
        ;;                       (-1 -2))
        ;;
        ;; Apply to point: (2 3 "1") => (3 7)
        ;;   (xform applied from the right on a row vector)
        ;; ----------------------------------------------------------------
        (gp:apply-translation xform  (- xmin) (- ymax))
        (gp:apply-scale xform        xscale   yscale)

        ;; ----------------------------------------------------------------
        ;; This, so far, now gets us to a viewport view for the
        ;; plotting area.  View is expressed using unscaled screen
        ;; coords, with origin at top-left corner. +X to the right, +Y
        ;; to the bottom.
        ;;
        ;; It is a virtual view.
        ;;
        ;; This much is all we need to perform plotting. The inverse
        ;; transform which follows goes the extra mile of converting
        ;; from screen coords to function space coords.
        ;; ---------------------------------------------------------------
        (setf (plotter-box  pane)   box
              (plotter-xmin pane)   xmin
              (plotter-xmax pane)   xmax
              (plotter-ymin pane)   ymin
              (plotter-ymax pane)   ymax
              (plotter-xlog pane)   xlog
              (plotter-ylog pane)   ylog
              (plotter-aspect pane) aspect
              (plotter-xform pane)  xform)
        ))))

(defun recompute-transform (pane)
  (with-accessors ((sf  plotter-sf)) pane
        
    (let ((xform     (gp:copy-transform (plotter-xform pane)))
          (inv-xform (gp:make-transform)))

      (gp:apply-translation xform +LEFT-INSET+ +TOP-INSET+)
      (gp:apply-scale xform sf sf)
      (gp:invert-transform xform   inv-xform)
      
      (setf (plotter-inv-xform pane) inv-xform
            (plotter-mask      pane) (plotting-region pane))
      )))

#|
(let* ((xform (gp:make-transform)))
  (gp:apply-translation xform -1 -1)
  (gp:apply-scale xform 2 3)
  (gp:apply-translation xform 1 1)
  xform
  ;; (gp:transform-point xform 2 3)
  )

;; -- these two are equivalent --
;; so, this tells us that CAPI applies transforms from innermost nesting outward

(let* ((screen (capi:contain (make-instance 'capi:pinboard-layout))))
  (capi:apply-in-pane-process
   screen
   (lambda ()
     (let ((pp (gp:create-pixmap-port screen 100 100 :background :gray70 :clear t)))
       (unwind-protect
           (gp:with-graphics-transform (pp (gp:make-transform))
             (gp:with-graphics-translation (pp 1 1)
               (gp:with-graphics-scale (pp 2 3)
                 (gp:with-graphics-translation (pp -1 -1)
                   ;; (gp:transform-point (gp:graphics-port-transform screen) 2 3)
                   (ac:send ac:println (gp:graphics-port-transform pp))
                   ))))
         (gp:destroy-pixmap-port pp))))
   ))

  |#

(defun half (x)
  (/ x 2))

(defun double (x)
  (+ x x))

(defun bounding-region (pane)
  ;; returns (list lf tp wd ht) for the parent frame of the plotting
  ;; region, as needed for gp:draw-rectangle
  (with-accessors ((sf         plotter-sf)
                   (nom-width  plotter-nominal-width)
                   (nom-height plotter-nominal-height)) pane
    (let ((xform  (gp:make-transform)))
      (gp:apply-scale xform sf sf)
      (destructuring-bind (lf tp rt bt)
          (gp:transform-points xform `(0 0 ,nom-width ,nom-height))
        (list lf tp (- rt lf) (- bt tp))
        ))))
  
(defun plotting-region (pane)
  ;; Return a region (lf tp wd ht) in parent pixel space that can be
  ;; used for clipping masks covering the plotting region. (The GP
  ;; State MASK seems ignorant of any transforms in effect, so we must
  ;; provide it.)
  ;;
  ;; We are always drawing into a "nominal plotview inset within a
  ;; parent frame of 400w x 300h pixels" but the actual parent frame
  ;; can grow or shrink, producing the outer scale factor, SF.
  ;;
  ;; Axis labels exist just outside the bounds of the PlotView and
  ;; extend into the parent frame by the inset amounts. IOW, the
  ;; PlotView is just the meat of the graph.
  ;;
  ;; We ignore scale factor, SF, in our drawing, since geometric
  ;; shapes, plotted points, and text annotations, all scale and
  ;; translate by the transform in effect at the time of drawing, and
  ;; that SF has been absorbed into the transform.
  ;;
  ;; The drawing should be enclosed within a WITH-PLOTVIEW-COORDS
  ;; clause. Within that clause we can assume that our top-left corner
  ;; of the plotting region is (0,0) and the bottom-right corner is at
  ;; (WD,HT).
  ;;
  ;; Separately applying the stored Pane Transform against data
  ;; produces pixel measures in PlotView Coords.  Hence, the top-left
  ;; is (xmin, ymax), and the bottom-right is (xmax, ymin).
  ;;
  ;; The Pane BOX has the PlotView represented in unscaled pixel
  ;; coordinates of the parent frame. It represents the nominal
  ;; plotting region.
  ;;
  (with-accessors ((sf   plotter-sf)
                   (box  plotter-box)) pane
    (let ((xform (gp:make-transform)))
      (gp:apply-translation xform +LEFT-INSET+ +TOP-INSET+)
      (gp:apply-scale xform sf sf)
      (destructuring-bind (lf tp rt bt)
          (gp:transform-points xform `(0 0 ,(box-width box) ,(box-height box)))
        `(,lf ,tp ,(- rt lf) ,(- bt tp))
        ))
    ))

(defun remove-from-top (r n)
  (destructuring-bind (lf tp rt bt) r
    (values `(,lf ,tp ,rt ,(+ tp n))
            `(,lf ,(+ tp n) ,rt ,bt))
    ))

(defun remove-from-bottom (r n)
  (destructuring-bind (lf tp rt bt) r
    (values `(,lf ,(- bt n) ,rt ,bt)
            `(,lf ,tp ,rt ,(- bt n)))
    ))

(defun remove-from-left (r n)
  (destructuring-bind (lf tp rt bt) r
    (values `(,lf ,tp ,n ,bt)
            `(,(+ lf n) ,tp ,rt ,bt))
    ))

(defun remove-from-right (r n)
  (destructuring-bind (lf tp rt bt) r
    (values `(,(- rt n) ,tp ,rt ,bt)
            `(,lf ,tp ,(- rt n) ,bt))
    ))

;; ---------------------------------------------------------

(defun vector-group-min (yvecs)
  (reduce #'min (mapcar #'vmin-of yvecs)))

(defun vector-group-max (yvecs)
  (reduce #'max (mapcar #'vmax-of yvecs)))

(defun pw-init-bars-xv-yv (pane xvec yvecs &rest args)
  ;; just run the usual scaling initialization
  ;; but against a y-vector that contains those values
  ;; from the multiple vectors which have the largest absolute values
  (apply #'pw-init-xv-yv pane
         (or (and xvec
                  (list (vmin-of xvec) (vmax-of xvec)))
             (and yvecs
                  (list 0 (1- (length-of (first yvecs))))
                  ))
         (and yvecs
              (list (vector-group-min yvecs)
                    (vector-group-max yvecs)))
         args))



(in-package :plotter)

;; ------------------------------------------
#|
(defun draw-path (port &rest positions)
  (gp:draw-polygon port
                   (mapcan #'append positions)))
|#
;; ------------------------------------------
#|
(defun zip (&rest seqs)
  (apply #'map 'list #'list seqs))

(defun staircase (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev yprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y xmid yprev) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun make-bars (xv yv)
  (let ((pairs (zip xv yv)))
    (um:foldl
     (lambda (ans pair)
       (destructuring-bind (x y) pair
         (destructuring-bind (xprev &rest _) ans
           (declare (ignore _))
           (let ((xmid (* 0.5 (+ x xprev))))
             (nconc (list x y xmid y) ans)
             ))))
     (first pairs)
     (rest pairs)
     )))

(defun interleave (&rest seqs)
  (mapcan #'nconc (apply #'zip seqs)))

|#
;; -------------------------------------------------------
#|
(defmethod draw-vertical-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-x
         (wd   (* 0.1 (gp:port-width port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (with-pairs (bars x y)
      (when xprev
        (setf wd   (abs (- x xprev))
              wd/2 (* 0.5 wd))
        (unless (= y yprev)
          (let ((next-x (+ xprev wd/2))
                (prev-x (or last-x
                            (- xprev wd/2))
                        ))
            (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
            (setf last-x next-x)
            )))
      (setf xprev x
            yprev y))
    (when xprev
      ;; use the last known width
      (let ((next-x (+ xprev wd/2))
            (prev-x (or last-x
                        (- xprev wd/2))
                    ))
        (gp:draw-rectangle port prev-x 0 (- next-x prev-x) yprev :filled t)
        ))
    ))

(defmethod draw-horizontal-bars (port (bars <pair-scanner>))
  (let* (xprev
         yprev
         last-y
         (wd   (* 0.1 (gp:port-height port))) ;; default if only one data point
         (wd/2 (* 0.5 wd)))
    (with-pairs (bars x y)
      (when yprev
        (setf wd   (abs (- y yprev))
              wd/2 (* 0.5 wd))
        (unless (= x xprev)
          (let ((next-y (+ yprev wd/2))
                (prev-y (or last-y
                            (- yprev wd/2))
                        ))
            (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
            (setf last-y next-y)
            )))
      (setf xprev x
            yprev y))
    (when xprev
      ;; use the last known width
      (let ((next-y (+ yprev wd/2))
            (prev-y (or last-y
                        (- yprev wd/2))
                    ))
        (gp:draw-rectangle port 0 prev-y xprev (- next-y prev-y) :filled t)
        ))
    ))
|#

;; ----------------------------------------------------------  

(defun get-symbol-plotfn (pane symbol-style)
  (labels ((draw-symbol (fn)
             #-:WIN32
             (with-color (pane (or (if (fill-color symbol-style)
                                     (adjust-color pane
                                                   (fill-color symbol-style)
                                                   (fill-alpha symbol-style)))
                                   #.(color:make-gray 1.0 0.25)))
               (funcall fn t))
             #+:WIN32
             (when (fill-color symbol-style)
               (with-color (pane (adjust-color pane
                                               (fill-color symbol-style)
                                               (fill-alpha symbol-style)))
                 (funcall fn t)))
             (gp:with-graphics-state (pane
                                      :thickness  (adjust-linewidth (border-thick symbol-style))
                                      :foreground (adjust-color pane
                                                                (border-color symbol-style)
                                                                (border-alpha symbol-style)))
               (funcall fn))))
    
    (ecase (plot-symbol symbol-style)
      (:cross     (lambda (x y)
                    (gp:with-graphics-state (pane
                                             :thickness  (adjust-linewidth (border-thick symbol-style))
                                             :foreground (adjust-color pane
                                                                       (border-color symbol-style)
                                                                       (border-alpha symbol-style)))
                      (gp:draw-line pane (- x 3) y (+ x 3) y)
                      (gp:draw-line pane x (- y 3) x (+ y 3))
                      )))
      
      (:x         (lambda (x y)
                    (gp:with-graphics-state (pane
                                             :thickness  (adjust-linewidth (border-thick symbol-style))
                                             :foreground (adjust-color pane
                                                                       (border-color symbol-style)
                                                                       (border-alpha symbol-style)))
                      (gp:draw-line pane (- x 3) (- y 3) (+ x 3) (+ y 3))
                      (gp:draw-line pane (+ x 3) (- y 3) (- x 3) (+ y 3))
                      )))
      
      ((:circle :sampled-data)
       (lambda (x y)
         (labels ((draw-circle (&optional filled)
                    (gp:draw-circle pane
                                    x 
                                    #-:WIN32 (- y 0.5)
                                    #+:WIN32 y
                                    3
                                    :filled filled)))
           (draw-symbol #'draw-circle)
           )))

      ((:box :square)
       (lambda (x y)
         (labels ((draw-rectangle (&optional filled)
                    (gp:draw-rectangle pane (- x 3) (- y 3) 6 6
                                       :filled filled)))
           (draw-symbol #'draw-rectangle)
           )))

      ((:triangle :up-triangle)
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon pane
                                     (list (- x 3) (+ y 3)
                                           x (- y 4)
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:down-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon pane
                                     (list (- x 3) (- y 3)
                                           x (+ y 4)
                                           (+ x 3) (- y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:right-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon pane
                                     (list (- x 3) (- y 3)
                                           (+ x 4) y
                                           (- x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))
      
      (:left-triangle
       (lambda (x y)
         (labels ((draw-triangle (&optional filled)
                    (gp:draw-polygon pane
                                     (list (+ x 3) (- y 3)
                                           (- x 4) y
                                           (+ x 3) (+ y 3))
                                     :closed t
                                     :filled filled)))
           (draw-symbol #'draw-triangle)
           )))

      (:dot
       (lambda (x y)
         (with-color (pane (adjust-color pane
                                         (border-color symbol-style)
                                         (border-alpha symbol-style)))
           (gp:draw-circle pane x (1- y) 0.5))
         ))
      )))

;; --------------------------------------------------------------------

(defmethod pw-plot-prepped ((pane plotter-pane) prepped symbol-fn
                                   &key
                                   ;; (color #.(color:make-rgb 0.0 0.5 0.0))
                                   ;; alpha
                                   ;; thick
                                   ;; (linewidth (or thick 1))
                                   ;; linedashing
                                   ;; symbol
                                   ;; legend
                                   legend-x
                                   legend-y
                                   legend-anchor
                                   ;; (border-color color)
                                   ;; symbol-filled
                                   ;; (fill-color color)
                                   ;; (border-thick linewidth)
                                   ;; barwidth
                                   ;; bar-offset
                                   plot-style
                                   symbol-for-legend
                                   &allow-other-keys)
    (when (legend plot-style)
      (append-legend pane plot-style))

    (when legend-x
      (setf (plotter-legend-x pane) legend-x))
    (when legend-y
      (setf (plotter-legend-y pane) legend-y))
    (when legend-anchor
      (setf (plotter-legend-anchor pane) legend-anchor))

    (with-plotview-coords (pane)
      
      (gp:with-graphics-state (pane
                               ;; :thickness  linewidth
                               ;; :dashed     (not (null linedashing))
                               ;; :dash       linedashing
                               ;; :foreground color
                               :line-end-style   :butt
                               :line-joint-style :miter
                               ;; :scale-thickness  t  ;; already present form WITH-PLOTVIEW-COORDS
                               :mask             (plotter-mask pane))
        (let+ ((xform        (plotter-xform pane))
               (line-style   (line-style plot-style))
               (symbol-style (and (not symbol-for-legend)
                                  (symbol-style plot-style)))
               (:mvb (_ y0) (gp:transform-point xform 0 0)))
          (flet ((draw-lines ()
                   (gp:with-graphics-state (pane
                                            :thickness  (adjust-linewidth (line-thick line-style))
                                            :foreground (adjust-color pane
                                                                      (line-color line-style)
                                                                      (line-alpha line-style))
                                            :dashed     (line-dashing line-style)
                                            :dash       (line-dashing line-style))
                     (gp:draw-polygon pane prepped))
                   ))
            (cond (symbol-style
                   (case (plot-symbol symbol-style)
                     
                     ((:vbars :hbars)
                      (gp:with-graphics-state (pane
                                               :foreground (adjust-color pane
                                                                         (fill-color symbol-style)
                                                                         (fill-alpha symbol-style)))
                        (if (bar-width symbol-style)
                            (with-rects (prepped x y wd ht)
                              (gp:draw-rectangle pane
                                                 x y wd ht
                                                 :filled t))
                          ;; else
                          (gp:draw-polygon pane prepped :filled t)
                          )))
                     
                     (:sampled-data
                      (gp:with-graphics-state (pane
                                               :foreground (adjust-color pane
                                                                         (or (and line-style
                                                                                  (line-color line-style))
                                                                             :black)
                                                                         (or (and line-style
                                                                                  (line-alpha line-style))
                                                                             1))
                                               :thickness  (adjust-linewidth (or (and line-style
                                                                                      (line-thick line-style))
                                                                                 1)))
                        (with-pts (prepped x y)
                          (gp:draw-line pane x y0 x y)
                          (funcall symbol-fn x y))
                        ))
                     
                     (otherwise
                      (when line-style
                        (draw-lines))                   
                      (with-pts (prepped x y)
                        (funcall symbol-fn x y)))
                     ))
                  
                  (line-style
                   (case (line-type line-style)
                     ((:stepped :histo)
                      (gp:with-graphics-state (pane
                                               :thickness  (adjust-linewidth (line-thick line-style))
                                               :foreground (adjust-color pane
                                                                         (line-color line-style)
                                                                         (line-alpha line-style)))
                        (gp:draw-polygon pane prepped)
                        ))
                     
                     (otherwise
                      (draw-lines))
                     ))
                  ))))))

;; --------------------------------------------------------------------
#|
(defmethod unsafe-pw-plot-xv-yv ((cpw plotter-pane) port xvector yvector 
                                 &key
                                 ;; (color #.(color:make-rgb 0.0 0.5 0.0))
                                 ;; alpha
                                 ;; thick
                                 ;; (linewidth (or thick 1))
                                 ;; linedashing
                                 ;; symbol
                                 plot-joined
                                 ;; legend
                                 legend-x
                                 legend-y
                                 legend-anchor
                                 ;; (border-color color)
                                 ;; symbol-filled
                                 ;; (fill-color color)
                                 ;; (border-thick linewidth)
                                 ;; barwidth
                                 ;; bar-offset
                                 plot-style
                                 symbol-for-legend
                                 &allow-other-keys)
  ;; this is the base plotting routine
  ;; called only from within the pane process
  ;; (return-from unsafe-pw-plot-xv-yv)
  (let+ (;; (color     (adjust-color cpw color alpha))
            ;; (linewidth (adjust-linewidth linewidth))
            (line-style   (line-style plot-style))
            (symbol-style (and (not symbol-for-legend)
                               (symbol-style plot-style)))
            
            (nel       (if xvector
                           (min (length-of xvector) (length-of yvector))
                         (length-of yvector)))
            (xlog      (plotter-xlog cpw))
            (xlogfn    (logfn xlog))
            (ylog      (plotter-ylog cpw))
            (ylogfn    (logfn ylog))
            (xs         (let ((scanner (make-scanner (or xvector
                                                         nel))
                                       ))
                          (if xlog
                              (make-transformer scanner xlogfn)
                            scanner)))
            
            (ys         (let ((scanner (make-scanner yvector)))
                          (if ylog
                              (make-transformer scanner ylogfn)
                            scanner)))
            (pairs     (make-pair-scanner xs ys))
            (xform     (plotter-xform cpw))
            (xfpairs   (make-gpxform-pairs xform pairs))
            (:mvb (x0 y0) (gp:transform-point xform 0 0)))

    (when (legend plot-style)
      (append-legend cpw plot-style))

    (when legend-x
      (setf (plotter-legend-x cpw) legend-x))
    (when legend-y
      (setf (plotter-legend-y cpw) legend-y))
    (when legend-anchor
      (setf (plotter-legend-anchor cpw) legend-anchor))

    (with-plotview-coords (cpw port)
      
      (gp:with-graphics-state (port
                               ;; :thickness  linewidth
                               ;; :dashed     (not (null linedashing))
                               ;; :dash       linedashing
                               ;; :foreground color
                               :line-end-style   :butt
                               :line-joint-style :miter
                               ;; :scale-thickness  t  ;; already present form WITH-PLOTVIEW-COORDS
                               :mask             (plotter-mask cpw))
        (labels ((draw-lines ()
                   (let* ((xfpairs (case plot-joined
                                     ((:spline)
                                      (make-gpxform-pairs xform
                                                          (make-pairs-for-spline pairs)))
                                     (otherwise
                                      xfpairs))))
                     (gp:with-graphics-state (port
                                              :thickness  (adjust-linewidth (line-thick line-style))
                                              :foreground (adjust-color cpw
                                                                        (line-color line-style)
                                                                        (line-alpha line-style))
                                              :dashed     (line-dashing line-style)
                                              :dash       (line-dashing line-style))
                       (gp:draw-polygon port (collect-pairs xfpairs))
                       ))))

          (cond (symbol-style
                 (case (plot-symbol symbol-style)
                 
                   (:vbars
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port
                                                                       (fill-color symbol-style)
                                                                       (fill-alpha symbol-style)))
                      (if (bar-width symbol-style)
                          (let* ((wd   (get-x-width port (bar-width symbol-style)))
                                 (wd/2 (* 0.5 wd))
                                 (off  (if (bar-offset symbol-style)
                                           (get-x-width cpw (bar-offset symbol-style))
                                         0)
                                       ))
                            (with-pairs (xfpairs x y)
                              (gp:draw-rectangle port
                                                 (+ off (- x wd/2)) y0
                                                 wd (- y y0)
                                                 :filled t)
                              ))
                        
                        (let ((xys (histo-vbars-pairs xfpairs y0)))
                          (gp:draw-polygon port xys :filled t)
                          ))))
                 
                   (:hbars
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port
                                                                       (fill-color symbol-style)
                                                                       (fill-alpha symbol-style)))
                      (if (bar-width symbol-style)
                          (let* ((wd   (get-y-width port (bar-width symbol-style)))
                                 (wd/2 (* 0.5 wd))
                                 (off  (if (bar-offset symbol-style)
                                           (get-y-width port (bar-offset symbol-style))
                                         0)
                                       ))
                            (with-pairs (xfpairs x y)
                              (gp:draw-rectangle port
                                                 x0 (+ off (- y wd/2))
                                                 (- x x0) wd
                                                 :filled t)
                              ))
                        (let ((xys (histo-hbars-pairs xfpairs x0)))
                          (gp:draw-polygon port xys :filled t)
                          ))))
                 
                   (:sampled-data
                    (gp:with-graphics-state (port
                                             :foreground (adjust-color port
                                                                       (or (and line-style
                                                                                (line-color line-style))
                                                                           :black)
                                                                       (or (and line-style
                                                                                (line-alpha line-style))
                                                                           1))
                                             :thickness  (adjust-linewidth (or (and line-style
                                                                                    (line-thick line-style))
                                                                               1)))
                      (let ((dotfn (get-symbol-plotfn port (symbol-style plot-style))))
                        (with-pairs (xfpairs x y)
                          (gp:draw-line port x y0 x y)
                          (funcall dotfn x y))
                        )))
                 
                   (otherwise
                    (when line-style
                      (draw-lines))
                  
                    (let ((plotfn (get-symbol-plotfn port symbol-style)))
                      (with-pairs (xfpairs x y)
                        (funcall plotfn x y)
                        )))
                   ))
              
                (line-style
                 (case (line-type line-style)
                   (:stepped
                    (gp:with-graphics-state (port
                                             :thickness  (adjust-linewidth (line-thick line-style))
                                             :foreground (adjust-color port
                                                                       (line-color line-style)
                                                                       (line-alpha line-style)))
                      (gp:draw-polygon port (stairstep-pairs xfpairs))
                      ))
                 
                   (:histo
                    (gp:with-graphics-state (port
                                             :thickness  (adjust-linewidth (line-thick line-style))
                                             :foreground (adjust-color port
                                                                       (line-color line-style)
                                                                       (line-alpha line-style)))
                      (gp:draw-polygon port (histo-pairs xfpairs))
                      ))
                 
                   (otherwise
                    (draw-lines))
                   ))
                )))
      )))

(defmethod pw-plot-xv-yv ((cpw plotter-pane) port xvector yvector &rest args)
  (let ((state (plotter-cache-state cpw)))
    (when (or (null state)
              (eql state (getf args :cache :drawing)))
      (progn ;; ignore-errors
        (apply #'unsafe-pw-plot-xv-yv cpw port xvector yvector args)))
    ))          
|#

;; ------------------------------------------------------------------------------
(defun get-bar-symbol-plotfn (pane symbol color neg-color bar-width testfn)
  ;; bear in mind that the y values at this point are absolute screen
  ;; coords and are inverted with respect to data ordering
  (ecase symbol
    (:sigma
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line pane x ymin x ymax)
         (gp:draw-line pane (- x (/ bar-width 2)) ymin (+ x (/ bar-width 2)) ymin)
         (gp:draw-line pane (- x (/ bar-width 2)) ymax (+ x (/ bar-width 2)) ymax)
         )))

    (:hl-bar
     (lambda (x ys)
       (destructure-vector (ymin ymax) ys
         (gp:draw-line pane x ymin x ymax)
         )))
    
    (:hlc-bar
     (lambda (x ys)
       (destructure-vector (h l c) ys
         (gp:draw-line pane x l x h)
         (gp:draw-line pane x c (+ x (/ bar-width 2)) c)
         )))
    
    (:ohlc-bar
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (with-color (pane (if (funcall testfn c o) neg-color color))
           (gp:draw-line pane x l x h)
           (gp:draw-line pane (- x (/ bar-width 2)) o x o)
           (gp:draw-line pane x c (+ x (/ bar-width 2)) c)
           ))))
    
    (:candlestick
     (lambda (x ys)
       (destructure-vector (o h l c) ys
         (if (funcall testfn c o)
             (with-color (pane neg-color)
               (gp:draw-line pane x l x h)
               (gp:draw-rectangle pane (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
           (progn
             (with-color (pane :black)
               (gp:draw-line pane x l x h))
             (with-color (pane color)
               (gp:draw-rectangle pane (- x (/ bar-width 2)) o bar-width (- c o)
                                  :filled t))
             (with-color (pane :black)
               (gp:draw-rectangle pane (- x (/ bar-width 2)) o bar-width (- c o)))
             ))
         )))
    ))

;;-------------------------------------------------------------------

(defmethod pw-plot-bars-xv-yv ((pane plotter-pane) xvector yvectors 
                               &key
                               (color #.(color:make-rgb 0.0 0.5 0.0))
                               (neg-color color)
                               alpha
                               thick
                               (linewidth (or thick 1))
                               (bar-width 6)
                               (symbol (ecase (length yvectors)
                                         (2 :sigma)
                                         (3 :hlc-bar)
                                         (4 :ohlc-bar)))
                               &allow-other-keys)
  ;; this is the base bar-plotting routine
  ;; called only from within the pane process
  (let* ((xform     (plotter-xform pane))
         (color     (adjust-color pane color alpha))
         (neg-color (adjust-color pane neg-color alpha))
         (linewidth (adjust-linewidth linewidth))

         (nel       (let ((nely (reduce #'min (mapcar #'length-of yvectors))))
                      (if xvector
                          (min (length-of xvector) nely)
                        nely)))
         (xlog      (plotter-xlog pane))
         (xlogfn    (logfn xlog))
         (ylog      (plotter-ylog pane))
         (ylogfn    (logfn ylog))
         (xs        (let* ((xform   (lambda (x)
                                      (gp:transform-point xform x 0)))
                           (scanner (make-scanner (or xvector
                                                      nel)
                                                  :max-items nel)))
                      (make-transformer scanner
                                        (if xlog
                                            (um:compose xform xlogfn)
                                          xform))
                      ))

         (xform-y   (lambda (y)
                      (second (multiple-value-list
                               (gp:transform-point xform 0 y)))
                      ))

         (ys        (let* ((scanners (mapcar #'make-scanner yvectors)))
                      (mapcar (um:rcurry #'make-transformer
                                         (if ylog
                                             (um:compose xform-y ylogfn)
                                           xform-y))
                              scanners)
                      ))
         (c<o-testfn (let ((y1 (funcall xform-y 0))
                           (y2 (funcall xform-y 1)))
                       (if (< y2 y1)
                           #'>
                         #'<)))
         (plotfn (get-bar-symbol-plotfn pane symbol
                                        color neg-color bar-width
                                        c<o-testfn))
         (tmp       (make-array (length ys))))

    (with-plotview-coords (pane)
      (gp:with-graphics-state (pane
                               :thickness  linewidth
                               :foreground color
                               :line-end-style   :butt
                               :line-joint-style :miter
                               :mask       (plotter-mask pane))
        
        (with-scanner (xs x)
          (map-into tmp #'next-item ys)
          (funcall plotfn x tmp)))
      )))

;; ============================================================

(declaim (inline /2))

(defun /2 (x)
  (/ x 2))

(defun plt-draw-shape (pane shape x0 y0
                          &key
                          width height radius to
                          color alpha filled
                          border-thick border-color border-alpha
                          start-angle sweep-angle raw
                          &allow-other-keys)
  ;; For :rect,   (x0,y0) = ctr, (wd,ht) are width, height
  ;; For :ellipse (x0,y0) = ctr, (wd,ht) are radii
  ;; For :arc     (x0,y0) = ctr, (wd,ht) are radii, sweep and start angles are radian measure
  (let* ((color     (adjust-color pane color alpha))
         (bcolor    (adjust-color pane border-color border-alpha))
         (linewidth (adjust-linewidth (or border-thick 0)))
         (x0        (get-x-location pane x0))
         (y0        (get-y-location pane y0)))
    (labels
        ((draw-fns ()
             (labels ((draw (shape-fn)
                        (labels ((do-drawing ()
                                   (gp:with-graphics-state (pane
                                                            :thickness        linewidth
                                                            :foreground       color
                                                            :line-end-style   :butt
                                                            :line-joint-style :miter)
                                     (when filled
                                       (funcall shape-fn :filled t))
                                     (when border-thick
                                       (with-color (pane bcolor)
                                         (funcall shape-fn :filled nil)))
                                     )))
                          (if raw
                              (do-drawing)
                            (gp:with-graphics-state (pane
                                                     :mask (plotter-mask pane))
                              (do-drawing))
                            ))))
               
               (ecase shape
                 (:line
                  (let ((x1 (get-x-location pane (car to)))
                        (y1 (get-y-location pane (cdr to))))
                    (draw (um:curry #'gp:draw-line pane x0 y0 x1 y1))
                    ))
                 (:rect
                  (let ((wd (get-x-width pane width))
                        (ht (get-y-width pane height)))
                    (draw (um:curry #'gp:draw-rectangle pane
                                    (- x0 (/2 wd)) (- y0 (/2 ht)) wd ht))
                    ))
                 (:ellipse
                  (let ((wd (get-x-width pane width))
                        (ht (get-y-width pane height)))
                    (draw (um:curry #'gp:draw-ellipse pane x0 y0 wd ht))))
                 
                 (:arc
                  (let ((wd (get-x-width pane width))
                        (ht (get-y-width pane height)))
                    (draw (um:curry #'gp:draw-arc pane
                                    (- x0 (/2 wd)) (- y0 (/2 ht)) wd ht
                                    start-angle sweep-angle))
                    ))
                 (:circle
                  (let ((rad (get-x-width pane radius)))
                    (draw (um:curry #'gp:draw-circle pane x0 y0 rad))
                    ))
                 ))))
      (if raw
          (draw-fns)
        (with-plotview-coords (pane)
          (draw-fns)))
      )))

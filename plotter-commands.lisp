
(in-package :plotter)

;; -------------------------------------------------------------------

(defparameter *default-args*
  (list
   :watermarkfn 'watermark))

(defmacro with-default-args ((&rest args) &body body)
  `(apply (lambda (&rest *default-args*) ,@body) ,@args *default-args*))

;; -------------------------------------------------------------------

(defun augment-display-list (pane action fresh)
  ;; Set up the pane to peform work in the CAPI thread.
  (without-capi-contention pane
    ;; it is probably a bad idea to mutate the display list from any
    ;; but the CAPI thread. The pane might already be displayed and
    ;; there could be an update in progress.
    (when fresh
      (discard-display-list pane))
    (append-display-list pane action)
    (redraw-entire-pane pane)))

;; -------------------------------------------------------------------

(defun draw-shape (shape pane x0 y0
                         &rest args
                         &key
                         (color :darkgreen)
                         (filled t)
                         (alpha 1)
                         border-thick
                         (border-color :black)
                         (border-alpha 1)
                         start-angle  ;; for arc
                         sweep-angle  ;; for arc
                         radius
                         width
                         height
                         to
                        
                         &allow-other-keys)
  (let* ((pane      (plotter-pane-of pane))
         (augm-args (list*
                     :color  color
                     :alpha  alpha
                     :filled filled
                     :border-thick border-thick
                     :border-color border-color
                     :border-alpha border-alpha
                     :width  width
                     :height height
                     :radius radius
                     :to     to
                     :start-angle  start-angle
                     :sweep-angle  sweep-angle
                     args))
         (action    (lambda (pane _x _y _width _height)
                      (declare (ignore _x _y _width _height))
                      (apply 'plt-draw-shape pane shape x0 y0 augm-args))))
    (augment-display-list pane action nil)
    ))

;; user callable function
(defun draw-rect (pane x0 y0 width height &rest args)
  (multiple-value-call 'draw-shape :rect pane x0 y0 :width width :height height
    (values-list args) (values-list *default-args*)))

;; user callable function
(defun draw-ellipse (pane x0 y0 width height &rest args)
  (multiple-value-call 'draw-shape :ellipse pane x0 y0 :width width :height height
    (values-list args) (values-list *default-args*)))

;; user callable function
(defun draw-arc (pane x0 y0 width height start-angle sweep-angle &rest args)
  (multiple-value-call 'draw-shape :arc pane x0 y0 :width width :height height
    :start-angle start-angle :sweep-angle sweep-angle
    (values-list args) (values-list *default-args*)))

;; user callable function
(defun draw-circle (pane x0 y0 radius &rest args)
  (multiple-value-call 'draw-shape :circle pane x0 y0 :radius radius
    (values-list args) (values-list *default-args*)))

;; user callable function
(defun draw-line (pane x0 y0 x1 y1 &rest args)
  (multiple-value-call 'draw-shape :line pane x0 y0 :to `(,x1 . ,y1)
    (values-list args) (values-list *default-args*)))

;; -------------------------------------------------------------------

(defun oplot2 (pane xv yv 
                    &rest args
                    &key
                    clear
                    ;;draw-axes
                    ;;(color :darkgreen)
                    ;; thick
                    xlog
                    ylog
                    ;; (linewidth (or thick 1))
                    (logo *ext-logo*)
                    (logo-alpha *ext-logo-alpha*)
                    (cright1 *cright1*)
                    (cright2 *cright2*)
                    ;;(fullgrid t)
                   
                    &allow-other-keys)
  (let ((pane  (plotter-pane-of pane args)))
    (with-delayed-update (pane)
      (let+ ((:mvb (xvf yvf)
              (cond (xv
                     (filter-potential-x-y-nans-and-infinities xv yv xlog ylog))
                    (yv
                     (values nil
                             (filter-potential-nans-and-infinities yv ylog)))
                    ))
             (style (apply 'get-plot-style args))
             (fresh (or clear
                        (display-list-empty-p pane)))
             (augm-args (if fresh
                            (list*
                             :plot-style style
                             ;; :color     color
                             ;; :linewidth linewidth
                             ;; :fullgrid  fullgrid
                             :logo       logo
                             :logo-alpha logo-alpha
                             :cright1    cright1
                             :cright2    cright2
                             args)
                          ;; else
                          (list*
                           :plot-style style
                           ;; :color color
                           args)))
             (_      (when fresh
                       (apply 'pw-init-xv-yv pane xvf yvf augm-args)))
             (:mvb (prepped symbol-fn)  (apply #'prep-vectors pane xvf yvf augm-args))
             (action    (lambda (pane x y width height)
                          (declare (ignore x y width height))
                          (when fresh
                            (apply 'pw-axes pane augm-args))
                          (apply 'pw-plot-prepped pane prepped symbol-fn augm-args)
                          )))
        (augment-display-list pane action fresh)
        ))))

;; -------------------------------------------------------------------

(defun oplot-bars2 (pane xv yvs
                         &rest args
                         &key
                         ;; draw-axes
                         clear
                         (color     :black)
                         (neg-color color)
                         thick
                         (linewidth (or thick 1))
                         ;; (fullgrid t)
                         (logo *ext-logo*)
                         (logo-alpha *ext-logo-alpha*)
                         (cright1 *cright1*)
                         (cright2 *cright2*)
                        
                         &allow-other-keys)
  (let ((pane  (plotter-pane-of pane args)))
    (with-delayed-update (pane)
      (let+ ((fresh (or clear
                        (display-list-empty-p pane)))
             (augm-args (if fresh
                            (list*
                             :color     color
                             :neg-color neg-color
                             :linewidth linewidth
                             ;; :fullgrid  fullgrid
                             :logo logo
                             :logo-alpha logo-alpha
                             :cright1 cright1
                             :cright2 cright2
                             args)
                          ;; else
                          (list*
                           :color color
                           :neg-color neg-color
                           args)))
             (action    (lambda (pane x y width height)
                          (declare (ignore x y width height))
                          (when fresh
                            (apply 'pw-axes pane augm-args))
                          (apply 'pw-plot-bars-xv-yv pane xv yvs augm-args))
                        ))
        (when fresh
          ;; no drawing in the init, so do it in my thread
          (apply 'pw-init-bars-xv-yv pane xv yvs augm-args))
        (augment-display-list pane action fresh)
        ))))

;; ------------------------------------------

(defun find-x-y-parms (args)
  ;; Enables flexible invocation of plot.
  ;; e.g., (plot xs ys &rest kw-args)
  ;;  or   (plot ys &rest kw-args), with implied xs
  (let ((nargs (or (position-if 'keywordp args)
                   (length args))))
    (case nargs
      (0   (values nil nil args))
      (1   (values nil (car args) (cdr args)))
      (2   (values (car args) (cadr args) (cddr args)))
      (otherwise (error "Too many arguments"))
      )))

(defun vector-to-plotfn (fn pane &rest args)
  (multiple-value-bind (xs ys parms)
      (find-x-y-parms args)
    (multiple-value-call fn pane xs ys
      (values-list parms) (values-list *default-args*))))

;; user callable function
(defun plot (pane &rest args)
  (apply 'vector-to-plotfn 'oplot2 pane args))

;; user callable function
(defun plot-bars (pane &rest args)
  (apply 'vector-to-plotfn 'oplot-bars2 pane args))

;; ------------------------------------------

;; user callable function
(defun clear (pane)
  (let ((pane (plotter-pane-of pane)))
    (augment-display-list pane nil t)))

;; -------------------------------------------------------------------

(defun axes2 (pane xvector yvectors &rest args &key xrange xlog ylog
                   (logo *ext-logo*)
                   (logo-alpha *ext-logo-alpha*)
                   (cright1 *cright1*)
                   (cright2 *cright2*)
                  
                   &allow-other-keys)
  ;; allow a list of yvectors to be given
  ;; so that we can find the best fitting autoscale that accommodates all of them
  (let ((pane (plotter-pane-of pane args)))
    (with-delayed-update (pane)
      (multiple-value-bind (xv yv)
          (let ((ylist (remove nil (um:mklist yvectors))))
            (unless (lw:sequencep (first ylist))
              (setf ylist (list ylist)))
            (values (or (and xvector
                             (let ((xv (filter-potential-nans-and-infinities xvector xlog)))
                               (vector (vmin-of xv) (vmax-of xv))))
                        (and (null xrange)
                             ylist
                             (vector (if xlog 0.1 0) (1- (length-of (first ylist))))
                             ))
                    (and ylist
                         (let ((ys (mapcar (um:rcurry 'filter-potential-nans-and-infinities ylog) ylist)))
                           (vector (vector-group-min ys)
                                   (vector-group-max ys))))
                    ))
        (let+ ((augm-args (list*
                           :logo logo
                           :logo-alpha logo-alpha
                           :cright1 cright1
                           :cright2 cright2
                           args))
               (action  (lambda (pane x y width height)
                          (declare (ignore x y width height))
                          (apply 'pw-axes pane augm-args))
                        ))
          ;; do the init setup in our own thread
          (apply 'pw-init-xv-yv pane xv yv augm-args)
          (augment-display-list pane action t)
          )))))

;; user callable function
(defun axes (pane &rest args)
  (apply 'vector-to-plotfn 'axes2 pane args))

;; ------------------------------------------
(defun outsxy (pane x y str
                    &rest args
                    &key
                    (font-size $normal-times-font-size)
                    (font "Times")
                    anchor
                    (align :w)
                    (offset-x 0) ;; pixel offsets
                    (offset-y 0)
                    (color :black)
                    alpha                  
                    &allow-other-keys)
  (let* ((pane   (plotter-pane-of pane))
         (action (lambda (pane xarg yarg width height)
                   (declare (ignore xarg yarg width height))
                   (with-plotview-coords (pane)
                     (let* ((xx (+ offset-x (get-x-location pane x)))
                            (yy (+ offset-y (get-y-location pane y)))
                            (font (find-best-font pane
                                                  :family font
                                                  :size   font-size))
                            (x-align (ecase (or anchor align)
                                       ((:nw :w :sw) :left)
                                       ((:n :s :ctr) :center)
                                       ((:ne :e :se) :right)))
                            (y-align (ecase (or anchor align)
                                       ((:nw :n :ne) :top)
                                       ((:w :ctr :e) :center)
                                       ((:sw :s :se) :baseline)))
                            (color (adjust-color pane color alpha)))
                       
                       ;; #+:WIN32
                       (with-mask (pane (plotter-mask pane))
                         (apply 'draw-string-x-y pane str
                                xx yy
                                :font font
                                :x-alignment x-align
                                :y-alignment y-align
                                :color       color
                                args))
                       #|
                        #-:WIN32
                        (let* ((font-attrs (gp:font-description-attributes
                                            (gp:font-description font)))
                               (font-name  (getf font-attrs :name))
                               (font-size  (getf font-attrs :size)))
                          (apply 'add-label port str (* sf xx) (* sf yy)
                                 :font        font-name
                                 :font-size   font-size
                                 :color       color
                                 :x-alignment x-align
                                 :y-alignment y-align
                                 :box         mask
                                 args)
                          )
                        |#
                       )))
                 ))
    (augment-display-list pane action nil)
    ))

(defun draw-text-box (pane strs xorg yorg
                           &key
                           (font-size $normal-times-font-size)
                           (font "Times")
                           (text-color :black)
                           filled
                           (color :white)
                           alpha
                           border-thick
                           (border-color :black)
                           border-alpha
                           &allow-other-keys)
  (let* ((pane  (plotter-pane-of pane))
         (action (lambda (pane xarg yarg width height)
                   (declare (ignore xarg yarg width height))
                   (with-plotview-coords (pane)
                     (let* ((font (find-best-font pane
                                                  :size   font-size
                                                  :family font))
                            (width (loop for s in strs maximize
                                           (multiple-value-bind (left top right bottom)
                                               (gp:get-string-extent pane s font)
                                             (declare (ignore top bottom))
                                             (- right left))))
                            (height (multiple-value-bind (left top right bottom)
                                        (gp:get-string-extent pane (car strs) font)
                                      (declare (ignore left right))
                                      (- bottom top)))
                            (x0        (get-x-location pane xorg))
                            (y0        (get-y-location pane yorg))
                            (color     (adjust-color pane color alpha))
                            (bcolor    (adjust-color pane border-color border-alpha))
                            (linewidth (adjust-linewidth (or border-thick 0))))
                       
                       (gp:with-graphics-state (pane
                                                :foreground color
                                                :thickness  linewidth
                                                :line-end-style   :butt
                                                :line-joint-style :miter
                                                :mask  (plotter-mask pane))
                         (when filled
                           (gp:draw-rectangle pane x0 y0
                                              (+ width 4)
                                              (* (length strs) height)
                                              :filled color))
                         (when border-thick
                           (with-color (pane bcolor)
                             (gp:draw-rectangle pane x0 y0
                                                (+ width 4)
                                                (* (length strs) height)
                                                :filled nil))))
                       
                       (loop for y from (+ height y0 -2) by height
                             for s in strs
                             for x = (+ x0 2)
                             do
                               (gp:draw-string pane s x y
                                               :font font
                                               :foreground text-color
                                               :block nil) )
                       )))
                 ))
    (augment-display-list pane action nil)
    ))

;; --------------------------------------------

(defun cmplx-plot (pane zs &rest args &key &allow-other-keys)
  (let ((xs (map 'vector 'realpart zs))
        (ys (map 'vector 'imagpart zs)))
    (apply 'plot pane xs ys args)))

(defun cmplx-paramplot (pane dom fn &rest args &key &allow-other-keys)
  (let* ((last-param nil)
         (last-value nil)
         (cfn        (lambda (param)
                       (if (eql param last-param)
                           last-value
                         (setf last-param param
                               last-value (funcall fn param)))))
         (xfn        (lambda (param)
                       (realpart (funcall cfn param))))
         (yfn        (lambda (param)
                       (imagpart (funcall cfn param)))))
    (apply 'paramplot pane dom xfn yfn args)))

(defun polar-fplot (pane dom fn &rest args &key &allow-other-keys)
  (let ((rfn (lambda (th)
               (let ((r (funcall fn th)))
                 (complex (* r (cos th)) (* r (sin th)))))))
    (apply 'cmplx-paramplot pane dom rfn args)))

;; ----------------------------------------------

(defun help ()
  #>.end
:symbol :dot
        :circle
        :box
        :square
        :triangle
        :up-triangle
        :down-triangle
        :right-triangle
        :left-triangle
        :cross
        :sampled-data
        :vbars
        :hbars
        nil - default none

:filled t/f
:plot-joined t/f
:xrange '(lo hi)
:yrange '(lo hi)
:title  "a title string"
:xtitle "an X-axis string"
:ytitle "a Y-axis string"
:color  :red
        :blue
        :darkgreen etc...
:thick  <a number>

:line-type :stepped
            nil - default :interpolated
:legend "legend string"
:alpha  <a 0..1 number>
:cright1 <a string> or nil
:cright2 <a string> or nil
:wmark   default or nil
.end)

;; ----------------------------------------------------------

(defun min-of (xs)
  (reduce 'min xs))

(defun max-of (xs)
  (reduce 'max xs))

(defun spline (pane &rest args)
  (let+ ((:mvb (xs ys parms) (find-x-y-parms args))
         (xlog   (getf parms :xlog))
         (ylog   (getf parms :ylog))
         (spl-xs (cond (xs
                        (if xlog
                            (map 'vector 'log10 xs)
                          (coerce xs 'vector)))
                       (t
                        (let ((sxs (vm:framp (length ys))))
                          (setf xs (if xlog
                                       (map 'vector 'pow10 sxs)
                                     sxs))
                          sxs))
                       ))
         (spl-ys (if ylog
                     (map 'vector 'log10 ys)
                   (coerce ys 'vector)))
         (spl    (interpolation:spline spl-xs spl-ys :natural :natural))
         (dom    (list (min-of xs) (max-of xs)))
         (symbol (getf parms :symbol)))
    (with-delayed-update (pane)
      (apply 'fplot pane dom
             (lambda (x)
               (let ((yval (interpolation:splint spl (if xlog
                                                         (log10 x)
                                                       x))))
                 (if ylog
                     (pow10 yval)
                   yval)))
             :symbol nil
             :symbol-for-legend symbol
             :plot-joined t
             parms)
      (when symbol
        (apply 'plot pane xs ys :clear nil :legend nil :clear nil parms))
      )))

#|
(spline 'junk '(1 5 3 2 7 10) :clear t :symbol :circle :legend "data")
 |#

;; ----------------------------------------------------------------

(defun set-move-augmentation (pane fn)
  (let* ((pane (plotter-pane-of pane)))
    (capi:apply-in-pane-process pane
                                (lambda ()
                                  (setf (plotter-move-augment pane) fn)))
    ))
  
(defun set-click-augmentation (pane fn)
  (let* ((pane (plotter-pane-of pane)))
    (capi:apply-in-pane-process pane
                                (lambda ()
                                  (setf (plotter-click-augment pane) fn)))
    ))
  
;; ----------------------------------------------------------------
#|
(defmacro with-cached-graphing ((pane ver) &body body)
  `(do-with-cached-graphing ,pane ,ver (lambda () ,@body)))

(defun do-with-cached-graphing (pane ver fn)
  (let ((pane (plotter-pane-of pane)))
    (cond ((eql ver (plotter-cache-ver pane))
           (funcall fn))
          
          (t
           (setf (plotter-cache-state pane) nil)
           (funcall fn)
           (let ((mbox (mp:make-mailbox)))
             (capi:apply-in-pane-process pane
               (lambda ()
                 (let ((pixmap (construct-pixmap pane)))
                   (setf (plotter-cache-state pane) :axes)
                   (redraw-display-list pane pixmap 0 0 (gp:port-width pane) (gp:port-height pane) :legend t)
                   (setf (plotter-cache-state pane) :drawing
                         (plotter-cache-ver pane)   ver)
                   (mp:mailbox-send mbox :ok))))
             (mp:mailbox-read mbox)
             ))
          )))

(defun construct-pixmap (pane)
  (let ((pixmap (plotter-cache-pixmap pane)))
    (cond ((and pixmap
                (eql (gp:port-width pane)  (gp:port-width pixmap))
                (eql (gp:port-height pane) (gp:port-height pixmap)))
           pixmap)

          (t
           (when pixmap
             (gp:destroy-pixmap-port pixmap))
           (setf (plotter-cache-pixmap pane)
                 (gp:create-pixmap-port pane
                                        (gp:port-width pane)
                                        (gp:port-height pane)
                                        :background (background-color pane)
                                        :foreground (foreground-color pane))
                 ))
          )))
|#

              

          

(in-package :plotter)

;; -------------------------------------------------------------------

(defparameter *default-args*
  (list
   :watermarkfn 'watermark))

(defmacro with-default-args ((&rest args) &body body)
  `(apply (lambda (&rest *default-args*) ,@body) ,@args *default-args*))

;; -------------------------------------------------------------------

#|
(defun set-reply-mbox (pane mbox)
  (unless (eq mp:*current-process* mp:*main-process*)
    (when mbox
      (loop until (mp:mailbox-empty-p mbox) do
            (mp:mailbox-read mbox))
      (push mbox (reply-mboxes pane))
      )))
|#

(defun add-to-work-order (pane action)
  ;; Set up the pane to peform work in the CAPI thread.
  (append-display-list pane action)
  (redraw-entire-pane pane))

;; -------------------------------------------------------------------

(defun draw-shape (shape pane x0 y0
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
  (let* ((pane      (plotter-mixin-of pane))
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
                     :sweep-angle  sweep-angle))
         (action    (lambda (pane port _x _y _width _height)
                      (declare (ignore _x _y _width _height))
                      (plt-draw-shape pane port shape x0 y0 augm-args))))
    (add-to-work-order pane action)
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
  (multiple-value-bind (xv yv)
      (cond (xv
             (filter-potential-x-y-nans-and-infinities xv yv xlog ylog))
            (yv
             (values nil
                     (filter-potential-nans-and-infinities yv ylog)))
            (t (values nil nil)))
    
    (um:let+ ((pane  (plotter-mixin-of pane args))
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
              (_    (when fresh
                      (discard-display-list pane)
                      (apply 'pw-init-xv-yv pane xv yv augm-args)))
              (:mvb (prepped symbol-fn)  (apply #'prep-vectors pane xv yv augm-args))
              (action    (if fresh
                             (lambda (pane port x y width height)
                               (declare (ignore x y width height))
                               (apply 'pw-axes pane port augm-args)
                               ;; (apply 'pw-plot-xv-yv pane port xv yv augm-args)
                               (apply 'pw-plot-prepped pane port prepped symbol-fn augm-args)
                               )
                           ;; else
                           (lambda (pane port x y width height)
                             (declare (ignore x y width height))
                             ;; (apply 'pw-plot-xv-yv pane port xv yv augm-args)
                             (apply 'pw-plot-prepped pane port prepped symbol-fn augm-args)
                             )
                           )))
      (add-to-work-order pane action)
      )))

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
  (let* ((pane  (plotter-mixin-of pane args))
         (fresh (or clear
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
         (action    (if fresh
                        (lambda (pane port x y width height)
                          (declare (ignore x y width height))
                          (apply 'pw-axes pane port augm-args)
                          (apply 'pw-plot-bars-xv-yv pane port xv yvs augm-args))
                      ;; else
                      (lambda (pane port x y width height)
                        (declare (ignore x y width height))
                        (apply 'pw-plot-bars-xv-yv pane port xv yvs augm-args))
                      )))
    (when fresh
      ;; no drawing in the init, so do it in my thread
      (discard-display-list pane)
      (apply 'pw-init-bars-xv-yv pane xv yvs augm-args))
    (add-to-work-order pane action)
    ))

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
  (let ((pane (plotter-mixin-of pane)))
    (discard-display-list pane)
    (redraw-entire-pane pane)
    ))

;; -------------------------------------------------------------------

(defun stuff-display-list (pane lst)
  ;; be careful here... the list is expected to be a list of lambda forms
  ;; as if from another plotter's display list...
  (let ((pane (plotter-mixin-of pane)))
    (discard-display-list pane)
    (dolist (item lst)
      (vector-push-extend item (plotter-display-list pane)))
    (redraw-entire-pane pane)
    ))
      
;; -------------------------------------------------------------------

(defun axes2 (pane xvector yvectors &rest args &key xrange xlog ylog
                   (logo *ext-logo*)
                   (logo-alpha *ext-logo-alpha*)
                   (cright1 *cright1*)
                   (cright2 *cright2*)
                  
                   &allow-other-keys)
  ;; allow a list of yvectors to be given
  ;; so that we can find the best fitting autoscale that accommodates all of them
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
    (let* ((pane (plotter-mixin-of pane args))
           (augm-args (list*
                       :clear t
                       :logo logo
                       :logo-alpha logo-alpha
                       :cright1 cright1
                       :cright2 cright2
                       args))
           (action  (lambda (pane port x y width height)
                      (declare (ignore x y width height))
                      (apply 'pw-axes pane port augm-args))
                    ))
      ;; do the init setup in our own thread
      (apply 'pw-init-xv-yv pane xv yv augm-args)
      (add-to-work-order pane action)
      )))

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
  (let* ((pane   (plotter-mixin-of pane))
         (action (lambda (pane port xarg yarg width height)
                   (declare (ignore xarg yarg width height))
                   (with-plotview-coords (pane port)
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
                       (with-mask (port (plotter-mask pane))
                         (apply 'draw-string-x-y pane port str
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
    (add-to-work-order pane action)
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
  (let* ((pane  (plotter-mixin-of pane))
         (action (lambda (pane port xarg yarg width height)
                   (declare (ignore xarg yarg width height))
                   (with-plotview-coords (pane port)
                     (let* ((font (find-best-font pane
                                                  :size   font-size
                                                  :family font))
                            (width (loop for s in strs maximize
                                           (multiple-value-bind (left top right bottom)
                                               (gp:get-string-extent port s font)
                                             (declare (ignore top bottom))
                                             (- right left))))
                            (height (multiple-value-bind (left top right bottom)
                                        (gp:get-string-extent port (car strs) font)
                                      (declare (ignore left right))
                                      (- bottom top)))
                            (x0        (get-x-location pane xorg))
                            (y0        (get-y-location pane yorg))
                            (color     (adjust-color pane color alpha))
                            (bcolor    (adjust-color pane border-color border-alpha))
                            (linewidth (adjust-linewidth (or border-thick 0))))
                               
                       (gp:with-graphics-state (port
                                                :foreground color
                                                :thickness  linewidth
                                                :line-end-style   :butt
                                                :line-joint-style :miter
                                                :mask  (plotter-mask pane))
                         (when filled
                           (gp:draw-rectangle port x0 y0
                                              (+ width 4)
                                              (* (length strs) height)
                                              :filled color))
                         (when border-thick
                           (with-color (port bcolor)
                             (gp:draw-rectangle port x0 y0
                                                (+ width 4)
                                                (* (length strs) height)
                                                :filled nil))))
                               
                       (loop for y from (+ height y0 -2) by height
                             for s in strs
                             for x = (+ x0 2)
                             do
                               (gp:draw-string port s x y
                                               :font font
                                               :foreground text-color
                                               :block nil) )
                       )))
                 ))
    (add-to-work-order pane action)
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
    (apply 'plt:cmplx-paramplot pane dom rfn args)))

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
  (multiple-value-bind (xs ys parms)
      (find-x-y-parms args)
    (let* ((xlog   (getf parms :xlog))
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
        (apply 'plot pane xs ys :clear nil :legend nil parms))
      )))

#|
(spline 'junk '(1 5 3 2 7 10) :clear t :symbol :circle :legend "data")
 |#

;; ----------------------------------------------------------------

(defmacro with-cached-graphing ((pane ver) &body body)
  `(do-with-cached-graphing ,pane ,ver (lambda () ,@body)))

(defun do-with-cached-graphing (pane ver fn)
  (let ((pane (plotter-mixin-of pane)))
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
          
              

          

(in-package :plotter)

;; ------------------------------------------
(defun calc-starting-sf (vmin vmax)
  (let ((diff (abs (- vmax vmin))))
    ;; return a starting scale factor so that the range between vmin and vmax
    ;; is greater than 1 but less than 10
    (expt 10 (- (floor (log diff 10))))
    ))

(defun adjust-sf-for-midvalue (sf vmin vmax)
  ;; adjust the scale factor sf, and find the central starting value c
  ;; such that the scaled range between sf*vmin and sf*vmax is >= 5
  ;; and such that c lies between sf*vmin and sf*vmax, and c is a multiple of 10.
  ;;
  ;; NOTE: it is possible that vmin > vmax if the user requests it.
  (let ((vmn (min vmin vmax))
        (vmx (max vmin vmax)))
    (labels ((iterz (sf)
               (if (>= (* sf (- vmx vmn) 5))
                   (list sf 0)
                 (iterz (* 10 sf))))
             (iternz (sf)
               (let ((c (* 10 (ceiling (* sf vmn) 10))))
                 (if (and (<= (* sf vmn) c (* sf vmx))
                          (>= (* sf (- vmx vmn)) 5))
                     (list sf c)
                   (iternz (* 10 sf))
                   ))
               ))
      (if (<= vmn 0 vmx)
          ;; zero is a preferred value for c
          (iterz sf)
        (iternz sf))
      )))

(defun get-delta-sf (vmin vmax)
  (adjust-sf-for-midvalue (calc-starting-sf vmin vmax) vmin vmax))

(defun calc-start-delta (vmin vmax)
  ;; compute a good axis increment and starting value
  ;; these are considered good if the increment is a decade multiple of 1, 2, or 5.
  ;; The starting value must be the largest whole part of the axis values
  ;; in one of these good increments:
  ;; e.g.,
  ;; if the axis ranges from 1.23 to 3.28, then the largest whole part will be 2.00.
  ;; That will be our starting label, and we then number by (non-overlapping strings)
  ;; at increment spacings on either side of that largest whole part.
  ;;
  ;; This avoid bizarre labels like 1.23 ... 1.37 ... 2.45 ...
  ;; giving instead, someting like  1.2 .. 1.6 .. 2.0 .. 2.4 ...
  ;; which is enormously more readable than what most plotting packages produce.
  ;; (This is the way a human would chart the axes)
  ;;
  (destructuring-bind (sf c)
      (get-delta-sf vmin vmax)
    #|
      ;; sf starting = 1/(10^Ceil( log10( Max(|vmin|, |vmax|)))),
      ;; then 10*sf -> sf
      ;; until |sf*vmax - sf*vmin| > 1.
      ;;
      ;; starting value is guaranteed to scale these two (|vmax|, |vmin|) to values
      ;; less than 1. We keep increasing the scale factor by 10 until the difference
      ;; between them, |sf*vmax - sf*vmin| > 1.
      ;;
      ;; We might not have to increase sf by 10,
      ;; e.g., vmin = -5.1, vmax = 6 ==> sf = 1/10, diff = 1.11
      ;;
      ;; Initial scaled span is: 0 <= diff <= 2,
      ;; the min case if vmin = vmax,
      ;; the max case if vmin = -1, vmax = 1.
      ;;
      (loop for sf = (/ (pow10
                         (ceiling (log10 (max (abs vmin)
                                              (abs vmax))
                                         ))
                         ))
            then (* 10.0d0 sf)
            do
            ;;
            ;; this loop finds the scale factor sf and minimum integer value c such that
            ;; the scaled min and max values span a range greater than 1
            ;; and c is no further from the scaled min value than that range.
            ;; It is the case that a <= c <= b, where a and b are the scaled min and max values,
            ;; and abs(c) is some integer multiple (positive, zero, or negative) of 10.
            ;;
            (let* ((a   (* sf vmin))
                   (b   (* sf vmax))
                   (rng (abs (- b a)))
                   (c   (* 10.0d0 (ceiling (min a b) 10.0d0))))
              (if (and (> rng 1.0d0)
                       (<= (abs (- c a)) rng))
                  (return (list sf c)))
              ))
      |#
    
    (loop for sf2 = 1.0d0 then (* 0.1d0 sf2)
          do
          (let* ((a   (* sf sf2 vmin))
                 (b   (* sf sf2 vmax))
                 (c   (* sf2 c))
                 (rng (abs (- b a))))
            
            (if (<= rng 10.0d0)
                (let* ((dv  (cond ((> rng 5.0d0) 1.0d0)
                                  ((> rng 2.0d0) 0.5d0)
                                  (t             0.2d0)))
                       (nl  (floor (abs (- c a)) dv))
                       (nu  (floor (abs (- b c)) dv))
                       (v0  (if (not (plusp (* a b)))
                                0.0d0
                              (/ c sf sf2)))
                       (dv  (/ dv sf sf2)))
                  (return (list v0 dv nl nu)))
              ))
          )))

;; ------------------------------------------
(defparameter *log-subdivs*
  (mapcar #'log10
          '(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
                2 3 4 5 6 7 8 9)))

(defparameter $axis-style
  (make-instance '<plot-style>
                 :line-style (make-instance '<line-style>
                                            :color #.(color:make-gray 0.5))))

(defun trim-mantissa (v)
  (string-right-trim
   "."
   (string-right-trim
    "0" v)))

(defun plabel (val)
  (if (or (zerop val)
          (and (<= 0.01 (abs val))
               (< (abs val) 10000)))
    (trim-mantissa (format nil "~,3F" (float val 1.0)))

    ;; Engineering notation
    (let* ((pwr (* 3 (floor (log (abs val) 10) 3)))
           (v   (/ val (expt 10 pwr))))
      (when (>= (round (abs v)) 1000)
        (incf pwr 3)
        (setf v (* 0.001 v)))
      (concatenate 'string
                   (trim-mantissa (format nil "~,2F" v))
                   (format nil "e~d" pwr))
      )))

(defun funcallable-p (x)
  (or (functionp x)
      (and (symbolp x)
           (ignore-errors
             (symbol-function x)))
      ))

(defmethod pw-axes ((pane <plotter-mixin>) &rest args)
  (cond ((eql (plotter-cache-state pane) :drawing)
         (gp:copy-pixels pane (plotter-cache-pixmap pane)
                         0 0 (gp:port-width pane) (gp:port-height pane) 0 0))
        
        (t
         (apply 'internal-pw-axes pane args))
        ))

(defun internal-pw-axes (pane
                             &key
                             (fullgrid t)
                             (xtitle "X")
                             (ytitle "Y")
                             (title  "Plot")
                             (axes t)
                             (axis-values t)
                             (x-axis-values t)
                             (y-axis-values t)
                             (watermarkfn #'watermark)
                             (logo *ext-logo*)
                             (logo-alpha *ext-logo-alpha*)
                             (cright1 *cright1*)
                             (cright2 *cright2*)
                             x-values
                             &allow-other-keys)
  (recompute-transform pane)
  (let* ((box      (plotter-box pane))
         (xform    (plotter-xform pane))
         (xlog     (plotter-xlog pane))
         (iqxalog  (alogfn xlog))
         (iqxlog   (logfn xlog))
         (iqxmin   (if xlog 1 0))
           
         (ylog     (plotter-ylog pane))
         (iqyalog  (alogfn ylog))
         (iqylog   (logfn ylog))
         (iqymin   (if ylog 1 0)))

    (apply (um:rcurry #'gp:draw-rectangle
                      :filled t
                      :foreground (capi:simple-pane-background pane)
                      :compositing-mode :copy
                      :shape-mode :plain)
           pane (bounding-region pane))
    
    (when watermarkfn
      ;; watermark is affected by scaling and translation of the transform
      (with-plotview-coords (pane)
        (funcall watermarkfn pane logo logo-alpha
                 cright1 cright2)))

    (with-plotview-coords (pane)
      (when title
        (draw-string-x-y pane title
                         (floor (box-width box) 2)
                         -5
                         :x-alignment :center
                         :y-alignment :bottom
                         :font        (find-best-font pane
                                                      :size $big-times-font-size)
                         ))

      (when axes
        ;; draw a bold frame from top-left to bottom-left to bottom-right
        (gp:draw-polygon pane `(0 0 0 ,(box-height box) ,(box-width box) ,(box-height box))))
      
      ;; draw a bold line representing the x-axis at y=0
      (gp:with-graphics-state (pane
                               :mask (plotter-mask pane)
                               :foreground :gray50
                               :thickness 2
                               :scale-thickness t)
        (when (and axis-values
                   y-axis-values)
          ;; (ac:send ac:fmt-println "mask = ~A" (plotting-region pane))
          (let+ ((:mvb (_ y0) (gp:transform-point xform 0 (funcall iqylog iqymin))))
            (gp:draw-polygon pane `(0 ,y0 ,(box-width box) ,y0))
              ))
        
        ;; draw a bold line representing the y-axis at x=0
        (when (and axis-values
                   x-axis-values)
          (let ((x0  (gp:transform-point xform (funcall iqxlog iqxmin) 0)))
            (gp:draw-polygon pane `(,x0 0 ,x0 ,(box-height box)))
            )))
    
      ;; ----------------------------------------------------------------
      ;; Label the x-axis
    
      ;; font is affected by transform scaling
      (let ((font  (find-best-font pane
                                   :size $normal-times-font-size
                                   )))
        (when xtitle
          (draw-string-x-y pane xtitle
                           (floor (box-width box) 2)
                           (+ (box-height box)
                              (if axis-values #+:WIN32 16 #-:WIN32 15 5))
                           :font font
                           :x-alignment :center
                           :y-alignment :top)
          (when (and axis-values
                     x-axis-values)
            (let* ((_xmin   (plotter-xmin pane))
                   (_xmax   (plotter-xmax pane))
                   (_xlast  nil)
                   (_xstart nil))
              (destructuring-bind (x0 dx nl nu) (calc-start-delta _xmin _xmax)
                (declare (ignore nl nu))
                (when xlog
                  (setf dx 1))
                (labels ((xwork (xval xprev)
                           (let* ((xpos  (gp:transform-point xform xval 0))
                                  (ypos  (+ (box-height box) #+:WIN32 2 #-:WIN32 3))
                                  (xlast (draw-string-x-y
                                          pane
                                          (cond ((funcallable-p x-values)
                                                 (funcall x-values xval))
                                                ((consp x-values)
                                                 (elt x-values (round xval)))
                                                (t
                                                 (plabel (funcall iqxalog xval)))
                                                )
                                          xpos ypos
                                          :prev-bounds xprev
                                          :margin 2
                                          :x-alignment :center
                                          :y-alignment :top
                                          :font font)))
                             
                             (when fullgrid
                               (when xlog
                                 (with-color (pane :gray75)
                                   (loop for ix in *log-subdivs* do
                                           (let ((x (gp:transform-point xform (+ xval ix) 0)))
                                             (if (< 0 x (box-width box))
                                                 (gp:draw-line
                                                  pane
                                                  x 0
                                                  x (box-height box))
                                               )))
                                   ))
                               (with-color (pane (if (vectorp fullgrid)
                                                     fullgrid
                                                   (color:make-gray
                                                    (if xlog 0.5 0.75))))
                                 (gp:draw-line pane
                                               xpos 0
                                               xpos (box-height box))
                                 ))
                             
                             (gp:draw-line pane
                                           xpos (- (box-height box) 2)
                                           xpos (+ (box-height box) 3))
                             
                             xlast)))
                  
                  
                  (loop for xval = x0 then (- xval dx)
                        until (< xval (min _xmax _xmin))
                        do
                          (setf _xlast (xwork xval _xlast))
                          (unless _xstart
                            (setf _xstart _xlast)))
                  
                  (setf _xlast _xstart)
                  
                  (loop for xval = (+ x0 dx) then (+ xval dx)
                        until (> xval (max _xmin _xmax))
                        do
                          (setf _xlast (xwork xval _xlast)))
                  )))))
        
        ;; ----------------------------------------------------------------
        ;; Label the y axis
        
        (when ytitle
          (draw-vert-string-x-y pane ytitle
                                #+(AND :WIN32 (NOT (OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8))) 0
                                #+(AND :COCOA (NOT (OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8))) (if axis-values  -3 -15)
                                #+(OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8)                    (if axis-values -15 -20)
                                (floor (box-height box) 2)
                                :font  font
                                :x-alignment :center
                                :y-alignment :top)
          (when (and axis-values
                     y-axis-values)
            (let* ((_ymin   (plotter-ymin pane))
                   (_ymax   (plotter-ymax pane))
                   (_ylast  nil)
                   (_ystart nil))
              (destructuring-bind (y0 dy nl nu) (calc-start-delta _ymin _ymax)
                (declare (ignore nl nu))
                (when ylog
                  (setf dy 1))
                (labels ((ywork (yval yprev)
                           (let+ ((:mvb (_ ypos) (gp:transform-point xform 0 yval))
                                  (xpos  #+:WIN32 -1 #-:WIN32 -3)
                                  (ylast (draw-vert-string-x-y
                                          pane
                                          (plabel (funcall iqyalog yval))
                                          xpos ypos
                                          :prev-bounds yprev
                                          :margin      2
                                          :x-alignment :center
                                          :y-alignment :bottom
                                          :font        font)))
                             
                               (when fullgrid
                                 (when ylog
                                   (with-color (pane :gray75)
                                     (loop for ix in *log-subdivs* do
                                             (let+ ((:mvb (_ y) (gp:transform-point xform 0 (+ yval ix)) ))
                                               (if (> (box-height box) y 0)
                                                   (gp:draw-line
                                                    pane
                                                    0 y
                                                    (box-width box) y)
                                                 )))
                                     ))
                                 (with-color (pane (if (vectorp fullgrid)
                                                       fullgrid
                                                     (color:make-gray
                                                      (if ylog 0.5 0.75))))
                                   (gp:draw-line pane
                                                 0 ypos
                                                 (box-width box) ypos)
                                   ))
                               
                               (gp:draw-line pane
                                             -2 ypos
                                              3 ypos)
                               ylast)))
                  
                  (loop for yval = y0 then (- yval dy)
                        until (< yval (min _ymin _ymax))
                        do
                          (setf _ylast (ywork yval _ylast))
                          (unless _ystart
                            (setf _ystart _ylast)))
                  
                  (setf _ylast _ystart)
                  
                  (loop for yval = (+ y0 dy) then (+ yval dy)
                        until (> yval (max _ymin _ymax))
                        do
                          (setf _ylast (ywork yval _ylast)))
                  ))))
          )))))

#|

(plt:axes 'pltx
          :clear t
          :xrange '(0.02 20) ;; (0.2 20)
          :yrange '(0.02 20)
          :xlog t
          :ylog t)
|#


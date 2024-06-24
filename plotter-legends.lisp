
(in-package :plotter)

;; ----------------------------------------------------------------

(defun internal-draw-existing-legend (pane)
  (let* ((legend (plotter-legend pane))
         (items  (has-content legend)))
    (when items
      (with-plotview-coords (pane)
        (let* ((font1 (find-best-font pane
                                      :size $tiny-times-font-size))
               (font (find-best-font pane
                                     :size $tiny-times-font-size
                                     ;; :size $tiny-times-font-size
                                     ))
               (nitems (length items)))
          
          (multiple-value-bind (txtwd txtht)
              (let ((maxwd   0)
                    (maxht   0))
                (dolist (item items)
                  (multiple-value-bind (lf tp rt bt)
                      (gp:get-string-extent pane (legend item) font)
                    (setf maxwd   (max maxwd (- rt lf))
                          maxht   (max maxht (- bt tp))
                          )))
                (values maxwd maxht))
              
            (let* ((totwd  (+ txtwd  40))
                   (totht  (+ 2 (* nitems txtht)))
                   (effwd  totwd)
                   (effht  totht)
                   (effht1 txtht)
                   (box    (plotter-box pane))
                   (x      (or (preferred-x pane)
                               (let ((x (get-x-location pane (plotter-legend-x pane))))
                                 (ecase (plotter-legend-anchor pane)
                                   (:auto         (if (> x (/ (box-width box) 2))
                                                      (- x effwd)
                                                    x))
                                   ((:nw :w :sw)  (- x effwd))
                                   ((:ne :e :se)  x)
                                   ((:n  :ctr :s) (- x (/ effwd 2)))
                                   ))))
                   (y      (or (preferred-y pane)
                               (let ((y (get-y-location pane (plotter-legend-y pane))))
                                 (case (plotter-legend-anchor pane)
                                   (:auto         (if (> y (/ (box-height box) 2))
                                                      (- y effht)
                                                    y))
                                   ((:nw :n :ne)  y)
                                   ((:sw :s :sw)  (- y effht))
                                   ((:w  :ctr :e) (- y (/ effht 2)))
                                   )))))
                
              (setf (x legend)      x
                    (y legend)      y
                    (width legend)  totwd
                    (height legend) totht)
                
              (gp:draw-rectangle pane x y effwd effht
                                 :filled t
                                 :foreground (adjust-color pane
                                                           (background-color pane)
                                                           0.75))
                
              (gp:draw-rectangle pane x y effwd effht
                                 :foreground (if (highlighted legend)
                                                 :magenta
                                               :black))
                
              (loop for item in items
                    for y from (+ y effht1 1) by effht1
                    do
                      (let* ((line-style   (line-style   item))
                             (symbol-style (symbol-style item)))
                          
                        ;; ---------------------------------------------
                        (labels ((draw-line (&optional thickness)
                                   (gp:with-graphics-state
                                       (pane
                                        :thickness  (or thickness
                                                        (line-thick line-style))
                                        :dashed     (and line-style
                                                         (line-dashing line-style))
                                        :dash       (and line-style
                                                         (line-dashing line-style))
                                        :foreground (if line-style
                                                        (adjust-color pane
                                                                      (line-color line-style)
                                                                      (line-alpha line-style))
                                                      (adjust-color pane
                                                                    (fill-color symbol-style)
                                                                    (fill-alpha symbol-style))))
                                     (let ((y (floor (- y (/ effht1 2)))))
                                       (gp:draw-line pane
                                                     (+ x  3) y
                                                     (+ x 33) y)
                                       ))))
                            
                          ;; ---------------------------------------------
                          (cond  (symbol-style
                                  (case (plot-symbol symbol-style)
                                    ((:vbars :hbars) (draw-line 5))
                                    (otherwise
                                     (when line-style
                                       (draw-line))
                                     (funcall (get-symbol-plotfn pane symbol-style)
                                              (+ x 18) (- y (/ effht1 2))
                                              ))
                                    ))
                                   
                                 (line-style (draw-line))
                                 ))
                          
                        ;; ---------------------------------------------
                        (gp:draw-string pane (legend item) (+ x 36) (- y 3)
                                        :font font1)
                        ))
              ))
          ))
      )))

(defun draw-accumulated-legend (pane)
  (let ((items  (all-legends pane))
        (legend (plotter-legend pane)))
    
    (cond ((null items)
           (setf (has-content legend) nil))
          
          ((activep legend)
           (setf (has-content legend) items)
           (internal-draw-existing-legend pane))
          )))

(defun refresh-view (pane x y w h)
  ;; Code to convert a plotter region rectangle to an absolute,
  ;; scaled, rectangle in the parent graphport.
  ;;
  ;; CAPI:REDISPLAY-ELEMENT is great, but it doesn't know anything
  ;; about graphics transforms.
  (let ((xform (gp:make-transform))
        (sf    (plotter-sf pane))
        (box   (plotter-box pane)))
    (gp:apply-translation xform (box-left box) (box-top box))
    (gp:apply-scale xform sf sf)
    (multiple-value-bind (xp yp)
        (gp:transform-point xform x y)
      (multiple-value-bind (rp bp)
          (gp:transform-point xform (+ x w) (+ y h))
        (capi:redisplay-element pane xp yp (- rp xp) (- bp yp))
        ))
    ))

(defun draw-existing-legend (pane legend)
  (let* ((extra   2)
         (extra*2 (* 2 extra))
         (w       (ceiling (+ extra*2 (width legend))))
         (h       (ceiling (+ extra*2 (height legend))))
         (x       (floor (- (or (preferred-x pane)
                                (x legend))
                            extra)))
         (y       (floor (- (or (preferred-y pane)
                                (y legend))
                            extra))))
    (refresh-view pane x y w h)
    ))

(defun restore-legend-background (pane legend)
  (let* ((extra   2)
         (extra*2 (* 2 extra))
         (w       (ceiling (+ extra*2 (width legend))))
         (h       (ceiling (+ extra*2 (height legend))))
         (x-old   (floor (- (x legend) extra)))
         (y-old   (floor (- (y legend) extra))))
    (refresh-view pane x-old y-old w h)
    ))

(defun highlight-legend (pane)
  (let ((legend (plotter-legend pane)))
    (when (activep legend)
      (unless (highlighted legend)
        (setf (highlighted legend) t)
        (restore-legend-background pane legend)
        (draw-existing-legend pane legend)
        ))))

(defun unhighlight-legend (pane)
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend)
               (highlighted legend))
      (setf (highlighted legend) nil)
      (restore-legend-background pane legend)
      (draw-existing-legend pane legend)
      )))

(defun window-to-plotview-coords (pane x y)
  ;; Convert reported screen coords to nominal plotting region coords.
  ;; Effectively, this just makes the system resize-scaling agnostic.
  (multiple-value-bind (xd yd)
      ;; back to data coords
      (gp:transform-point (plotter-inv-xform pane) x y)
    ;; then to nominal frame coords
    (gp:transform-point (plotter-xform pane) xd yd)
    ))

(defun on-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (and (activep legend)
         (has-content legend)
         (multiple-value-bind (xp yp)
             (window-to-plotview-coords pane x y)
           (with-accessors ((xl   x)
                            (yl   y)
                            (wd   width)
                            (ht   height)) legend
             (and (<= xl xp (+ xl wd))
                  (<= yl yp (+ yl ht)))
             ))
         )))

(defun start-drag-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend)
               (has-content legend))
      (multiple-value-bind (xp yp)
          (window-to-plotview-coords pane x y)
        (setf (dragging legend) t
              (dx legend) (- (x legend) xp)
              (dy legend) (- (y legend) yp))
        ))))

(defun undrag-legend (pane x y)
  (declare (ignore x y))
  (let ((legend (plotter-legend pane)))
    (when (dragging legend)
      (setf (dragging legend) nil)
      )))

(defun drag-legend (pane x y)
  (let ((legend (plotter-legend pane)))
    (when (dragging legend)
      (restore-legend-background pane legend)
      (multiple-value-bind (xp yp)
          (window-to-plotview-coords pane x y)
        (setf (preferred-x pane) (+ xp (dx legend))
              (preferred-y pane) (+ yp (dy legend)))
        (draw-existing-legend pane legend)
        ))))


#|
(setf plt (plt:wset 'plt))
(plt:fplot plt '(-10 10) (lambda (x) (/ (sin x) x))
           :thick 2
           :legend "Sinc(x)"
           :clear t)

(defun draw-test-pane (gp pane
                          x y
                          width height)
  
(defclass test-pane (capi:drawn-pinboard-object)
  ()
  (:default-initargs
   :display-callback 'draw-test-pane
   :visible-min-width 50
   :visible-min-height 50))

(capi:contain
 (make-instance 'test-pane
                :best-width 400
                :best-height 300))
|#

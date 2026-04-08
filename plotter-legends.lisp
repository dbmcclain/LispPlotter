
(in-package :plotter)

;; ----------------------------------------------------------------

(defun internal-draw-existing-legend (port plt)
  (let* ((legend (plotter-legend plt))
         (items  (has-content legend)))
    (when items
      (with-plotview-coords (port plt)
        (let* ((font1 (find-best-font port
                                      :size $tiny-times-font-size))
               (font (find-best-font port
                                     :size $tiny-times-font-size
                                     ;; :size $tiny-times-font-size
                                     ))
               (nitems (length items)))
          
          (multiple-value-bind (txtwd txtht)
              (let ((maxwd   0)
                    (maxht   0))
                (dolist (item items)
                  (multiple-value-bind (lf tp rt bt)
                      (gp:get-string-extent port (legend item) font)
                    (setf maxwd   (max maxwd (- rt lf))
                          maxht   (max maxht (- bt tp))
                          )))
                (values maxwd maxht))
              
            (let* ((totwd  (+ txtwd  40))
                   (totht  (+ 2 (* nitems txtht)))
                   (effwd  totwd)
                   (effht  totht)
                   (effht1 txtht)
                   (box    (plotter-box plt))
                   (x      (or (preferred-x plt)
                               (let ((x (get-x-location plt (plotter-legend-x plt))))
                                 (ecase (plotter-legend-anchor plt)
                                   (:auto         (if (> x (/ (box-width box) 2))
                                                      (- x effwd)
                                                    x))
                                   ((:nw :w :sw)  (- x effwd))
                                   ((:ne :e :se)  x)
                                   ((:n  :ctr :s) (- x (/ effwd 2)))
                                   ))))
                   (y      (or (preferred-y plt)
                               (let ((y (get-y-location plt (plotter-legend-y plt))))
                                 (case (plotter-legend-anchor plt)
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
                
              (gp:draw-rectangle port x y effwd effht
                                 :filled t
                                 :foreground (adjust-color port
                                                           (background-color port)
                                                           0.75))
                
              (gp:draw-rectangle port x y effwd effht
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
                                       (port
                                        :thickness  (or thickness
                                                        (line-thick line-style))
                                        :dashed     (and line-style
                                                         (line-dashing line-style))
                                        :dash       (and line-style
                                                         (line-dashing line-style))
                                        :foreground (if line-style
                                                        (adjust-color port
                                                                      (line-color line-style)
                                                                      (line-alpha line-style))
                                                      (adjust-color port
                                                                    (fill-color symbol-style)
                                                                    (fill-alpha symbol-style))))
                                     (let ((y (floor (- y (/ effht1 2)))))
                                       (gp:draw-line port
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
                                     (funcall (get-symbol-plotfn port symbol-style)
                                              (+ x 18) (- y (/ effht1 2))
                                              ))
                                    ))
                                   
                                 (line-style (draw-line))
                                 ))
                          
                        ;; ---------------------------------------------
                        (gp:draw-string port (legend item) (+ x 36) (- y 3)
                                        :font font1)
                        ))
              ))
          ))
      )))

(defun draw-accumulated-legend (plt)
  (let ((items  (all-legends plt))
        (legend (plotter-legend plt)))
    (cond ((null items)
           (setf (has-content legend) nil))

          ((activep legend)
           (setf (has-content legend) items)
           (internal-draw-existing-legend (display-pane-of plt) plt))
          )))

(defun refresh-view (plt x y w h)
  ;; Code to convert a plotter region rectangle to an absolute,
  ;; scaled, rectangle in the parent graphport.
  ;;
  ;; CAPI:REDISPLAY-ELEMENT is great, but it doesn't know anything
  ;; about graphics transforms.
  (let ((xform (gp:make-transform))
        (sf    (plotter-sf plt))
        (box   (plotter-box plt)))
    (gp:apply-translation xform (box-left box) (box-top box))
    (gp:apply-scale xform sf sf)
    (multiple-value-bind (xp yp)
        (gp:transform-point xform x y)
      (multiple-value-bind (rp bp)
          (gp:transform-point xform (+ x w) (+ y h))
        (capi:redisplay-element plt xp yp (- rp xp) (- bp yp))
        ))
    ))

(defun draw-existing-legend (plt legend)
  (let* ((extra   2)
         (extra*2 (* 2 extra))
         (w       (ceiling (+ extra*2 (width legend))))
         (h       (ceiling (+ extra*2 (height legend))))
         (x       (floor (- (or (preferred-x plt)
                                (x legend))
                            extra)))
         (y       (floor (- (or (preferred-y plt)
                                (y legend))
                            extra))))
    (refresh-view plt x y w h)
    ))

(defun restore-legend-background (plt legend)
  (let* ((extra   2)
         (extra*2 (* 2 extra))
         (w       (ceiling (+ extra*2 (width legend))))
         (h       (ceiling (+ extra*2 (height legend))))
         (x-old   (floor (- (x legend) extra)))
         (y-old   (floor (- (y legend) extra))))
    (refresh-view plt x-old y-old w h)
    ))

(defun highlight-legend (plt)
  (let ((legend (plotter-legend plt)))
    (when (activep legend)
      (unless (highlighted legend)
        (setf (highlighted legend) t)
        (restore-legend-background plt legend)
        (draw-existing-legend plt legend)
        ))))

(defun unhighlight-legend (plt)
  (let ((legend (plotter-legend plt)))
    (when (and (activep legend)
               (highlighted legend))
      (setf (highlighted legend) nil)
      (restore-legend-background plt legend)
      (draw-existing-legend plt legend)
      )))

(defun window-to-plotview-coords (plt x y)
  ;; Convert reported screen coords to nominal plotting region coords.
  ;; Effectively, this just makes the system resize-scaling agnostic.
  (multiple-value-bind (xd yd)
      ;; back to data coords
      (gp:transform-point (plotter-inv-xform plt) x y)
    ;; then to nominal frame coords
    (gp:transform-point (plotter-xform plt) xd yd)
    ))

(defun on-legend (plt x y)
  (let ((legend (plotter-legend plt)))
    (and (activep legend)
         (has-content legend)
         (multiple-value-bind (xp yp)
             (window-to-plotview-coords plt x y)
           (with-accessors ((xl   x)
                            (yl   y)
                            (wd   width)
                            (ht   height)) legend
             (and (<= xl xp (+ xl wd))
                  (<= yl yp (+ yl ht)))
             ))
         )))

(defun start-drag-legend (plt x y)
  (let ((legend (plotter-legend plt)))
    (when (and (activep legend)
               (has-content legend))
      (multiple-value-bind (xp yp)
          (window-to-plotview-coords plt x y)
        (setf (dragging legend) t
              (dx legend) (- (x legend) xp)
              (dy legend) (- (y legend) yp))
        ))))

(defun undrag-legend (plt x y)
  (declare (ignore x y))
  (let ((legend (plotter-legend plt)))
    (when (dragging legend)
      (setf (dragging legend) nil)
      )))

(defun drag-legend (plt x y)
  (let ((legend (plotter-legend plt)))
    (when (dragging legend)
      (restore-legend-background plt legend)
      (multiple-value-bind (xp yp)
          (window-to-plotview-coords plt x y)
        (setf (preferred-x plt) (+ xp (dx legend))
              (preferred-y plt) (+ yp (dy legend)))
        (draw-existing-legend plt legend)
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

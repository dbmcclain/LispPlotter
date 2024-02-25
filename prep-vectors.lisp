;; prep-vectors.lisp - Preprocess data vectors into PlotView pixel-space vectors
;;
;; DM/RAL 02/24
;; ---------------------------------------------------------------

(in-package :plotter)

;; Precompute a pixel-space list of PlotView (x,y) coords suitable for
;; use by GP:DRAW-POLYGON.
;;
;; This transforms data points from data space to pixel space within
;; the PlotView. That mapping does not change when the frame shrinks
;; or grows, because that scaling occurs afterward during rendering.
;;
;; Doing this offloads a huge amount of work from the CAPI Main
;; thread. All it needs to do is blast through the list of prepped
;; coords and perform graphics state transforms along the way. That
;; would happen in any event.
;;
(defun prep-vectors (pane xvector yvector
                          &key
                          plot-style
                          symbol-for-legend
                          plot-joined
                          &allow-other-keys)
  (let+ ((line-style   (line-style plot-style))
         (symbol-style (and (not symbol-for-legend)
                            (symbol-style plot-style)))
         (nel          (if xvector
                           (min (length-of xvector) (length-of yvector))
                         (length-of yvector)))
         (xlog         (plotter-xlog pane))
         (xlogfn       (logfn xlog))
         (ylog         (plotter-ylog pane))
         (ylogfn       (logfn ylog))
         (xs           (let ((scanner (make-scanner (or xvector
                                                        nel))
                                      ))
                         (if xlog
                             (make-transformer scanner xlogfn)
                           scanner)))
         (ys           (let ((scanner (make-scanner yvector)))
                         (if ylog
                             (make-transformer scanner ylogfn)
                           scanner)))
         (pairs        (make-pair-scanner xs ys))
         (xform        (plotter-xform pane))
         (xfpairs      (make-gpxform-pairs pairs xform))
         (:mvb (x0 y0) (gp:transform-point xform 0 0)))
    (flet ((line-pairs ()
             (let* ((xfpairs (case plot-joined
                               ((:spline)
                                (make-gpxform-pairs xform
                                                    (make-pairs-for-spline pairs)))
                               (otherwise
                                xfpairs))
                             ))
               (collect-pairs xfpairs))
             ))
      ;; Mirror the decision logic used by the renderer, PW-PLOT-PREPPED.
      (cond (symbol-style
             (case (plot-symbol symbol-style)
               (:vbars
                (if (bar-width symbol-style)
                    (let* ((wd   (get-x-width pane (bar-width symbol-style)))
                           (off  (if (bar-offset symbol-style)
                                     (get-x-width pane (bar-offset symbol-style))
                                   0)
                                 ))
                      (vbar-rects xfpairs wd off y0))
                      
                  ;; else
                  (histo-vbars-pairs xfpairs y0)
                  ))
                
               
               (:hbars
                (if (bar-width symbol-style)
                    (let* ((wd   (get-y-width pane (bar-width symbol-style)))
                           (off  (if (bar-offset symbol-style)
                                     (get-y-width pane (bar-offset symbol-style))
                                   0)
                                 ))
                      (hbar-rects xfpairs wd off x0))
                  ;; else
                  (histo-hbars-pairs xfpairs x0)
                  ))
               
               (:sampled-data
                (values (line-pairs)
                        (get-symbol-plotfn pane (symbol-style plot-style))
                        ))
               
               (otherwise
                (values (line-pairs)
                        (get-symbol-plotfn pane symbol-style)
                        ))))
            
            (line-style
             (case (line-type line-style)
               (:stepped
                (stairstep-pairs xfpairs))
               
               (:histo
                (histo-pairs xfpairs))
               
               (otherwise
                (line-pairs))
               ))
            ))
    ))

;; -------------------------------------------------------------
;; Used by
;;    :SYMBOL :SAMPLED-DATA
;;    :SYMBOL <sym>

(defun do-with-pts (lst fn)
  (um:nlet iter ((lst  lst))
    (unless (endp lst)
      (let+ (( (x y . rest) lst))
        (funcall fn x y)
        (go-iter rest))
      )))

(defmacro with-pts ((lst x y) &body body)
  `(do-with-pts ,lst (lambda (,x ,y)
                       ,@body)))

;; ------------------------------------------------------
;; Used by
;;    :SYMBOL :VBARS
;;    :SYMBOL :HBARS
;;
;; when the bars have user selected :BAR-WIDTH and :BAR-OFFSET.

(defun do-with-rects (lst fn)
  (um:nlet iter ((lst lst))
    (unless (endp lst)
      (let+ (( (x y w h . rest) lst))
        (funcall fn x y w h)
        (go-iter rest)))
    ))

(defmacro with-rects ((lst x y w h) &body body)
  `(do-with-rects ,lst (lambda (,x ,y ,w ,h)
                         ,@body)))

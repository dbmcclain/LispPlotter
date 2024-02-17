
(in-package :plotter)

(defun prep-vectors (pane xvector yvector
                          &key
                          plot-style
                          symbol-for-legend
                          plot-joined
                          &allow-other-keys)
  (um:let+ ((line-style   (line-style plot-style))
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
            (xfpairs      (make-gpxform-pairs xform pairs))
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
                    (let* ((wd   (get-y-width port (bar-width symbol-style)))
                           (off  (if (bar-offset symbol-style)
                                     (get-y-width port (bar-offset symbol-style))
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

(defun do-with-pts (lst fn)
  (um:nlet iter ((lst  lst))
    (unless (endp lst)
      (destructuring-bind (x y . rest) lst
        (funcall fn x y)
        (go-iter rest))
      )))

(defmacro with-pts ((lst x y) &body body)
  `(do-with-pts ,lst (lambda (,x ,y)
                       ,@body)))

(defun do-with-rects (lst fn)
  (um:nlet iter ((lst lst))
    (unless (endp lst)
      (destructuring-bind (x y w h . rest) lst
        (funcall fn x y w h)
        (go-iter rest)))
    ))

(defmacro with-rects ((lst x y w h) &body body)
  `(do-with-rects ,lst (lambda (,x ,y ,w ,h)
                         ,@body)))


(in-package :plotter)

;; ------------------------------------------
(defconstant $gray-colormap
  (let ((map (make-array 256)))
    (loop for ix from 0 to 255 do
          (setf (aref map ix) (color:make-gray (/ (float ix) 255.0))
                ))
    map)
  "A gray-scale colormap.")

(defconstant $heat-colormap
  (let ((map (make-array 256)))
    (labels ((clip (v)
               (max 0.0 (min 1.0 (/ v 255)))))
      (loop for ix from 0 to 255 do
            (setf (aref map ix)
                  (color:make-rgb
                   (clip (/ (* 255 ix) 176))
                   (clip (/ (* 255 (- ix 120)) 135))
                   (clip (/ (* 255 (- ix 190)) 65)))
                  )))
    map)
  "A reddish to white colormap.")

(defparameter *current-colormap* $heat-colormap ;;$gray-colormap
  "The colormap currently in effect for TVSCL and Plot-Image.")

(defparameter *tst-img*
  (let ((img (make-array '(64 64))))
    (loop for row from 0 below 64 do
          (loop for col from 0 below 64 do
                (setf (aref img row col) (* row col))
                ))
    img))

(defun do-tvscl (pane arr
              &key (magn 1)
              (colormap *current-colormap*)
              reply-mbox
              zrange
              &allow-other-keys)
  "Internal workhorse routine for TVSCL."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list
       pane
       (lambda (pane port x y width height)
         (declare (ignore x y width height))
         (let* ((wd   (array-dimension-of arr 1))
                (ht   (array-dimension-of arr 0))
                (mn   (or (and zrange (reduce #'min zrange)) (vmin-of arr)))
                (mx   (or (and zrange (reduce #'max zrange)) (vmax-of arr)))
                (sf   (let ((diff (- mx mn)))
                        (if (zerop diff)
                            0
                          (/ 255 diff))))
                (xcolors (make-array 256)))

           (labels ((convert-to-color (v)
                      (let* ((vlim (max mn (min mx v)))
                             (cix  (round (* sf (- vlim mn)))))
                        (declare (fixnum cix))
                        (or (aref xcolors cix)
                            (setf (aref xcolors cix)
                                  (color:convert-color pane
                                                       (aref colormap cix)))) )))
             
             ;; this scaling gives the origin at the ULC
             ;; with positive Y values downward, positive X values rightward
             (pw-init-xv-yv pane (vector 0 wd) (vector 0 ht)
                            :xrange `(0 ,wd)
                            :yrange `(,ht 0)
                            :box    `(0 0 ,(* magn wd) ,(* magn ht)))
             
             (with-image (port (img #+:COCOA (gp:make-image port wd ht)
                                    #+:WIN32 (gp:make-image port wd ht :alpha nil)
                                    ))
               (with-image-access (acc (gp:make-image-access port img))
                 (loop for row from 0 below ht do
                       (loop for col from 0 below wd do
                             (setf (gp:image-access-pixel acc col row)
                                   (convert-to-color (aref-of arr row col)))
                             ))
                 (gp:image-access-transfer-to-image acc))

               (let ((sf (* magn (plotter-sf pane))))
                 (gp:with-graphics-scale (port sf sf)
                   (gp:draw-image port img 0 0))
                 ))
             (setf (plotter-magn pane) magn)
             )))
       ))))

;; user callable routine
(defun tvscl (pane arr &rest args)
  "Display an image array of z-values in the specified pane.
   This is distinct from plotting an image with user specified
   X and Y values. TVSCL always uses implicit pixel coordinates
   for its X and Y values."
  (apply 'do-tvscl pane arr (append args *default-args*)))

(defun do-plot-image (pane xv yv arr
                           &rest args
                           &key
                           (colormap *current-colormap*)
                           reply-mbox
                           zlog
                           zrange
                           &allow-other-keys)
  "Internal workhorse for image plotting."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (discard-display-list pane)
      (append-display-list 
       pane
       (lambda (pane port x y width height)
         (declare (ignore x y width height))
         (apply 'pw-init-xv-yv pane xv yv args)
         (gp:clear-graphics-port port)
         (let* ((wd   (array-dimension-of arr 1))
                (ht   (array-dimension-of arr 0))
                (mn   (let ((v (if zrange
                                   (first zrange)
                                 (vmin-of arr))))
                        (if zlog
                          (log (1+ v))
                          v)))
                (mx   (let ((v (if zrange
                                   (second zrange)
                                 (vmax-of arr))))
                        (if zlog
                          (log (1+ v))
                          v)))
                (gsf  (let ((diff (- mx mn)))
                        (if (zerop diff)
                          0
                          (/ 255 diff))))
                (sf        (plotter-sf  pane))
                (box       (let ((box (plotter-box pane)))
                             (adjust-box
                              (list (1+ (* sf (box-left box)))
                                    (* sf (box-top box))
                                    (1- (* sf (box-width box)))
                                    (* sf (box-height box))))
                             ))
                (xcolors  (make-array 256)))

           (labels ((convert-to-color (v)
                      (let* ((vlim (max mn (min mx v)))
                             (cix  (round (* gsf (- (if zlog
                                                        (log (1+ vlim))
                                                      vlim)
                                                    mn)))))
                        (declare (fixnum cix))
                        (or (aref xcolors cix)
                            (setf (aref xcolors cix)
                                  (color:convert-color pane
                                                       (aref colormap cix)))) )))

             (with-image (port (img #+:COCOA (gp:make-image port wd ht)
                                    #+:WIN32 (gp:make-image port wd ht :alpha nil)
                                    ))
               (with-image-access (acc (gp:make-image-access port img))
                 (loop for row from 0 below ht do
                       (loop for col from 0 below wd do
                             (setf (gp:image-access-pixel acc col (- ht row 1))
                                   (convert-to-color (aref-of arr row col)))
                             ))
                 (gp:image-access-transfer-to-image acc))

               (multiple-value-bind (px py)
                   (gp:transform-point (plotter-xform pane)
                                       (if (plotter-xlog pane)
                                           (log10 (elt xv 0))
                                         (elt xv 0))
                                       (if (plotter-ylog pane)
                                           (log10 (elt yv 0))
                                         (elt yv 0)))
                 (multiple-value-bind (px2 py2)
                     (gp:transform-point (plotter-xform pane)
                                         (if (plotter-xlog pane)
                                             (log10 (elt xv 1))
                                           (elt xv 1))
                                         (if (plotter-ylog pane)
                                             (log10 (elt yv 1))
                                           (elt yv 1)))
                   (let ((plt-wd (1+ (- px2 px)))
                         (plt-ht (1+ (- py py2))))
                     ;; (print (list plt-wd plt-ht))
                     (gp:with-graphics-scale (port sf sf)
                       (gp:with-graphics-state (port
                                                :mask box)
                         (gp:draw-image port img
                                        px py2
                                        :from-width  wd
                                        :from-height ht
                                        :to-width    plt-wd
                                        :to-height   plt-ht
                                        ))))
                   ))
               )))
         (apply 'pw-axes pane port :clear nil args)
         )))))

(defun plot-image (pane xv yv image-arr &rest args)
  "Plot an image in the specified pane using the sequences xv and yv
   for the X and Y scales. The image array holds the z-values."
  (apply 'do-plot-image pane xv yv image-arr (append args *default-args*)))
  
#|
;; tst plot-image
(let* ((nx  32)
       (ny  8)
       (arr (make-array (list (1+ ny) (1+ nx))
                       :element-type 'single-float
                       :initial-element 0.0)))
  (loop for iy from 0 to ny do
        (loop for ix from 0 to nx do
              (setf (aref arr iy ix) (float (* ix iy) 1.0))))
  ;; (loop for ix from 0 to 50 do (setf (aref arr ix (round ix 2)) 0.0))
  (let ((plt (wset 'plt :background :black :foreground :white)))
    (plot-image plt '(0 100) '(0 100) arr :watermarkfn nil)))
|#

#|
(defvar *dbg* (debug-stream:make-debug-stream
               :display t
               :title "Debugger Window"
               ))

(defun pdbg (fmt &rest args)
  (debug-stream:debug-print *dbg* (apply #'format nil fmt args)))
|#

(defun do-render-image (pane ext-img
                     &key
                     (magn 1)
                     (to-x 0)
                     (to-y 0)
                     (from-x 0)
                     (from-y 0)
                     to-width
                     to-height
                     from-width
                     from-height
                     transform
                     global-alpha
                     reply-mbox
                     &allow-other-keys)
  "Internal workhorse for image rendering."
  (let ((pane (plotter-mixin-of pane)))
    (with-delayed-update (pane)
      (set-reply-mbox pane reply-mbox)
      (append-display-list
       pane
       #'(lambda (pane port x y wd ht)
           (declare (ignore x y wd ht))
           (progn ;; let ((sf (* magn (plotter-sf pane))))
             (with-image (port (img (gp:convert-external-image port ext-img)))
               (let* ((from-width  (or from-width  (gp:image-width  img)))
                      (from-height (or from-height (gp:image-height img)))
                      (to-width    (or to-width
                                       (if (>= from-width from-height)
                                           (gp:port-width port)
                                         (* (gp:port-height port)
                                            (/ from-width from-height)))))
                      (to-height   (or to-height
                                       (if (>= from-height from-width)
                                           (gp:port-height port)
                                         (* (gp:port-width port)
                                            (/ from-height from-width))))))
                 #|
                 (pdbg "i-wd: ~A, i-ht: ~A, p-wd: ~A, p-ht: ~A"
                       from-width from-height
                       to-width to-height)
                 |#
                 (progn ;; gp:with-graphics-scale (port sf sf)
                   (setf (plotter-box pane) `(0 0 ,to-width ,to-height)
                         (plotter-xmin pane) 0
                         (plotter-ymin pane) 0
                         (plotter-xmax pane) from-width
                         (plotter-ymax pane) from-width
                         (plotter-xlog pane) nil
                         (plotter-ylog pane) nil)
                   (let ((xform (gp:make-transform))
                         (inv-xform (gp:make-transform)))
                     (gp:apply-scale xform 1 -1)
                     (gp:apply-translation xform 0 to-height)
                     (gp:invert-transform xform inv-xform)
                     (setf (plotter-xform pane) xform
                           (plotter-inv-xform pane) inv-xform))
                     
                   #|(pw-init-xv-yv pane (vector 0 from-width) (vector 0 from-height)
                                  :aspect 1
                                  :box    (list 0 0 to-height to-width))|#
                   (gp:draw-image port img to-x to-y
                                  :transform    transform
                                  :from-x       from-x
                                  :from-y       from-y
                                  :to-width     to-width
                                  :to-height    to-height
                                  :from-width   from-width
                                  :from-height  from-height
                                  :global-alpha global-alpha)
                   )))
             (setf (plotter-magn pane) magn)
             )))
      )))

;; user callable routine
(defun render-image (pane ext-img &rest args)
  "Render an external image in the specified window pane."
  (apply #'do-render-image pane ext-img (append args *default-args*)))


(defvar *last-image-path* nil
  "Holds the last used folder path for read-image.")

(defun read-image (&optional filename)
  "Read an external image file. If the filename argument is elided,
   CAPI will prompt for one."
  (let ((path (or filename
                  (capi:prompt-for-file
                   "Select Image File"
                   :filter "*.*"
                   :pathname *last-image-path*))))
    (when path
      (setf *last-image-path* path)
      (gp:read-external-image path))
    ))


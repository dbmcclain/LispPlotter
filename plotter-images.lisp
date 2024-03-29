
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
    (labels ((clipx (v)
               (clip (/ v 255) 0.0 1.0)))
      (loop for ix from 0 to 255 do
            (setf (aref map ix)
                  (color:make-rgb
                   (clipx (/ (* 255 ix) 176))
                   (clipx (/ (* 255 (- ix 120)) 135))
                   (clipx (/ (* 255 (- ix 190)) 65)))
                  )))
    map)
  "A reddish to white colormap.")

(defparameter *current-colormap* $heat-colormap ;;$gray-colormap
  "The colormap currently in effect for TVSCL and Plot-Image.")

(defparameter *tst-img*
  (let ((img (make-array '(64 64))))
    (loop for row from 0 below 64 do
          (loop for col from 0 below 64 do
                (setf (aref img row col) (abs (complex row col)))
                ))
    img))

;; -----------------------------------------------------

(defun do-convert-array-to-color-image-for-pane (arr pane continuation
                                                      &key
                                                      (colormap *current-colormap*)
                                                      first-row
                                                      zrange
                                                      zlog
                                                      flipv
                                                      fliph
                                                      &allow-other-keys)
  (let* ((wd   (array-dimension-of arr 1))
         (ht   (array-dimension-of arr 0))
         (first-row (or first-row
                        (1- ht))))
    (declare (fixnum wd ht first-row))
    
    (with-image (pane (img #-:WIN32 (gp:make-image pane wd ht)
                           #+:WIN32 (gp:make-image pane wd ht :alpha nil)
                           ))
      (with-image-access (acc (gp:make-image-access pane img))
        
        (labels ((z-value (z)
                   (if zlog
                       (log (1+ z))
                     z)))
           
          (destructuring-bind (mn mx)
              (mapcar #'z-value
                      (or zrange
                          (multiple-value-list (vextrema-of arr))))
      
            (let* ((gsf  (let ((diff (- mx mn)))
                           (if (zerop diff)
                               0
                             (/ 255 diff))))
                   (xcolors  (rest (or (let ((cmap (cached-cmap pane)))
                                         (and cmap
                                              (eql colormap (first cmap))
                                              cmap))
                                       (setf (cached-cmap pane)
                                             (cons colormap (make-array 256))) ))))
              
              (labels ((convert-to-color (v)
                         (let* ((cix  (round (* gsf (- (clip (z-value v) mn mx) mn)))))
                           (declare (fixnum cix))
                           
                           (or (aref xcolors cix)
                               (setf (aref xcolors cix)
                                     (color:convert-color pane
                                                          (aref colormap cix)))) ))
                       
                       (xfer-line (src-row dst-row)
                         (declare (fixnum src-row dst-row))
                         
                         (labels ((xfer-pixel (src-col dst-col)
                                    (declare (fixnum src-col dst-col))
                                    (setf (gp:image-access-pixel acc dst-col dst-row)
                                          (convert-to-color
                                           (aref-of arr src-row src-col)))))
                           (if fliph
                               (loop for src-col fixnum from (1- wd) downto 0
                                     for dst-col fixnum from 0 do
                                     (xfer-pixel src-col dst-col))
                             ;; else
                             (loop for col fixnum from 0 below wd do
                                   (xfer-pixel col col)) ))))
              
                ;; split the conversion into two portions to allow
                ;; for scrolling waterfall displays. Top line indicated by
                ;; keyword parameter :first-row

                ;; NOTE: image addressing is upside down vertically.
                ;; We display, by default (- not flipped -), so that
                ;; the origin of the array is shown at the lower left
                ;; corner.
                
                (if flipv
                    (progn
                      (loop for src-row fixnum from first-row downto 0
                            for dst-row fixnum from (1- ht) by -1 do
                            (xfer-line src-row dst-row))
                      (loop for src-row fixnum from (1- ht) above first-row
                            for dst-row fixnum from (- ht first-row 2) by -1 do
                            (xfer-line src-row dst-row)))
                  ;; else
                  (progn
                    (loop for src-row fixnum from first-row downto 0
                          for dst-row fixnum from 0 do
                          (xfer-line src-row dst-row))
                    (loop for src-row fixnum from (1- ht) above first-row
                          for dst-row fixnum from (1+ first-row) do
                          (xfer-line src-row dst-row)))
                  )))))
            
        (gp:image-access-transfer-to-image acc))
      
      (funcall continuation img)) ))

(defmacro with-array-converted-to-color-image-for-pane ((arr pane img args)
                                                        &body body)
  `(apply #'do-convert-array-to-color-image-for-pane
          ,arr ,pane
          (lambda (,img)
            ,@body)
          ,args))

#+:LISPWORKS
(editor:setup-indent "with-array-converted-to-color-image-for-pane" 1)


(defun do-tvscl (pane arr
                      &rest args
                      &key
                      (magn 1)
                      clear
                      &allow-other-keys)
  "Internal workhorse routine for TVSCL."
  (let ((pane (plotter-pane-of pane)))
    (with-delayed-update (pane)
      (let+ ((wd (array-dimension-of arr 1))
             (ht (array-dimension-of arr 0))
             (action (lambda (pane x y width height)
                       (declare (ignore x y width height))
                       (when clear
                         (apply (um:rcurry #'gp:draw-rectangle
                                           :filled t
                                           :foreground (capi:simple-pane-background pane)
                                           :compositing-mode :copy
                                           :shape-mode :plain)
                                pane (bounding-region pane)))
                       
                       (with-array-converted-to-color-image-for-pane (arr pane img args)
                         (let+ (( (lf bt rt tp) (gp:transform-points (plotter-xform pane)
                                                                     (list 0 0 wd ht))
                                  ))
                           (with-plotview-coords (pane)
                             (gp:draw-image pane img lf tp
                                            :to-width    (1+ (- rt lf))
                                            :to-height   (1+ (- bt tp))
                                            :from-width  wd
                                            :from-height ht))
                           )))))
        ;; this scaling gives the unflipped origin at the LLC
        ;; with positive Y values upward, positive X values rightward
        (pw-init-xv-yv pane (vector 0 wd) (vector 0 ht)
                       :xrange `(0 ,wd)
                       :yrange `(0 ,ht)
                       :magn   magn
                       :aspect 1)
        (augment-display-list pane action clear)
        ))))

;; user callable routine
(defun tvscl (pane arr &rest args)
  "Display an image array of z-values in the specified pane.
   This is distinct from plotting an image with user specified
   X and Y values. TVSCL always uses implicit pixel coordinates
   for its X and Y values."
  (apply 'do-tvscl pane arr (append args *default-args*)))

#|
(let* ((xs   (map 'vector #'sinc (vops:voffset -50/5 (vops:vscale 1/5 (vm:framp 100)))))
       (img  (vm:outer-prod xs xs)))
  (tvscl 'plt img :clear t) :magn 2 :clear t)
 |#

(defun pw-plot-image (pane xv yv arr &rest args)
  (with-array-converted-to-color-image-for-pane (arr pane img args)
    (let* ((wd     (array-dimension-of arr 1))
           (ht     (array-dimension-of arr 0))
           #|
             (box    (let ((box (plotter-box pane)))
                       (adjust-box
                        (list (1+ (box-left box))
                              (box-top box)
                              (1- (box-width box))
                              (box-height box)))
                       ))
             |#
           (xlog   (plotter-xlog pane))
           (xlogfn (logfn xlog))
           (ylog   (plotter-ylog pane))
           (ylogfn (logfn ylog)))
      (declare (fixnum wd ht))
        
      (flet ((x-value (x)
               (funcall xlogfn x))
             (y-value (y)
               (funcall ylogfn y)))

        (let+ ((xform  (plotter-xform pane))
               ( (lf bt rt tp)
                 (gp:transform-points xform
                                      (list (x-value (elt xv 0)) (y-value (elt yv 0))
                                            (x-value (elt xv 1)) (y-value (elt yv 1)))
                                      ))
                (plt-wd (1+ (- rt lf)))
                (plt-ht (1+ (- bt tp))))
          
          ;; (print (list plt-wd plt-ht))
          (with-plotview-coords (pane)
            (gp:with-graphics-state (pane
                                     :mask (plotter-mask pane))
              (gp:draw-image pane img
                             lf tp
                             :from-width  wd
                             :from-height ht
                             :to-width    plt-wd
                             :to-height   plt-ht)
              )))))
    ))

(defun do-plot-image (pane xv yv arr
                           &rest args
                           &key
                           clear
                           &allow-other-keys)
  "Internal workhorse for image plotting."
  (let ((pane (plotter-pane-of pane)))
    (with-delayed-update (pane)
      (let+ ((fresh   (or clear
                          (display-list-empty-p pane)))
             (action  (if fresh
                          (lambda (pane x y width height)
                            (declare (ignore x y width height))
                            (apply 'pw-axes pane args)
                            (apply 'pw-plot-image pane xv yv arr args))
                        (lambda (pane x y width height)
                          (declare (ignore x y width height))
                          (apply 'pw-plot-image pane xv yv arr args))
                        )))
        (apply 'pw-init-xv-yv pane xv yv args)
        (augment-display-list pane action fresh)
        ))))

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
                       :initial-element 0.0f0)))
  (loop for iy from 0 to ny do
        (loop for ix from 0 to nx do
              (setf (aref arr iy ix) (float (* ix iy) 1.0f0))))
  ;; (loop for ix from 0 to 50 do (setf (aref arr ix (round ix 2)) 0.0f0))
  (let ((plt (wset 'plt :background :black :foreground :white)))
    (plot-image plt '(0 100) '(0 100) arr :clear t :watermarkfn nil :aspect 1)))

(let* ((xs   (map 'vector #'sinc (vops:voffset -50/5 (vops:vscale 1/5 (vm:framp 100)))))
       (img  (vm:outer-prod xs xs)))
  ;; (inspect img)
  (plot-image 'plt '(-20 20) '(-20 20) img :clear t :aspect 1 :magn 1))
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
                             &rest args
                             &key
                             (from-x 0)
                             (from-y 0)
                             from-width
                             from-height
                             clear
                             (global-alpha 1.0)
                             &allow-other-keys)
  "Internal workhorse for image rendering."
  (let+ ((pane     (plotter-pane-of pane))
         (action (lambda (pane x y wd ht)
                   (declare (ignore x y wd ht))
                   (with-image (pane (img (gp:convert-external-image pane ext-img)))
                     #|
                     (ac:send ac:fmt-println "wd = ~A, ht = ~A"
                              (gp:image-width img)
                              (gp:image-height img))
                     |#
                     (let* ((iwd         (gp:image-width  img))
                            (iht         (gp:image-height img))
                            (from-x      (or (and from-x
                                                  (min from-x iwd))
                                             0))
                            (from-y      (or (and from-y
                                                  (min from-y iht))
                                             0))
                            (sub-wd      (- iwd from-x))
                            (sub-ht      (- iht from-y))
                            (from-width  (or (and from-width
                                                  (min from-width sub-wd))
                                             sub-wd))
                            (from-height (or (and from-height
                                                  (min from-height sub-ht))
                                             sub-ht)))
                       (apply 'pw-init-xv-yv pane
                              (vector from-x (+ from-x from-width))
                              (vector from-y (+ from-y from-height))
                              :aspect 1
                              (append args *default-args*))
                       (apply 'pw-axes pane args)
                       #|
                       (ac:send ac:fmt-println "from-x = ~A, from-y = ~A, from-wd = ~A, from-ht = ~A"
                                from-x from-y from-width from-height)
                       |#
                       (let+ (( (lf tp rt bt) (gp:transform-points (plotter-xform pane)
                                                                   ;; bottom-left, top-right
                                                                   (list from-x                 from-y
                                                                         (+ from-x from-width)  (+ from-y from-height)))
                                ))
                         #|
                         (ac:send ac:fmt-println "l = ~A, t = ~A, r = ~A, b = ~A" lf tp rt bt)
                         |#
                         (with-plotview-coords (pane)
                           #|
                           (when clear
                             (apply (um:rcurry #'gp:draw-rectangle
                                               :filled t
                                               :foreground :white
                                               :compositing-mode :copy
                                               :shape-mode :plain)
                                    pane (bounding-region pane)))
                           |#
                           (gp:with-graphics-state (pane
                                                    :mask  (plotter-mask pane))
                             #|
                             (pdbg "i-wd: ~A, i-ht: ~A, p-wd: ~A, p-ht: ~A"
                                   from-width from-height
                                   to-width to-height)
                             |#
                             (gp:draw-image pane img lf tp
                                            :transform    nil ;; (or transform (plotter-xform pane))
                                            :from-x       from-x
                                            :from-y       from-y
                                            :to-width     (- rt lf)
                                            :to-height    (- bt tp)
                                            :from-width   from-width
                                            :from-height  from-height
                                            :global-alpha global-alpha)
                             ))
                         ))
                     ))
                 ))
    (augment-display-list pane action clear)
    ))

#|
(render-image 'plt (read-image) :clear t :global-alpha 1.0)
|#

;; user callable routine
(defun render-image (pane ext-img &rest args)
  "Render an external image in the specified window pane."
  (when ext-img
    (apply #'do-render-image pane ext-img
           :logo nil
           :cright1 nil
           :cright2 nil
           (append args *default-args*))))


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


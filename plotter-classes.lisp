
(in-package :plotter)

(defvar *cross-cursor*
  (progn ;; ignore-errors
    (capi:load-cursor
     `((:win32 ,(namestring (translate-logical-pathname "PROJECTS:LIB;cross-i.cur")))
       #+nil
       (:cocoa ,(namestring (translate-logical-pathname
                             ;; "PROJECTS:LIB;crosshair2.gif"
                             "PROJECTS:LIB;cursor3.gif"
                             ))
        :x-hot 7
        :y-hot 7)
       #-nil
       (:cocoa ,(namestring (translate-logical-pathname
                             "PROJECTS:LIB;cursor4.gif"
                             ))
        :x-hot 15
        :y-hot 15)
       (:gtk   #P"~/Linux-stuff/crosshair.gif"
        :x-hot 7
        :y-hot 7)
       ))
    ))

(defclass line-style ()
  ((line-thick    :accessor line-thick
                  :initarg :thick)
   (line-dashing  :accessor line-dashing
                  :initarg  :dashing)
   (line-color    :accessor line-color
                  :initarg :color)
   (line-alpha    :accessor line-alpha
                  :initarg  :alpha)
   (line-type     :accessor line-type
                  :initarg  :type))
  (:default-initargs
   :thick   1
   :dashing nil
   :color   :darkgreen
   :alpha   nil
   :type    :interpolated))

(defclass symbol-style ()
  ((plot-symbol   :accessor plot-symbol
                  :initarg :symbol
                  :initform :circle)
   (fill-color    :accessor fill-color
                  :initarg :fill-color
                  :initform nil)
   (fill-alpha    :accessor fill-alpha
                  :initarg  :fill-alpha
                  :initform nil)
   (border-color  :accessor border-color
                  :initarg :border-color
                  :initform :black)
   (border-alpha  :accessor border-alpha
                  :initarg  :border-alpha
                  :initform nil)
   (border-thick  :accessor border-thick
                  :initarg :border-thick
                  :initform 1)
   (bar-width     :accessor bar-width
                  :initarg  :bar-width
                  :initform nil)
   (bar-offset    :accessor bar-offset
                  :initarg  :bar-offset
                  :initform nil))
  (:default-initargs
   :symbol       :circle
   :fill-color   nil
   :fill-alpha   nil
   :border-color :black
   :border-alpha nil
   :border-thick 1
   :bar-width    nil
   :bar-offset   nil))

(defclass plot-style ()
  ((line-style    :accessor line-style
                  :initarg :line-style)
   (symbol-style  :accessor symbol-style
                  :initarg :symbol-style)
   (legend        :accessor legend
                  :initarg :legend))
  (:default-initargs
   :line-style (make-instance 'line-style
                              :thick 1
                              :color :darkgreen)
   :symbol-style nil
   :legend       nil))

(defclass legend ()
  ((activep     :accessor activep     :initform t)
   (has-content :accessor has-content :initform nil)
   (highlighted :accessor highlighted :initform nil)
   (x           :accessor x           :initform 0)
   (y           :accessor y           :initform 0)
   (width       :accessor width       :initform 0)
   (height      :accessor height      :initform 0)
   (dragging    :accessor dragging    :initform nil)
   (dx          :accessor dx          :initform 0)
   (dy          :accessor dy          :initform 0)
   ))

(defclass plotter-pane (capi:output-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ;; The mixin has all the information needed to produce plots
  ;; but has nothing to draw on...
  ((xlog          :accessor plotter-xlog           :initform nil)
   (xmin          :accessor plotter-xmin           :initform 0.0d0)
   (xmax          :accessor plotter-xmax           :initform 1.0d0)

   (ylog          :accessor plotter-ylog           :initform nil)
   (ymin          :accessor plotter-ymin           :initform 0.0d0)
   (ymax          :accessor plotter-ymax           :initform 1.0d0)
   
   (box           :accessor plotter-box            :initform nil  :initarg :box)
   (xform         :accessor plotter-xform          :initform (gp:make-transform))
   (inv-xform     :accessor plotter-inv-xform      :initform (gp:make-transform))
   (mask          :accessor plotter-mask           :initform '(0 0 0 0))
   (dlist         :accessor plotter-display-list   :initform (make-array 16 :adjustable t :fill-pointer 0))
   (delayed       :accessor plotter-delayed-update :initform 0)
   (damage        :accessor plotter-delayed-damage :initform nil)
   (aspect        :accessor plotter-aspect         :initform nil)
   
   ;; info for nice looking zooming
   (def-wd        :accessor plotter-nominal-width  :initarg :nominal-width)
   (def-ht        :accessor plotter-nominal-height :initarg :nominal-height)

   (sf            :accessor plotter-sf    :initform 1)
   (magn          :accessor plotter-magn  :initform 1)

   (legend-info   :accessor plotter-legend-info        :initform (make-array 16 :adjustable t :fill-pointer 0))
   (legend-x      :accessor plotter-legend-x           :initform '(:frac 0.05))
   (legend-y      :accessor plotter-legend-y           :initform '(:frac 0.95))
   (legend-anchor :accessor plotter-legend-anchor      :initform :auto)
   (legend        :accessor plotter-legend             :initform (make-instance 'legend))
   (preferred-x   :accessor preferred-x                :initform nil)
   (preferred-y   :accessor preferred-y                :initform nil)
  
   (initial-gs    :accessor plotter-initial-gs         :initform nil)
   (plotting-gs   :accessor plotter-plotting-gs        :initform nil)
   (prev-frame    :accessor plotter-prev-frame         :initform nil)
   (plotter-valid :accessor plotter-valid              :initform t) ;; nil after destroy
   (cached-cmap   :accessor cached-cmap                :initform nil)
   (copy-oper     :accessor plotter-copy-oper          :initform nil)
   (move-augment  :accessor plotter-move-augment       :initform nil  :initarg :move-augmentation)
   (click-augment :accessor plotter-click-augment      :initform nil  :initarg :click-augmentation)
   (plot-cursor   :accessor plotter-cursor             :initform (or *cross-cursor* :crosshair))
   )
  (:default-initargs
   :nominal-width      400
   :nominal-height     300
   :display-callback   'display-callback
   :resize-callback    'resize-callback
   :destroy-callback   'destroy-callback
   :default-width      400
   :default-height     300
   :background :white
   :foreground :black
   :visible-min-width  200
   :visible-min-height 150
   :visible-max-width  800
   :visible-max-height 600
   :draw-with-buffer   t  ;; MSWindows performance is miserable without this...
   :pane-menu        'popup-menu
   :input-model      '((:motion mouse-move)
                       ((:button-1 :motion)  drag-legend)
                       ((:button-1 :press)   mouse-button-press)
                       ((:button-1 :release) undrag-legend)
                       ((:button-1 :press :shift) mouse-shift-button-press)
                       ((:gesture-spec "Backspace")
                        maybe-remove-legend)
                       ((:gesture-spec "Delete")
                        maybe-remove-legend)
                       ((:gesture-spec "Control-c")
                        copy-image-to-clipboard)
                       ((:gesture-spec "Control-p")
                        print-plotter-pane)
                       ((:gesture-spec "Control-s")
                        save-image-from-menu))
   ))

(defclass articulated-plotter-pane (plotter-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ;; The pane adds something to draw on...
  ;; And it also adds some user gestures and any display related items
  ;; like cross hairs, cursors, backing images, etc.
  (;; (backing-pixmap  :accessor plotter-backing-pixmap :initform nil)
   (full-crosshair  :accessor plotter-full-crosshair :initform nil   :initarg :full-crosshair)
   (prev-x          :accessor plotter-prev-x         :initform nil)
   (prev-y          :accessor plotter-prev-y         :initform nil)
   (x-ro-hook       :accessor plotter-x-readout-hook :initform #'identity)
   (y-ro-hook       :accessor plotter-y-readout-hook :initform #'identity)
   
   (mark-x           :accessor mark-x                :initform nil)
   (mark-y           :accessor mark-y                :initform nil)
   (mark-x-raw       :accessor mark-x-raw            :initform nil)
   (mark-y-raw       :accessor mark-y-raw            :initform nil)
   )
  (:default-initargs
   :pane-menu        'popup-menu
   :input-model      '((:motion mouse-move)
                       ((:button-1 :motion)  drag-legend)
                       ((:button-1 :press)   mouse-button-press)
                       ((:button-1 :release) undrag-legend)
                       ((:button-1 :press :shift) mouse-shift-button-press)
                       ((:gesture-spec "Backspace")
                        maybe-remove-legend)
                       ((:gesture-spec "Delete")
                        maybe-remove-legend)
                       ((:gesture-spec "Control-c")
                        copy-image-to-clipboard)
                       ((:gesture-spec "Control-p")
                        print-plotter-pane)
                       ((:gesture-spec "Control-s")
                        save-image-from-menu)
                       ((:gesture-spec "C")
                        toggle-full-crosshair)
                       ((:gesture-spec "c")
                        toggle-full-crosshair)
                       ((:gesture-spec "x")
                        mark-x-at-cursor)
                       ((:gesture-spec "y")
                        mark-y-at-cursor)
                       ((:gesture-spec "m")
                        mark-x-y-at-cursor)
                       ((:gesture-spec "u")
                        unmark-x-y))
   :cursor   (or *cross-cursor*
                 :crosshair)
   ))

(defmethod initialize-instance :after ((pane plotter-pane) &key
                                       (xsize         400)
                                       (ysize         300)
                                       (left-margin   +left-inset+)
                                       (top-margin    +top-inset+)
                                       (right-margin  +right-inset+)
                                       (bottom-margin +bottom-inset+)
                                       &allow-other-keys)
  ;; make a backup copy for restore as needed
  (setf (plotter-cursor pane) (capi:simple-pane-cursor pane))
  (unless (plotter-box pane)
    (setf (plotter-box pane) (inset-box-sides
                              `(0 0 ,xsize ,ysize)
                              left-margin  top-margin
                              right-margin bottom-margin))
    ))


#+:WIN32
(defmethod initialize-instance :after ((pane articulated-plotter-pane)
                                       &key full-crosshair background &allow-other-keys)
  (when full-crosshair
    (setf (plotter-full-crosshair pane)
          (complementary-color pane full-crosshair background))))
                               
(defun destroy-callback (pane)
  (setf (plotter-valid pane) nil)
  ;; (discard-backing-pixmap pane)
  )

;; ---------------------------------------------------------

(defun make-copy-menu ()
  (make-instance 'capi:menu-component
                 :items `(,(make-instance 'capi:menu-item
                                          :data :copy-image
                                          :text "Copy to clipboard")
                          ,(make-instance 'capi:menu-item
                                          :data :print-image
                                          :text "Print image")
                          ,(make-instance 'capi:menu-item
                                          :data :save-image
                                          :text "Save image")
                          )))

(defun maybe-legend-menu (pane)
  (when (have-legends-p pane)
    `(,(make-instance 'capi:menu-component
                      :items `(,(if (activep (plotter-legend pane))
                                    (make-instance 'capi:menu-item
                                                   :data :remove-legend
                                                   :text "Remove Legend")
                                  (make-instance 'capi:menu-item
                                                 :data :restore-legend
                                                 :text "Restore Legend")))
                      ))))

(defun make-popup-callback (pane x y)
  (lambda (key intf)
    (declare (ignore intf))
    (case key
      (:toggle-cursor
       (toggle-full-crosshair pane))
      (:copy-image
       (copy-image-to-clipboard pane))
      (:print-image
       (print-plotter-pane pane))
      (:save-image
       (save-image-from-menu pane))
      (:remove-legend
       (let ((legend (plotter-legend pane)))
         (setf (activep legend) nil)
         (restore-legend-background pane legend)))
      (:restore-legend
       (let ((legend (plotter-legend pane)))
         (setf (activep legend) t)
         (restore-legend-background pane legend)
         (draw-existing-legend pane legend)))
      (:mark-x
       (mark-x-at-cursor pane x y))
      (:mark-y
       (mark-y-at-cursor pane x y))
      (:mark-x-y
       (mark-x-y-at-cursor pane x y))
      (:remove-mark
       (unmark-x-y pane))
      )))
  
(defmethod popup-menu ((pane plotter-pane) selection x y)
  (declare (ignore selection))
  (make-instance 'capi:menu
                 :items `(,(make-copy-menu)
                          ,@(maybe-legend-menu pane))                 
                 :callback (make-popup-callback pane x y)
                 ))

(defmethod popup-menu ((pane articulated-plotter-pane) selection x y)
  (declare (ignore selection))
  (make-instance 'capi:menu
                 :items `(,(make-instance 'capi:menu-component
                                          :items `(,(make-instance 'capi:menu-item
                                                                   :data :toggle-cursor
                                                                   :text "Toggle crosshair")))
                          ,(make-copy-menu)
                          ,@(maybe-legend-menu pane)

                          ,(make-instance 'capi:menu-component
                                          :items `(,(make-instance 'capi:menu-item
                                                                   :data :mark-x
                                                                   :text "Mark X")
                                                   ,(make-instance 'capi:menu-item
                                                                   :data :mark-y
                                                                   :text "Mark Y")
                                                   ,(make-instance 'capi:menu-item
                                                                   :data :mark-x-y
                                                                   :text "Mark X & Y")
                                                   ,@(if (or (mark-x pane)
                                                             (mark-y pane))
                                                         `(,(make-instance 'capi:menu-item
                                                                           :data :remove-mark
                                                                           :text "Remove marker"))))
                                          ))
                 :callback (make-popup-callback pane x y)
                 ))

(defun maybe-remove-legend (pane x y &rest args)
  (declare (ignore args))
  (let ((legend (plotter-legend pane)))
    (when (and (activep legend)
               (on-legend pane x y))
      (setf (activep legend) nil)
      (restore-legend-background pane legend))
    ))

;; ---------------------------------------------------------
(defgeneric plotter-pane-of (pane-rep &optional args)
  ;; rep might be a plotter-pane,
  ;; a subclass of capi:interface,
  ;; or a symbolic name of a window
  )

(defmethod plotter-pane-of ((pane plotter-pane) &optional args)
  ;; it is me...
  (declare (ignore args))
  pane)

;; ------------------------------------------
(defmethod display-pane-of (pane)
  pane)

(defmethod display-pane-of ((obj capi:pinboard-object))
  (display-pane-of (capi:element-parent obj)))

;; -------------------------------------------------------------------

(defun display-list-empty-p (pane)
  (zerop (fill-pointer (plotter-display-list pane))))

(defun append-display-list (pane item)
  (when item
    (vector-push-extend item (plotter-display-list pane))
    (update-pane pane)))

(defun discard-display-list (pane)
  (setf (fill-pointer (plotter-display-list pane)) 0))

(defun augment-display-list (pane action fresh)
  (when fresh
    (discard-display-list pane))
  (append-display-list pane action))

(defun perform-display-list-items (pane &rest args)
  (loop for fn across (plotter-display-list pane) do
          (apply fn pane args)))

;; -------------------------------------------------------------

(defun append-legend (pane item)
  (vector-push-extend item (plotter-legend-info pane)))

(defun all-legends (pane &key discard)
  (prog1
      (coerce (plotter-legend-info pane) 'list)
    (when discard
      (setf (fill-pointer (plotter-legend-info pane)) 0))
    ))

(defun discard-legends (pane)
  (setf (fill-pointer (plotter-legend-info pane)) 0))

(defun have-legends-p (pane)
  (plusp (length (plotter-legend-info pane))))

;; --------------------------------------------------------------------

(defun sfloat (v)
  (float v 1f0))

(defun dfloat (v)
  (float v 1d0))

(defun log10 (x)
  (if (not (and (realp x)
                (plusp x)))
      -300
    (log x 10.0d0)))

(defun pow10 (x)
  (expt 10.0d0 x))

;; ------------------------------------------

(defun logfn (islog)
  (cond ((consp islog) (first islog))
        (islog         #'log10)
        (t             #'identity)))

(defun alogfn (islog)
  (cond ((consp islog) (second islog))
        (islog         #'pow10)
        (t             #'identity)))

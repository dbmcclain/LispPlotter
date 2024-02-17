
(in-package :plotter)

(defvar *cross-cursor*
  (progn ;; ignore-errors
    (capi:load-cursor
     `((:win32 ,(namestring (translate-logical-pathname "PROJECTS:LIB;cross-i.cur")))
       (:cocoa ,(namestring (translate-logical-pathname "PROJECTS:LIB;crosshair.gif"))
        :x-hot 7
        :y-hot 7)
       (:gtk   #P"~/Linux-stuff/crosshair.gif"
        :x-hot 7
        :y-hot 7)
       ))
    ))

(defclass <line-style> ()
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

(defclass <symbol-style> ()
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

(defclass <plot-style> ()
  ((line-style    :accessor line-style
                  :initarg :line-style)
   (symbol-style  :accessor symbol-style
                  :initarg :symbol-style)
   (legend        :accessor legend
                  :initarg :legend))
  (:default-initargs
   :line-style (make-instance '<line-style>
                              :thick 1
                              :color :darkgreen)
   :symbol-style nil
   :legend       nil))

(defclass <legend> ()
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

(defclass <plotter-mixin> ()
  ;; stuff used by 2-D plot scaling and plotting
  ;; The mixin has all the information needed to produce plots
  ;; but has nothing to draw on...
  ((lock          :accessor plotter-lock           :initform (mp:make-lock))
   
   (xlog          :accessor plotter-xlog           :initform nil)
   (xmin          :accessor plotter-xmin           :initform 0.0d0)
   (xmax          :accessor plotter-xmax           :initform 1.0d0)

   (ylog          :accessor plotter-ylog           :initform nil)
   (ymin          :accessor plotter-ymin           :initform 0.0d0)
   (ymax          :accessor plotter-ymax           :initform 1.0d0)
   
   (box           :accessor plotter-box)
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
   ;; (def-x         :accessor plotter-nominal-x      :initarg :nominal-x   :initform 0)
   ;; (def-y         :accessor plotter-nominal-y      :initarg :nominal-y   :initform 0)

   (sf            :accessor plotter-sf    :initform 1)
   (magn          :accessor plotter-magn  :initform 1)

   (legend-info   :accessor plotter-legend-info        :initform (make-array 16 :adjustable t :fill-pointer 0))
   (legend-x      :accessor plotter-legend-x           :initform '(:frac 0.05))
   (legend-y      :accessor plotter-legend-y           :initform '(:frac 0.95))
   (legend-anchor :accessor plotter-legend-anchor      :initform :auto)
   (legend        :accessor plotter-legend             :initform (make-instance '<legend>))
   (preferred-x   :accessor preferred-x                :initform nil)
   (preferred-y   :accessor preferred-y                :initform nil)
  
   (notify-cust   :accessor plotter-notify-cust        :initform nil)
   (initial-gs    :accessor plotter-initial-gs         :initform nil)
   (plotting-gs   :accessor plotter-plotting-gs        :initform nil)
   )
  (:default-initargs
   :nominal-width      400
   :nominal-height     300
   ))

(defclass <plotter-pane> (<plotter-mixin> capi:output-pane)
  ;; stuff used by 2-D plot scaling and plotting
  ;; The pane adds something to draw on...
  ;; And it also adds some user gestures and any display related items
  ;; like cross hairs, cursors, backing images, etc.
  (;; (backing-pixmap  :accessor plotter-backing-pixmap :initform nil)
   (delay-backing   :accessor plotter-delay-backing  :initform nil)
   (full-crosshair  :accessor plotter-full-crosshair :initform nil   :initarg :full-crosshair)
   (prev-x          :accessor plotter-prev-x         :initform nil)
   (prev-y          :accessor plotter-prev-y         :initform nil)
   (x-ro-hook       :accessor plotter-x-readout-hook :initform #'identity)
   (y-ro-hook       :accessor plotter-y-readout-hook :initform #'identity)
   (plotter-valid   :accessor plotter-valid          :initform t)
   
   (mark-x           :accessor mark-x                :initform nil)
   (mark-y           :accessor mark-y                :initform nil)
   (mark-x-raw       :accessor mark-x-raw            :initform nil)
   (mark-y-raw       :accessor mark-y-raw            :initform nil)

   (cached-cmap      :accessor cached-cmap           :initform nil)
   (cache-pixmap-ver :accessor plotter-cache-ver     :initform nil)
   (cache-pixmap     :accessor plotter-cache-pixmap  :initform nil)
   (cache-state      :accessor plotter-cache-state   :initform nil)

   (prev-frame       :accessor plotter-prev-frame    :initform nil)
   )
  (:default-initargs
   :display-callback 'display-callback
   :resize-callback  'resize-callback
   :destroy-callback 'destroy-callback
   :pane-menu        'popup-menu
   :input-model      '((:motion mouse-move)
                       ((:button-1 :motion)  drag-legend)
                       ((:button-1 :press)   show-x-y-at-cursor)
                       ((:button-1 :release) undrag-legend)
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
   :visible-min-width  200
   :visible-min-height 150
   :visible-max-width  800
   :visible-max-height 600
   :default-width      400
   :default-height     300
   :background :white
   :foreground :black
   ))

#+:WIN32
(defmethod initialize-instance :after ((pane <plotter-pane>)
                                       &key full-crosshair background &allow-other-keys)
  (when full-crosshair
    (setf (plotter-full-crosshair pane)
          (complementary-color pane full-crosshair background))))
                               
(defun destroy-callback (pane)
  (setf (plotter-valid pane) nil)
  ;; (discard-backing-pixmap pane)
  )

;; ---------------------------------------------------------
(defun popup-menu (pane selection x y)
  (declare (ignore selection))
  (make-instance 'capi:menu
                 :items `(,(make-instance 'capi:menu-component
                                          :items `(,(make-instance 'capi:menu-item
                                                                   :data :toggle-cursor
                                                                   :text "Toggle crosshair")))
                          ,(make-instance 'capi:menu-component
                                          :items `(,(make-instance 'capi:menu-item
                                                                   :data :copy-image
                                                                   :text "Copy to clipboard")
                                                   ,(make-instance 'capi:menu-item
                                                                   :data :print-image
                                          :text "Print image")
                                                   ,(make-instance 'capi:menu-item
                                                                   :data :save-image
                                                                   :text "Save image")))
                          ,@(if (or (on-legend pane x y)
                                    (not (activep (plotter-legend pane))))
                                `(,(make-instance 'capi:menu-component
                                                  :items `(,@(if (on-legend pane x y)
                                                                 `(,(make-instance 'capi:menu-item
                                                                                   :data :remove-legend
                                                                                   :text "Remove Legend")))
                                                           ,@(if (not (activep (plotter-legend pane)))
                                                                 `(,(make-instance 'capi:menu-item
                                                                                   :data :restore-legend
                                                                                   :text "Restore Legend"))))
                                                  )))
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
                                                                           :text "Remove marker"))))))
                 :callback (lambda (key intf)
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
                               ))
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
(defgeneric plotter-mixin-of (pane-rep &optional args)
  ;; rep might be a <plotter-pane>,
  ;; a subclass of capi:interface,
  ;; or a symbolic name of a window
  )

(defmethod plotter-mixin-of ((pane <plotter-mixin>) &optional args)
  ;; it is me...
  (declare (ignore args))
  pane)

;; ------------------------------------------
(defmethod display-pane-of (pane)
  pane)

(defmethod display-pane-of ((obj capi:pinboard-object))
  (display-pane-of (capi:element-parent obj)))

;; -------------------------------------------------------------------
(defun append-display-list (pane item)
  (vector-push-extend item (plotter-display-list pane))
  (update-pane pane))

(defun discard-display-list (pane)
  #|
  (setf (plotter-delayed-damage pane) nil
        (plotter-delayed-update pane) 0)
  |#
  (setf (fill-pointer (plotter-display-list pane)) 0
        (fill-pointer (plotter-legend-info  pane)) 0))

(defun display-list-items (pane &key discard)
  (prog1
      (coerce (plotter-display-list pane) 'list)
    (when discard
      (setf (fill-pointer (plotter-display-list pane)) 0))
    ))

(defun display-list-empty-p (pane)
  (zerop (length (plotter-display-list pane))))


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

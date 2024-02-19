
(in-package :plotter)

(defun bounds-overlap-p (bounds1 bounds2)
  (labels ((overlaps-p (bounds1 bounds2)
             (destructuring-bind (left1 right1) bounds1
               (destructuring-bind (left2 right2) bounds2
                 (declare (ignore right2))
                 (<= left1 left2 right1))
               )))
    (or (overlaps-p bounds1 bounds2)
        (overlaps-p bounds2 bounds1))
    ))

(defun expand-bounds (bounds dx)
  (list (- (first bounds) dx)
        (+ (second bounds) dx)))

;; ------------------------------------------
(defun draw-string-x-y (pane string x y
                             &key 
                             (x-alignment :left) 
                             (y-alignment :baseline)
                             prev-bounds
                             font
                             (margin 2)
                             (transparent t)
                             (color (foreground-color pane))
                             alpha
                             clip
                             (background :white)
                             bg-alpha
                             &allow-other-keys)
  ;; Draw a string at some location, unless the bounds of the new string
  ;; overlap the previous bounds. This is used to avoid placing axis labels
  ;; too closely together along the grid.
  (multiple-value-bind (left top right bottom)
      (gp:get-string-extent pane string font)
    (let* ((wd  (- right left))
           (dx (ecase x-alignment
                 (:left     0)
                 (:right    (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (dy (ecase y-alignment
                 (:top      (- top))
                 (:bottom   0)
                 (:center   (- (floor (+ top bottom) 2)))
                 (:baseline 0)))
           (new-bounds (list (+ x left dx) (+ x right dx))))
      
      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))
          prev-bounds

        (with-color (pane (adjust-color pane color alpha))
          (with-mask (pane (and clip
                                (adjust-box (plotter-box pane))))
            (gp:draw-string pane string (+ x dx) (+ y dy)
                            :font font
                            :block (not transparent)
                            :background (adjust-color pane background bg-alpha))
            new-bounds
            )))
      )))

;; ------------------------------------------

#+(OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8)
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :left)
                                  (y-alignment :baseline)
                                  font
                                  prev-bounds
                                  (margin 2)
                                  (color (foreground-color pane))
                                  ;;(transparent t)
                                  )
  ;;
  ;; draw vertical string by appealing directly to Cocoa
  ;;
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)
    (declare (ignore bt tp))

    (let* ((wd (- rt lf))
           (dx (ecase x-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dx) (+ y rt dx))))

      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))

          prev-bounds

        (progn
          (gp:with-graphics-translation (pane x y)
            (gp:with-graphics-rotation (pane (/ pi -2))
              (gp:with-graphics-translation (pane (- x) (- y))
                (gp:draw-string pane string (+ x dx) (- y 2)
                                :font      font
                                :color     color
                                :alpha     1.0
                                :x-alignment x-alignment
                                :y-alignment y-alignment))))
          new-bounds)
        ))))

#+(AND :COCOA (NOT (OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8)))
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :left)
                                  (y-alignment :baseline)
                                  font
                                  prev-bounds
                                  (margin 2)
                                  (color (foreground-color pane))
                                  ;;(transparent t)
                                  )
  ;;
  ;; draw vertical string by appealing directly to Cocoa
  ;;
  (declare (ignore pane))
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)
    (declare (ignore bt tp))

    (let* ((wd (- rt lf -1))
           (dx (ecase x-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dx) (+ y rt dx)))
           (font-attrs (gp:font-description-attributes (gp:font-description font)))
           (font-size  (getf font-attrs :size))
           (font-name  (getf font-attrs :name)))

      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))

          prev-bounds

        (progn
          (add-label pane string x  y
                     :font      font-name
                     :font-size font-size
                     :color     color
                     :alpha     1.0
                     :x-alignment x-alignment
                     :y-alignment y-alignment
                     :angle     90.0)
          new-bounds)
        ))))

#+(AND :WIN32 (NOT (OR :LISPWORKS6.1 :LISPWORKS7 :LISPWORKS8)))
(defun draw-vert-string-x-y (pane string x y
                                  &key
                                  (x-alignment :left)
                                  (y-alignment :baseline)
                                  font
                                  prev-bounds
                                  (margin 2)
                                  (color (foreground-color pane))
                                  (transparent t))
  ;;
  ;; draw vertical string by rotating bitmap of horizontal string
  ;;
  (multiple-value-bind (lf tp rt bt)
      (gp:get-string-extent pane string font)

    (let* ((wd (- rt lf -1))
           (ht (- bt tp -1))
           (dy (ecase y-alignment
                 (:top      0)
                 (:bottom   (- ht))
                 (:baseline tp)
                 (:center   (floor tp 2))
                 ))
           (dx (ecase x-alignment
                 (:right    0)
                 (:left     (- wd))
                 (:center   (- (floor wd 2)))
                 ))
           (new-bounds (list (+ y lf dx) (+ y rt dx))))
      
      (if (and prev-bounds
               (bounds-overlap-p (expand-bounds prev-bounds margin) new-bounds))

          prev-bounds

        (let ((wd (round wd))
              (ht (round ht)))
          (with-pixmap-graphics-port (ph pane wd ht
                                         :background (background-color pane)
                                         :foreground (foreground-color pane)
                                         :clear t)
            (gp:draw-string ph string
                            0 (- tp)
                            :font       font
                            :foreground color
                            :block      (not transparent))
            
            (with-image (pane (v-image #-:WIN32 (gp:make-image pane ht wd)
                                       #+:WIN32 (gp:make-image pane ht wd
                                                               :alpha nil)
                                       ))
              (with-image (ph (h-image (gp:make-image-from-port ph)))
                (with-image-access (ha (gp:make-image-access ph h-image))
                  (with-image-access (va (gp:make-image-access pane v-image))
                    (gp:image-access-transfer-from-image ha)
                    (loop for ix from 0 below wd do
                          (loop for iy from 0 below ht do
                                (setf (gp:image-access-pixel va iy (- wd ix 1))
                                      (gp:image-access-pixel ha ix iy))
                                ))
                    (gp:image-access-transfer-to-image va)
                    )))
              (gp:draw-image pane v-image
                             (+ x dy)
                             (+ y dx))
              ))
          new-bounds))
      )))


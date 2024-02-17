
(in-package :plotter)

;; ------------------------------------------
;; I don't know just how costly it is to convert an external image to
;; an image object, but here we have a cached-image that performs the
;; conversion only once for frequently used images, such as background
;; logo images.

(defstruct cached-image
  ext int)

(defvar *cached-dummy-port* nil)

(defun free-cached-image (obj)
  (when (and (cached-image-p obj)
             (cached-image-int obj))
    (gp:free-image *cached-dummy-port*
                   (shiftf (cached-image-int obj) nil))
    ))

(defmethod cached-image ((img cached-image))
  img)

(defmethod cached-image ((img gp:external-image))
  (make-cached-image
   :ext img))

(defmethod do-with-converted-image ((img gp:image) port fn)
  (declare (ignore port))
  (funcall fn img))

(defmethod do-with-converted-image ((img gp:external-image) port fn)
  (let ((converted (gp:convert-external-image port img)))
    (unwind-protect
        (funcall fn converted)
      (gp:free-image port converted))
    ))

(defmethod do-with-converted-image ((img cached-image) port fn)
  (declare (ignore port))
  (let ((converted  (or (cached-image-int img)
                        (let ((my-port (or *cached-dummy-port*
                                           (let ((screen  (capi:convert-to-screen)))
                                             (setf *cached-dummy-port* (capi:create-dummy-graphics-port screen))
                                             ))
                                       ))
                          (hcl:add-special-free-action 'free-cached-image)
                          (hcl:flag-special-free-action img)
                          (setf (cached-image-int img)
                                (gp:convert-external-image my-port (cached-image-ext img))
                                ))
                        )))
    (funcall fn converted)))

(defmacro with-converted-image ((img img-expr port) &body body)
  `(do-with-converted-image ,img-expr ,port (lambda (,img)
                                              ,@body)))
                        
;; ------------------------------------------------
   
(defvar *ext-logo*
  (cached-image
    (gp:read-external-image
     (translate-logical-pathname
      #+:COCOA "PROJECTS:LIB;Logo75Img-Alpha25y.bmp"
      ;; #+:COCOA "PROJECTS:LIB;Logo75Img-Alpha25y.pdf"
      ;; #+:COCOA "PROJECTS:LIB;AcudoraLogo.pdf"
      ;; #+:COCOA "PROJECTS:LIB;Logo75Img-Alpha20y-BlackBG.pdf"
      ;; "~/Desktop/Watermarks/graph_watermark1.jpg"
      #+:WIN32 "PROJECTS:LIB;Logo75Img-Alpha20y-BlackBG.bmp"
      ;; "Logo75Img-Alpha25y.bmp"
      #+:LINUX "~/Linux-stuff/Logo75Img-Alpha25y.bmp"
      ))))

(defvar *ext-logo-alpha* 1)

(defvar *cright1* "Copyright (c) 2006-2024 by Refined Audiometrics Laboratory")
(defvar *cright2* "All rights reserved.")

(defun stamp-logo (pane port logo logo-alpha)
  ;; We are positioning inside the plotting region. Not the parent
  ;; frame. Position (0,0) refers to the top-left corner of the
  ;; plotting region. The axis labels are located outside of this
  ;; region.
  (when logo
    (let* ((box (plotter-box pane))
           (bwd (box-width  box))
           (bht (box-height box)))
      (with-converted-image (image logo port)
        (unless (or (zerop (gp:image-width image))
                    (zerop (gp:image-height image)))
          (let* ((iwd  (gp:image-width  image))
                 (iht  (gp:image-height image))
                 (sf   (min (/ bwd iwd)
                            (/ bht iht)))
                 (dwd  (* sf iwd))
                 (dht  (* sf iht))
                 (top  (* 0.5 (- bht dht)))
                 (left (* 0.5 (- bwd dwd))))
            (gp:with-graphics-mask (port
                                    (plotter-mask pane))
              (gp:draw-image port image left top
                             :from-width  iwd
                             :from-height iht
                             :to-width    dwd
                             :to-height   dht
                             ;; WATCH OUT! if we don't have a float for global alpha
                             ;; then the COCOA system bombs out really badly...
                             :global-alpha (float logo-alpha 1.0)))
            )))
      )))

(defun watermark (pane port logo logo-alpha cright1 cright2)
  (let* ((box     (plotter-box pane))
         (font2   (find-best-font pane
                                  :size $tiny-times-font-size))
         (color2  #.(color:make-gray 0.7)))    
    (stamp-logo pane port logo logo-alpha)
    (let* ((left   18)
           (bottom (- (box-height box) 30)))
      (draw-string-x-y pane port cright1
                       left (- bottom 11)
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2)
      (draw-string-x-y pane port cright2
                       left bottom
                       :x-alignment :left
                       :y-alignment :top
                       :font  font2
                       :color color2))
    ))


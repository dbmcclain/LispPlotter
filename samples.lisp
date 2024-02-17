
(in-package :plotter)

;; ---------------------------------------------

(plt:fplot 'plt '(-20 20) (lambda (x) (/ (sin x) x))
           :clear t
           :title "Sinc(x)"
           :thick 2
           ;; :symbol :circle
           ;; :plot-joined t
           )

;; ---------------------------------------------

(plt:fplot 'plt '(-5 5) #'exp
           :clear t
           :title "Exp(x)"
           :thick 2
           :symbol :circle
           :plot-joined t
           :ylog t
           :legend "Trial Run"
           )

;; ---------------------------------------------

(plt:plot 'plt (vm:unoise 10)
          :clear t
          :thick 2
          :line-type :stepped)

(plt:plot 'plt (vm:unoise 10)
          :clear t
          :thick 2
          :line-type :histo)

;; ---------------------------------------------

(plt:plot 'plt (vm:unoise 10)
          :clear t
          :thick 2
          :symbol :vbars
          :alpha  0.2)

;; ---------------------------------------------

(plt:histogram 'plt (vm:gnoise 10000)
               :clear t
               :thick 1)

;; ---------------------------------------------

(let* ((screen  (capi:convert-to-screen))
       (port    (capi:create-dummy-graphics-port screen))
       (ext-img (gp:read-external-image
                 (translate-logical-pathname
                  "PROJECTS:LIB;Logo75Img-Alpha25y.bmp"
                  ;; "PROJECTS:LIB;Logo75Img-Alpha25y.pdf"
                  ))))
  (with-image (port
               (image (gp:convert-external-image port ext-img)))
    (ac:send ac:fmt-println "Image size: ~A x ~A"
             (gp:image-width image)
             (gp:image-height image))
    (ac:send ac:fmt-println "Pane size: ~A x ~A"
             (gp:port-width port)
             (gp:port-height port))
    ))

;; ---------------------------------------------

(defvar *plt*
  (let ((plt (capi:contain
              (make-instance '<plotter-pane>
                             :nominal-x 40
                             :nominal-y 50))))
    (fplot plt '(-20 20) #'sinc
           :clear t
           :thick 2)
    plt))


;; --------------------------------------------

(progn
  (defparameter *plt* nil)
  (capi:define-interface thingy ()
    ()
    (:panes
     (graf <plotter-pane>
           :accessor graf)

     (inspect-button capi:button
                     :text "Inspect"
                     :callback (lambda (x intf)
                                 (inspect (graf intf)))
                     ))
    (:layouts
     (button-layout
      capi:row-layout
      '(nil inspect-button))
     (main-layout
      capi:column-layout
      '(graf button-layout)))
    
    (:default-initargs
     :layout 'main-layout
     :title  "Test Graf"
     :best-width  408
     :best-height 351))
  
  (defun doit ()
    (setf *plt*
          (graf (capi:display (make-instance 'thingy)
                              )))
    (fplot *plt* '(-20 20) #'sinc :clear t :thick 2))

  (doit))
  
(doit)
(bounding-region *plt*)
(fplot *plt* '(-20 20) #'sinc :clear t :thick 2)
(clear *plt*)

;; -----------------------------------------------------

(progn
  (defparameter *intf*  nil)
  (defparameter *plt-l* nil)
  (defparameter *plt-r* nil)
  (capi:define-interface dual-thingy ()
    ()
    (:panes
     (graf-l <plotter-pane>
             :accessor graf-l)
     (graf-r <plotter-pane>
             :accessor graf-r)
     
     (inspect-button-l capi:button
                     :text "Inspect-L"
                     :callback (lambda (x intf)
                                 (inspect (graf-l intf))))
     (inspect-button-r capi:button
                     :text "Inspect-R"
                     :callback (lambda (x intf)
                                 (inspect (graf-r intf))))
     )
    (:layouts
     (left-layout
      capi:column-layout
      '(graf-l inspect-button-l))
     (right-layout
      capi:column-layout
      '(graf-r inspect-button-r))
     (main-layout
      capi:row-layout
      '(left-layout right-layout)))
    
    (:default-initargs
     :layout 'main-layout
     :title  "Test GrafPair"
     :best-width  811
     :best-height 351))
  
  (defun doit ()
    (setf *intf*
          (capi:display (make-instance 'dual-thingy)
                        )
          *plt-l* (graf-l *intf*)
          *plt-r* (graf-r *intf*))
    (fplot *plt-l* '(-20 20) #'sinc :clear t :thick 2)
    (fplot *plt-r* '(-10 10) #'sinc :clear t :thick 2))

  (doit))

(fplot *plt-r* '(-10 10) #'sinc :clear t :thick 2)
(inspect *plt-r*)
(bounding-region *plt-l*)
(bounding-region *plt-r*)
(plotting-region *plt-l*)
(plotting-region *plt-r*)
(plotter-mask *plt-l*)
(plotter-mask *plt-r*)

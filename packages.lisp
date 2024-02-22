
(defpackage #:plotter
  (:nicknames #:plt)
  #+nil
  (:local-nicknames
   (#:ca       #:com.ral.c-arrays)
   (#:c-arrays #:com.ral.c-arrays)
   ;; (#:um       #:com.ral.useful-macros)
   (#:engfmt   #:com.ral.useful-macros.engfmt)
   (#:vmath    #:com.ral.vectorized-math)
   ;; (#:vm       #:com.ral.vectorized-math)
   (#:interpolation #:com.ral.interpolation))
  (:use   #:common-lisp #:vops #:def*) ;; #:com.ral.vector-ops)
  (:export
   #:sinc
   #:find-named-plotter-pane
   #:*plotter-window-class*
   #:<plotter-window>
   #:<plotter-pane>
   #:<plotter-mixin>
   #:window ;; these functions take a symbolic window id argument
   #:wset
   #:wshow
   #:wclose
   
   #:clear  ;; these functions require a <plotter-pane> argument
   #:plot
   #:axes
   #:with-delayed-update
   #:histogram
   #:spline
   #:fplot
   #:paramplot
   #:plot-bars
   #:read-image
   #:render-image
   #:plot-image
   #:save-image
   #:save-plot  ;; a synonym for save-image
   #:tvscl
   #:set-x-readout-hook
   #:set-y-readout-hook
   #:display-cursor-readout

   #:draw-text
   #:draw-line
   #:draw-rect
   #:draw-circle
   #:draw-ellipse
   #:draw-arc

   #:set-full-crosshair

   #:$tiny-times-font-size
   #:$normal-times-font-size
   #:$big-times-font-size

   #:$heat-colormap
   #:$gray-colormap
   #:get-cmap
   #:set-cmap
   
   #:with-default-args
   #:wait-until-finished
   
   #:helpme

   #:draw-text-box

   #:cmplx-plot
   #:cmplx-paramplot
   #:polar-fplot

   #:help
   ))

#|
;; generate HTML documentation
(asdf :doctools)
(doctools:gen-docs
 :asdf-system-name :plotter
 :package-name     :plotter
 :directory        (translate-logical-pathname "PROJECTS:LISP;Plotter;")
 :subtitle         "a library for scientific graphing")
|#

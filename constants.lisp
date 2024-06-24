
(in-package :plotter)

;; Plotting region inset to accommotate axis labels
(defconstant +LEFT-INSET+   32)
(defconstant +TOP-INSET+    20)
(defconstant +RIGHT-INSET+  15)
(defconstant +BOTTOM-INSET+ 35)

(defconstant $largest-permissible-value
  (/ least-positive-normalized-double-float))

(defconstant $tiny-times-font-size
  #-:WIN32 10
  #+:WIN32  8)

(defconstant $normal-times-font-size
  #-:WIN32 12
  #+:WIN32  9)

(defconstant $big-times-font-size
  #-:WIN32 14
  #+:WIN32 10)
  

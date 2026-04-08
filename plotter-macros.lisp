
(in-package :plotter)

;; ------------------------------------------
;; Convenience macros

(defmacro with-color ((port color) &body body)
  `(gp:with-graphics-state
       (,port
        :foreground ,color)
     ,@body))
  
(defmacro with-mask ((port mask) &body body)
  `(gp:with-graphics-state
       (,port
        :mask ,mask)
     ,@body))


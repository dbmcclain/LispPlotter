
(in-package :plotter)

;; ------------------------------------------
;; gp rectangles are list ltrb
(defun inset-box-sides (box dxleft dytop 
                            &optional (dxright dxleft)
                                      (dybottom dytop))
  (let ((lf (+ (gp:rectangle-left box) dxleft))
        (tp (+ (gp:rectangle-top  box) dytop)))
    (list lf tp
          (- (gp:rectangle-right  box) dxright)
          (- (gp:rectangle-bottom box) dybottom))))

(defmacro box-left (box)
  `(gp:rectangle-left ,box))

(defmacro box-top (box)
  `(gp:rectangle-top ,box))

(defmacro box-right (box)
  `(gp:rectangle-right ,box))

(defmacro box-bottom (box)
  `(gp:rectangle-bottom ,box))

(defmacro box-width (box)
  `(gp:rectangle-width ,box))

(defmacro box-height (box)
  `(gp:rectangle-height ,box))

(defmacro box-top-left (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-left ,gbx) (box-top ,gbx)))))

(defmacro box-top-right (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-right ,gbx) (box-top ,gbx)))))

(defmacro box-bottom-left (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-left ,gbx) (box-bottom ,gbx)))))

(defmacro box-bottom-right (box)
  (let ((gbx (gensym)))
    `(let ((,gbx ,box))
       (list (box-right ,gbx) (box-bottom ,gbx)))))

;; ------------------------------------------------
;; Functional operators - peel off section of box, returning it, and
;; return the reduced original box/
;;
;; Rectangles are a list of numbers (lf tp rt bt)

(defun remove-from-top (r n)
  (let+ (( (lf tp rt bt) r)
         (nn  (max 0 (min n (- bt tp)))))
    (values `(,lf ,tp        ,rt ,(+ tp nn))
            `(,lf ,(+ tp nn) ,rt ,bt))
    ))

(defun remove-from-bottom (r n)
  (let+ (( (lf tp rt bt) r)
         (nn (max 0 (min n (- bt tp)))))
    (values `(,lf ,(- bt nn) ,rt ,bt)
            `(,lf ,tp        ,rt ,(- bt nn)))
    ))

(defun remove-from-left (r n)
  (let+ (( (lf tp rt bt) r)
         (nn (max 0 (min n (- rt lf)))))
    (values `(,lf        ,tp ,(+ lf nn) ,bt)
            `(,(+ lf nn) ,tp ,rt        ,bt))
    ))

(defun remove-from-right (r n)
  (let+ (( (lf tp rt bt) r)
         (nn (max 0 (min n (- rt lf)))))
    (values `(,(- rt nn) ,tp ,rt        ,bt)
            `(,lf        ,tp ,(- rt nn) ,bt))
    ))


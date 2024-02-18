;; graphics-state.lisp -- The vagaries of dealing with a mutable graphics state of mutable objects...
;;
;; DM/RAL  02/24
;; --------------------------------------

(in-package :plotter)

;; ================================================================
;; Graphics State - more than meets the eye...
;; ================================================================
;; Saving and Switching the Graphic State
;;
;; ------------------------------------------------------------------
;; Uh oh... this passes:
;;
;;    (assert (eq (gp:port-graphics-state port)
;;                (slot-value port 'gp:graphics-state)))
;;
;; So we better be making struture copies of the graphics state.
;; Else save/restore does nothing useful.
;;
;; It seems sad that we need to know that graphics-state is a STRUCT.
;; This is not future-proof design.
;;
(defun do-with-saved-graphics-state (pane fn)
  (let ((sav  (get-graphics-state pane)))
    (unwind-protect
        (funcall fn)
      (set-graphics-state pane sav))
    ))
        
(defmacro with-saved-graphics-state (pane &body body)
  `(do-with-saved-graphics-state ,pane (lambda ()
                                         ,@body)))

;; -------------------------------------------------------
;; It gets worse... not only is the graphics state a mutable struct,
;; but the transform inside is also a mutable thing.
;;
;; *AND* You must carry along the current GS Mask for clipping, and
;; not restore what it was when you saved the GS.

(defstruct saved-gs
  state xform)

(defun get-graphics-state (pane)
  (let* ((gs    (copy-structure (gp:port-graphics-state pane)))
         (xform (gp:copy-transform (gp:graphics-state-transform gs))))
    (make-saved-gs
     :state gs
     :xform xform)
    ))

(defun set-graphics-state (pane saved)
  (let ((new-gs    (copy-structure (saved-gs-state saved)))
        (new-xform (gp:copy-transform (saved-gs-xform saved)))
        (mask      (gp:graphics-state-mask (gp:port-graphics-state pane))))
    (setf (gp:graphics-state-transform new-gs) new-xform ;; restore the transform
          (gp:graphics-state-mask      new-gs) mask      ;; carry along the current clipping mask
          (gp:port-graphics-state      pane)   new-gs)   ;; restore the state
    ))

;; -----------------------

(defun do-with-new-graphics-state (pane new-gs fn)
  (with-saved-graphics-state pane
    (set-graphics-state pane new-gs)
    (funcall fn)))

(defmacro with-new-graphics-state ((pane new-state) &body body)
  `(do-with-new-graphics-state ,pane ,new-state (lambda ()
                                                  ,@body)))

; -------------------------------------------------------------

(defun do-with-plotview-coords (pane fn)
  ;;
  ;; Inside here we should work, with unscaled screen-level coords
  ;; for, the plotview - a viewport that assumes origin at top-left
  ;; corner, with positive measure in x pixels to the right, and y
  ;; pixels toward the bottom.
  ;;
  ;; The transform wrapped around the body will translate into
  ;; absolute scaled coords for the parent port.
  ;;
  ;; NOTE: LW states that they use transform pre-mult, which I take to
  ;; mean: mult from left (but their language is ambiguous). And they
  ;; demonstrate that transform is used by multiplying a row vector on
  ;; the left of the transform matrix.
  ;;
  ;; But after experimenting above, the net effect is to have the
  ;; transforms apply in order from innermost to outermost nesting
  ;; order, i.e., the first one (innermost) applied happens first to
  ;; coords.
  ;;
  ;; !! THIS IS OPPOSITE OF THE EFFECT OF SUCCESSIVE TRANFORMATIONS
  ;; WHEN CONSTRUCTING AN OVERT COMPOSITE TRANSFORM MATRIX.
  ;;
  ;;  (0,0) --> plus x
  ;;  | +------------------------------------------+
  ;;  | | Parent frame                             |
  ;;  v |  +----------------------------------+    |
  ;;    |  | Plotting region                  |    |
  ;;  p |  |                                  |    |
  ;;  l |  |                                  |    |
  ;;  u |  |                                  |    |
  ;;  s |  |                                  |    |
  ;;    |  |                                  |    |
  ;;  y |  |                                  |    |
  ;;    |  |                                  |    |
  ;;    |  +----------------------------------+    |
  ;;    |                                          |
  ;;    +------------------------------------------+
  ;;
  ;; This code transforms from pixel space inside the plotting region
  ;; to pixel space coords inside the parent frame.
  ;;
  ;; NB: Regarding PLOTTER-BOX. It is mutated along the way to
  ;; indicate the size of the plotting region in unscaled pixel
  ;; coords.
  ;;
  ;; Only the right and bottom are modified according to aspect ratio.
  ;; The top-left is adjusted to always be the location of
  ;; the plotting region within the parent graphport, before any
  ;; scaling has been applied. The true location of the parent
  ;; graphport is indicated by (LEFT - +LEFT-INSET+) and (TOP -
  ;; +TOP-INSET+) prior to any scaling.
  ;;
  ;; Masks, for clipping regions, appear to be unaffected by graphic
  ;; transforms, and so must be explicitly computed in parent frame
  ;; coordinates.
  ;;
  (with-accessors ((plot-state plotter-plotting-gs)) pane
    (with-new-graphics-state (pane plot-state)
      (funcall fn))
    ))

(defmacro with-plotview-coords ((pane) &body body)
  `(do-with-plotview-coords ,pane (lambda ()
                                    ,@body)))

;; ---------------------------------------------------
;; We don't have a PORT until we get called during REDRAW callback
;; time. So we can't precompute this information ahead of time, to
;; save cycles in the main CAPI thread. But we can compute just once
;; during the REDRAW and save on repeated needs.
;;
(defun compute-graphics-state (pane)
  ;; Precompute and cache state for the necessary plotting transforms
  ;; for use inside the PlotView region.
  (with-accessors ((sf    plotter-sf)) pane
    (with-saved-graphics-state pane
      (gp:with-graphics-state (pane
                               ;; :transform (gp:make-transform)
                               :scale-thickness t)
        (gp:with-graphics-scale (pane sf sf)
          (gp:with-graphics-translation (pane +LEFT-INSET+ +TOP-INSET+)
            (get-graphics-state pane)
            )))
      )))

;; ------------------------------------------------------------
;; To be used at the top of the REDRAW callback

(defun do-with-drawing-graphics-state (pane fn)
  (with-accessors ((init-state  plotter-initial-gs)
                   (plot-state  plotter-plotting-gs)) pane
    (unless init-state
      ;; what else can we do? just grab first seen instance as our
      ;; initial state
      (setf init-state (get-graphics-state pane)))
    (with-new-graphics-state (pane init-state)
      ;; we have to recompute because the port might have changed size
      ;; and scaling
      (setf plot-state (compute-graphics-state pane))
      (funcall fn))
    ))

(defun recompute-plotting-state (pane)
  (do-with-drawing-graphics-state pane #'lw:do-nothing))

(defmacro with-drawing-graphics-state ((pane) &body body)
  `(do-with-drawing-graphics-state ,pane (lambda ()
                                           ,@body)))

;; ---------------------------------------------------------
#| ... Unused...
(defun do-with-data-coords (pane port fn)
  ;; Inside here we should work with data coords (xmin,xmax) and
  ;; (ymin,ymax)
  ;;
  ;; This code converts from data coords to pixel space coords inside
  ;; the plotting region.
  (with-accessors ((xmin  plotter-xmin)
                   (xmax  plotter-xmax)
                   (ymin  plotter-ymin)
                   (ymax  plotter-ymax)
                   (box   plotter-box)) pane
    (let* ((dx      (- xmax xmin))
           (dy      (- ymin ymax))
           (xscale  (/ (box-width box) dx))
           (yscale  (/ (box-height box) dy)))
      (gp:with-graphics-scale (port xscale yscale)
        (gp:with-graphics-translation (port (- xmin) (- ymax))
          (funcall fn)
          )))))
  
(defmacro with-data-coords ((pane port) &body body)
  `(do-with-data-coords ,pane ,port (lambda ()
                                      ,@body)))
|#

#|
;; This fails the assertion... which means that each dummy port has
;; its own graphics state.
;;
;; That's good news for parallel code handing different portions of a
;; preview computation in prep for rapid playback on the main CAPI
;; thread.
;;
;; So even though the graphics state in a port cannot be safely shared
;; - a mutable struct of mutable elements - we can safely spin off
;; separate dummy ports for each computation thread, and they won't
;; step on each other.
;;
;; Still... anything that depends on transform scaling can't be
;; precomputed ahead of the REDRAW callback, since we only find out at
;; that time just what the transform scaling needs to be.
;;
;; And that comprises all of the computing, save for initial scan of
;; NaN's and Infinities, and finding out the data coord extrema
;; values. But that is already being performed ahead of the CAPI
;; thread.
;;
;; So, no... we really can't invoke parallel precomputation in any
;; meaningful way.
;;
;; ...unless... If we can sense whether or not the port frame size has
;; changed, or the frame has moved, then for unchanged plotting we
;; could precompute everything and use over and over again, except for
;; fresh data.
;;
(let* ((screen  (capi:convert-to-screen))
       (p1      (capi:create-dummy-graphics-port screen))
       (p2      (capi:create-dummy-graphics-port screen)))
  (assert (eq (gp:port-graphics-state p1)
              (gp:port-graphics-state p2))))
|#

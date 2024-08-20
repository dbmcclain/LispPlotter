;; plotter-mp.lisp -- Threading control, shared data with CAPI
;;
;; DM/RAL  02/24
;; ----------------------------------------------------------

(in-package :plotter)

;; ---------------------------------------------
;; CAPI needs mostly unfettered access to CAPI-elements, like
;; plotter-pane. We can't tolerate locks in the redraw callback.
;; Hence, the CAPI thread must be considered the owner of the element
;; data.
;;
;; While most of our interactions are synchronous, we can't be sure
;; that the user hasn't staged some delayed execution against our
;; pane, using APPLY-IN-PANE-PROCESS. So to keep things monotonically
;; ordered in time, we must always ask CAPI to tell us the value of
;; element data, and ask CAPI to perform any mutation of the data.

;; ---------------------------------------------
;; Define some safe image access macros...
;;
(defun do-with-image (port image fn)
  (unwind-protect
      (funcall fn image)
    (gp:free-image port image)))

(defmacro with-image ((port (image imgexpr)) &body body)
  ;; returned value will be that of the body
  `(do-with-image ,port ,imgexpr
                  (lambda (,image)
                    ,@body)))


(defun do-with-image-access (acc fn)
  (unwind-protect
      (funcall fn acc)
    (gp:free-image-access acc)))

(defmacro with-image-access ((acc access-expr) &body body)
  ;; returned value will be that of the body
  `(do-with-image-access ,access-expr
                         (lambda (,acc)
                           ,@body)))

;; ------------------------------------------
;; We can use WITH-DELAYED-UPDATE to ward off immediate and slowing
;; direct drawing operations. Delayed sections can be nested. Meanwhile,
;; within a delayed section we are simply building up a display list of
;; parameterized lambda closures that collectively will produce the sum
;; of all delayed operations, once the delay goes back to zero.
;;
(defun do-sync-with-capi (capi-fn capi-elt fn args)
  (apply capi-fn capi-elt fn args))

(defmethod sync-with-capi ((intf capi:interface) fn &rest args)
  (do-sync-with-capi #'capi:execute-with-interface intf fn args))

(defmethod sync-with-capi ((pane capi:simple-pane) fn &rest args)
  (do-sync-with-capi #'capi:apply-in-pane-process pane fn args))

(defmethod sync-with-capi ((pane capi:pinboard-object) fn &rest args)
  (let ((layout (capi:pinboard-object-pinboard pane)))
    (when layout
      (apply 'sync-with-capi layout fn args))
    ))

#|
;; ------------------------------------------
;; WITHOUT-CAPI-CONTENTION - perform an action synchronously, without
;; race conditions against CAPI, on data that is shared with the CAPI
;; thread.
;;
;; Not bullet-proof, but good enough, in a Lisp sense. If the pane is
;; in the process of being destroyed, then this might not work
;; correctly. But that shouldn't normally happen. It is not a good
;; idea to share control of Plotter panes among multiple threads.
;;
;; Assuming a thread is in control of its own Plotter panes, apart
;; from possible sharing with the CAPI thread, then this accomplishes
;; lock-free synchronous execution.
;;
(defun do-without-capi-contention (pane fn)
  (let+ ((:mvb (results status)
          (capi:apply-in-pane-process-wait-multiple pane nil fn) ))
    (if status
        (values-list results)
      (funcall fn))
    ))

(defmacro without-capi-contention (pane &body body)
  `(do-without-capi-contention ,pane (lambda ()
                                       ,@body)))
|#
;; ------------------------------------------

(defmethod capi:redisplay-element ((pane plotter-pane) &optional x y width height)
  (if (zerop (plotter-delayed-update pane))
      (call-next-method)
    (pushnew (list x y width height) (plotter-delayed-damage pane)
             :test #'equalp)))
  
(defun redraw-entire-pane (pane)
  (capi:redisplay-element pane))

(defun update-pane (pane &rest region) ;; x y wd ht
  (apply #'capi:redisplay-element (plotter-pane-of pane) region))

;; ---------------------------------------------------------------

(defun begin-update (pane)
  (incf (plotter-delayed-update pane)))

(defun end-update (pane)
  (when (zerop (decf (plotter-delayed-update pane)))
    (dolist (region (shiftf (plotter-delayed-damage pane) nil))
      (apply #'capi:redisplay-element pane region))))
  
(defun do-with-delayed-update (pane fn)
  (let ((the-pane (plotter-pane-of pane)))
    (begin-update the-pane)
    (unwind-protect
        (funcall fn)
      (end-update the-pane))
    ))

;; user callable macro
(defmacro with-delayed-update (pane &body body)
  `(do-with-delayed-update ,pane
    (lambda ()
      ,@body)))

;; ------------------------------------------------------------------

#|
(defun do-wait-until-finished (pane timeout fn)
  (let ((mbox     (mp:make-mailbox))
        (the-pane (plotter-pane-of pane)))
    (ac:β (ans)
        (progn
          (with-delayed-update (the-pane :notifying ac:β)
            (funcall fn))
          (mp:mailbox-read mbox "Waiting on Plotter" timeout))
      (mp:mailbox-send mbox ans))
    ))

(defmacro wait-until-finished ((pane &key timeout) &body body)
  `(do-wait-until-finished ,pane ,timeout (lambda ()
                                           ,@body)))
|#

#|
;; test delayed updates -- entire composite plot should appear at one time
(let ((win (plt:wset 'myplot)))
  (plt:clear win)
  (plt:with-delayed-update (win)
    ;; (plt:clear win)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x))
               :thick 2
               :title "Sinc"
               :clear t)
    (sleep 2)
    (plt:fplot win '(-20 20) (lambda (x) (/ (sin x) x))
               :symbol :circle
               :color :blue)))
|#




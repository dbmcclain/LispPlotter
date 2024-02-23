
(in-package :plotter)

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

;; ------------------------------------------

(defun do-without-capi-contention (pane fn in-capi-process-p)
  (cond (in-capi-process-p
         (funcall fn))
        (t
         (let+ ((:mvb (results status)
                 (capi:apply-in-pane-process-wait-multiple pane nil fn) ))
           (if status
               (values-list results)
             (funcall fn))
           ))
        ))

(defmacro without-capi-contention ((pane &key in-capi-process-p) &body body)
  `(do-without-capi-contention ,pane (lambda ()
                                       ,@body)
                               ,in-capi-process-p))

;; ------------------------------------------

(defmethod capi:redisplay-element :around ((pane <plotter-pane>) &optional x y width height)
  (if (zerop (plotter-delayed-update pane))
      (call-next-method)
    (pushnew (list x y width height) (plotter-delayed-damage pane)
              :test #'equalp)))

(defmethod redraw-entire-pane ((pane <plotter-pane>))
  (capi:redisplay-element pane))

(defun update-pane (pane &rest region) ;; x y wd ht
  (apply #'capi:redisplay-element (plotter-mixin-of pane) region))

;; ---------------------------------------------------------------

(defun do-with-delayed-update (pane notifying fn)
  (let ((the-pane (plotter-mixin-of pane)))
    (declare (dynamic-extent the-pane))
    (flet ((begin-update ()
             (without-capi-contention (pane)
               (incf (plotter-delayed-update the-pane))
               (when notifying
                 (pushnew notifying (plotter-notify-cust the-pane)))
               ))
           (end-update ()
             (without-capi-contention (pane)
               (when (zerop (decf (plotter-delayed-update the-pane)))
                 (dolist (region (shiftf (plotter-delayed-damage the-pane) nil))
                   (apply #'capi:redisplay-element the-pane region)))
               )))
      (declare (dynamic-extent #'begin-update #'end-update))
      (begin-update)
      (unwind-protect
          (funcall fn)
        (end-update))
      )))

;; user callable macro
(defmacro with-delayed-update ((pane &key notifying) &body body)
  `(do-with-delayed-update ,pane ,notifying
    (lambda ()
      ,@body)))

;; ------------------------------------------------------------------

(defun do-wait-until-finished (pane timeout fn)
  (cond ((zerop (plotter-delayed-update pane))
         (let ((mbox  (mp:make-mailbox)))
           (ac:β (ans)
               (progn
                 (with-delayed-update (pane :notifying ac:β)
                   (funcall fn))
                 (mp:mailbox-read mbox "Waiting on Plotter" timeout))
             (mp:mailbox-send mbox ans))
           ))
        (t
         (funcall fn))
        ))

(defmacro wait-until-finished ((pane &key timeout) &body body)
  `(do-wait-until-finished ,pane ,timeout (lambda ()
                                           ,@body)))

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




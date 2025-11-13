;; tst-capi=exec.lisp -- Probe the intelligence of the CAPI system
;;
;; DM/RAL  02/24
;; --------------------------------------------------------------

(in-package :plotter)

;; ------------------------------------------------------------

(defun tst (pane)
  (let* ((pane     (plotter-pane-of pane))
         (flag     :not-yet)
         (action   (lambda (pane x y w h)
                     (declare (ignore x y w h))
                     ;; This code gets executed in the redraw callback.
                     (capi:apply-in-pane-process pane
                                                 (lambda ()
                                                   (setf flag :done)))
                     (ac:send ac:println flag))))
    (augment-display-list pane action t)))

#|
 ;; The fact that this produces a display of DONE in the Output
 ;; Browser, shows that CAPI is intelligent enough to recognize that
 ;; when executing in the CAPI thread already, there is no need to
 ;; send a message to your process mailbox for APPLY-IN-PANE-PROCESS.
 ;;
 ;; Had it simply sent the message to itself, we would have seen
 ;; NOT-YET displayed, instead of DONE.
(tst 'plt)
 |#
;; ------------------------------------------------------

(defun tstx (pane)
  (let* ((pane     (plotter-pane-of pane))
         (flag     :not-yet)
         (action   (lambda (pane x y w h)
                     (declare (ignore x y w h))
                     ;; This code gets executed in the redraw callback.
                     (capi:apply-in-pane-process-wait-multiple pane nil
                                                 (lambda ()
                                                   (setf flag :done)))
                     (ac:send ac:println flag))))
    (augment-display-list pane action t)))

#|
;; The fact that this produces DONE in the Output Browser, shows that
;; the CAPI thread just directly executes the function. Had it staged
;; for later execution and then waited on its own mailbox, the system
;; should have hung.
(tstx 'plt)
|#
;; ------------------------------------------------------------

(defun tstu (pane)
  (let* ((pane     (plotter-pane-of pane))
         (flag     :not-yet))
    (capi:apply-in-pane-process pane
                                (lambda ()
                                  (setf flag :done)))
    flag))

|#
;; The fact that this produce :NOT-YET shows that
;; APPLY-IN-PANE-PROCESS merely schedules an action at some future
;; time, but does not synchronously execute the function.
(tstu 'plt)
|#

;; ------------------------------------------------------------

(defun tstw ()
  (plt:fplot 'plt '(-10 10) (lambda (x) (/ (sin x) x)) :clear t)
  (let* ((pane     (plotter-pane-of 'plt))
         (flag     :SCHEDULED-FOR-FUTURE))
    (capi:apply-in-pane-process-wait-multiple pane nil
                                              (lambda ()
                                                (setf flag :SYNCHRONOUSLY-EXECUTED)))
    flag))

|#
;; The fact that this produces :SYNCHRONOUSLY-EXECUTED shows that
;; APPLY-IN-PANE-PROCESS-WAIT-MULTIPLE synchronously executes the
;; function in sequence.
(tstw)
|#
;; --------------------------------------------

(defun tstuu ()
  (plt:fplot 'plt '(-10 10) (lambda (x) (/ (sin x) x)) :clear t)
  (let* ((pane     (plotter-pane-of 'plt))
         (flag     :scheduled-for-future))
    (capi:apply-in-pane-process pane
                                (lambda ()
                                  (capi:apply-in-pane-process pane
                                                              (lambda ()
                                                                (setf flag :synchronously-performed)))
                                  (ac:send ac:writeln flag)))
    ))

|#
;; The fact that this produces :SYNCHRONOUSLY-PERFORMED shows that, in the CAPI process,
;; APPLY-IN-PANE-PROCESS synchronously executes the function instead
;; of scheduling for some future execution.
(tstuu)
|#

LispPlotter
===========

2-D Data Plotting in Lisp

Step 1: Build the binaries in LispPlotter/mac-c-gluecode-32 for 32-bit LispWorks, i.e., all LispWorks editions including Personal except Enterprise. (For LispWorks Enterprise build LispPlotter/mac-c-gluecode-64/)

cd mac-c-gluecode-32
make plotterStuff                       # sudo may be needed to do a couple mv's that put the built binaries in place -
                                        # unless you use homebrew and have homebrew configured properly in which case
                                        # /usr/local/lib is not owned by root

Step 1b (optional): To build other useful native libraries used by Dr. McClain's
other git repos, i.e., other CL libraries, on macOS Sierra do simply:

make

Step 2: In LispWorks, or in Emacs/Slime connected to LispWorks via
Swank [1], you may execute a convenience script that will allow you to
quickly get it all loaded and working (see load-script.lisp for any changes
indicated for your environment):

(load "load-script.lisp")


[1] This is how to get Emacs/Slime and LispWorks working together via Swank
    (with a few nice extra config items thrown in), save it to, say,
    ~/.lw-init.lisp and then launch LispWorks and do (load "~/.lw-init.lisp"),
    then in Emacs do M-x slime-connect RET and accept the default IP and port:


;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :quicklisp-slime-helper)
(ql:quickload 'swank)
;; The following are just nice to haves.
#+lispworks (mp:initialize-multiprocessing)
#+lispworks (set-default-character-element-type 'simple-char)
#+lispworks (defun utf-8-file-encoding (pathname ef-spec buffer length)
              (declare (ignore pathname buffer length))
              (system:merge-ef-specs ef-spec :utf-8))
#+lispworks
(setq system:*file-encoding-detection-algorithm*
      (substitute 'utf-8-file-encoding
                  'system:locale-file-encoding
                  system:*file-encoding-detection-algorithm*))
;;#+lispworks (setq system:*file-encoding-detection-algorithm* ('utf-8-file-encoding))

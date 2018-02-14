LispPlotter
===========

2-D Data Plotting in Lisp

Step 1: Build the binaries in LispPlotter/mac-c-gluecode-32 for 32-bit LispWorks, i.e., all LispWorks editions including Personal except Enterprise. (For LispWorks Enterprise build LispPlotter/mac-c-gluecode-64/)

    cd mac-c-gluecode-32
    make plotterStuff                       # NOTE: sudo may be needed to do a couple mv's that put the built binaries in place -
                                            # unless you use homebrew and have homebrew configured properly in which case
                                            # /usr/local/lib is not owned by root - assuming you use the default LIBDIR
                                            # location in the makefile

Step 1b (optional): To build LispPlotter _and_ other useful native libraries used by dbmclain's
other git repos, i.e., other CL libraries, on macOS Sierra do simply:

    make

Step 2: Create the following quicklisp and plotter initialization
script (save in ~/.lispworks, or wherever you like, and subsequently (load
"~/.lispworks"). Note: LispWorks Personal Edition doesn't do automatic
initialization so you have to explicitly load.

    #-quicklisp
    (let ((quicklisp-init (merge-pathnames "lw-quicklisp/setup.lisp" ; I like to keep a separate quicklisp install for lispworks
                                          (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))
    (ql:quickload :quicklisp-slime-helper)
    (ql:quickload 'swank)
    (swank:create-server)
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

    ;; If you don't mess with the makefiles for the binaries, this will
    ;; just work on macOS Sierra at least
    #+lispworks
    (setf (logical-pathname-translations "projects")
          '(("dylib;*.*.*" "/usr/local/lib/")
            ("dylib64;*.*.*" "/usr/local/lib/")
            ("lisp;**;" "./;**;"))) ; This assumes you've set the listener's directory, via , cd RET, to the LispPlotter directory.

    ;; Preload plotter if you want.
    ;; (ql:quickload :plotter)
    ;; (plt:fplot 'sinc '(-10 10) (lambda (x) (/ (sin x) x)) :clear t :title "Sinc(x)" :legend "Sinc(x)")

Step 3: (for Emacs/Slime users) M-x slime-connect and take the defaults.


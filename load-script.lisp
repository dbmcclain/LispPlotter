;; Convenience script to load and configure everything
;;
;; Notes for newbs: If you are using Slime with LispWorks, make sure
;; to set Slime's default directory first before executing this next
;; form via ', cd RET' or programmatically, e.g.,
;; (swank:set-default-directory ;; "path/to/LispPlotter/"). Beware there is a
;; regex package available via quicklisp, if you make the mistake of
;; loading that first, just do a '(ql:uninstall "regex") then execute
;; the code below.You can just load everything via asdf since that is
;; how quicklisp is doing it anyway, but it is good to get into the
;; habit of using quicklisp.
;;
;;; Note: Instead of this next mapc form, you could alternatively symlink the
;;; asd files below into the ~/quicklisp/local-projects directory.
(mapc (lambda (system-file) (load system-file))  (list "../C-Arrays/c-arrays.asd"
                                                       "../data-objects/data-objects.asd"
                                                       "../MPCompat/mpcompat.asd"
                                                       "../useful-macros/useful-macros.asd"
                                                       "../regex/regex.asd"
                                                       "../regex/csv.asd"
                                                       "../vmath/vmath.asd"
                                                       "../lazy-streams/lazy-streams.asd"
                                                       "../LispPlotter/plotter.asd"))


;; If you don't mess with the makefiles for the binaries, this will
;; just work on macOS Sierra at least
(setf (logical-pathname-translations "projects")
      '(("dylib;*.*.*" "/usr/local/lib/")
        ("dylib64;*.*.*" "/usr/local/lib/")
        ("lisp;**;" "./;**;")))

;; Just a couple quick checks to make sure the binaries got built and
;; moved into place by make. You don't need to execute these unless
;; you want assurance and are too lazy to switch to a shell.
;; (probe-file (translate-logical-pathname "projects:dylib;liblispfft.dylib"))
;; (probe-file (translate-logical-pathname "projects:dylib64;liblispfft-64.dylib"))

;; This will now use quicklisp to load all the systems we loaded
;; above. It should all just load.
(ql:quickload :plotter)

;; If all went well and you didn't get a stack trace about the dylibs
;; not loading or something, then you can now execute the following to
;; graph a sinusoidal wave.
;; (plt:fplot 'sinc '(-10 10) (lambda (x) (/ (sin x) x)) :clear t :title "Sinc(x)" :legend "Sinc(x)")

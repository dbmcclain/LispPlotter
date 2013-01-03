
(defvar *siglab-lib*
  (fli:register-module :siglab-lib
                       :real-name
                       (translate-logical-pathname
                        #+:LISPWORKS-32BIT "PROJECTS:DYLIB;libLispSigLab.dylib"
                        #+:LISPWORKS-64BIT "PROJECTS:DYLIB64;libLispSigLab-64.dylib")))

(fli:define-foreign-function (d/ "siglab_float_divide" :source)
    ((a :double)
     (b :double))
  :result-type :double
  :module *siglab-lib*)

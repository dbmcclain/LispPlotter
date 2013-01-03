
GCC=g++ -dynamiclib -I$(HOME)/projects/include -O2 -msse3 -mfpmath=sse -m64

all: fftglue siglabGlue memcof svd \
	plotterStuff fdfilestuff cludp

svd: svdcmp.cpp svbksb.cpp pythag.cpp nrutil.cpp nrutil.h lisp_dgesvd.cpp
	$(GCC) -o libLispNrUtil-64.dylib svbksb.cpp svdcmp.cpp pythag.cpp \
		nrutil.cpp lisp_dgesvd.cpp -lstdc++ -framework Accelerate
	mv libLispNrUtil-64.dylib /usr/local/lib64

# For PPC/Altivec Machines
#
# fftglue: fft_glue.cpp
# 	$(GCC) -o libLispFFT-64.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
# 		-lstdc++ -lfftw3 -framework VecLib -framework Accelerate
# 	mv libLispFFT.dylib /usr/local/lib64

#
# For Intel Duo-Core x86 Machines
#
fftglue: fft_glue.cpp
	$(GCC) -o libLispFFT-64.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispFFT-64.dylib /usr/local/lib64

fwtglue: fwt2d.cpp
	$(GCC) -o libFWT2D-64.dylib fwt2d.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libFWT2D-64.dylib /usr/local/lib64

# aqtPlotter: lisp-aqtplotter.mm
# 	$(GCC) -o libLispAquaTermPlotter-64.dylib \
# 		lisp-aqtplotter.mm                      \
# 		-lstdc++ -laquaterm
# 	mv libLispAquaTermPlotter-64.dylib /usr/local/lib64

plotterStuff: plotter-stuff.mm
	$(GCC) -o libLispPlotterStuff-64.dylib \
		plotter-stuff.mm		\
		-lstdc++ -framework AppKit
	mv libLispPlotterStuff-64.dylib /usr/local/lib64

siglabGlue: siglab_glue.cpp
	$(GCC) -o libLispSigLab-64.dylib siglab_glue.cpp -lmx -lm \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispSigLab-64.dylib /usr/local/lib64


# lwbdb: lwbdb_intf.cpp
# 	$(GCC) -o libLispBDB-64.dylib \
# 		-I/usr/local/BerkeleyDB.4.4/include \
# 		-I~/projects/include \
# 		lwbdb_intf.cpp \
# 		-L/usr/local/BerkeleyDB.4.4/lib \
# 		-lSciDS-II -lFA -ldb -lstdc++
# 	mv libLispBDB-64.dylib /usr/local/lib64

memcof: burg.cpp
	$(GCC) -o libLispBurg-64.dylib burg.cpp -lmx -lm \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispBurg-64.dylib /usr/local/lib64

fdfilestuff: fdfilestuff.cpp
	$(GCC) -o libLispFDFileStuff-64.dylib fdfilestuff.cpp -lmx -lm \
		-lstdc++
	mv libLispFDFileStuff-64.dylib /usr/local/lib64

cludp: cludp.c cludp.h
	$(GCC) -o libLispClUDP-64.dylib cludp.c -lmx -lm -lstdc++
	mv libLispClUDP-64.dylib /usr/local/lib64



GCC=g++ -dynamiclib -I$(HOME)/projects/include -O2 -mfpmath=sse -msse

all: fftglue aqtPlotter siglabGlue lwbdb memcof svd \
	plotterStuff fdfilestuff cludp

svd: svdcmp.cpp svbksb.cpp pythag.cpp nrutil.cpp nrutil.h lisp_dgesvd.cpp
	$(GCC) -o libLispNrUtil.dylib svbksb.cpp svdcmp.cpp pythag.cpp \
		nrutil.cpp lisp_dgesvd.cpp -lstdc++ -framework Accelerate
	mv libLispNrUtil.dylib /usr/local/lib

# For PPC/Altivec Machines
#
# fftglue: fft_glue.cpp
# 	$(GCC) -o libLispFFT.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
# 		-lstdc++ -lfftw3 -framework VecLib -framework Accelerate
# 	mv libLispFFT.dylib /usr/local/lib

#
# For Intel Duo-Core x86 Machines
#
fftglue: fft_glue.cpp
	$(GCC) -o libLispFFT.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispFFT.dylib /usr/local/lib

fwtglue: fwt2d.cpp
	$(GCC) -o libFWT2D.dylib fwt2d.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libFWT2D.dylib /usr/local/lib

aqtPlotter: lisp-aqtplotter.mm
	$(GCC) -o libLispAquaTermPlotter.dylib \
		lisp-aqtplotter.mm                      \
		-lstdc++ -laquaterm
	mv libLispAquaTermPlotter.dylib /usr/local/lib

plotterStuff: plotter-stuff.mm
	$(GCC) -o libLispPlotterStuff.dylib \
		plotter-stuff.mm		\
		-lstdc++ -framework AppKit
	mv libLispPlotterStuff.dylib /usr/local/lib

siglabGlue: siglab_glue.cpp
	$(GCC) -o libLispSigLab.dylib siglab_glue.cpp -lmx -lm \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispSigLab.dylib /usr/local/lib


lwbdb: lwbdb_intf.cpp
	$(GCC) -o libLispBDB.dylib \
		-I/usr/local/BerkeleyDB.4.4/include \
		-I~/projects/include \
		lwbdb_intf.cpp \
		-L/usr/local/BerkeleyDB.4.4/lib \
		-lSciDS-II -lFA -ldb -lstdc++
	mv libLispBDB.dylib /usr/local/lib

memcof: burg.cpp
	$(GCC) -o libLispBurg.dylib burg.cpp -lmx -lm \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libLispBurg.dylib /usr/local/lib

fdfilestuff: fdfilestuff.cpp
	$(GCC) -o libLispFDFileStuff.dylib fdfilestuff.cpp -lmx -lm \
		-lstdc++
	mv libLispFDFileStuff.dylib /usr/local/lib

cludp: cludp.c cludp.h
	$(GCC) -o libLispClUDP.dylib cludp.c -lmx -lm -lstdc++
	mv libLispClUDP.dylib /usr/local/lib

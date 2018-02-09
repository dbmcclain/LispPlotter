
GCC=g++ -dynamiclib -Iinclude -O2 -msse3 -mfpmath=sse -m64

all: fftglue siglabGlue memcof svd \
	plotterStuff fdfilestuff cludp

# Note: translate-logical-pathname translates logical to uppercase and then to
# platform preferred case which on macOS Sierra is lowercase, hence we use that
# case, lowercase, in the lib names 

svd: svdcmp.cpp svbksb.cpp pythag.cpp nrutil.cpp nrutil.h lisp_dgesvd.cpp
	$(GCC) -o liblispnrutil-64.dylib svbksb.cpp svdcmp.cpp pythag.cpp \
		nrutil.cpp lisp_dgesvd.cpp -lstdc++ -framework Accelerate
	mv liblispnrutil-64.dylib /usr/local/lib

# For PPC/Altivec Machines
#
# fftglue: fft_glue.cpp
# 	$(GCC) -o libLispFFT-64.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
# 		-lstdc++ -lfftw3 -framework VecLib -framework Accelerate
# 	mv libLispFFT.dylib /usr/local/lib

#
# For Intel Duo-Core x86 Machines
#
fftglue: fft_glue.cpp
	$(GCC) -o liblispfft-64.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++  -framework Accelerate
	mv liblispfft-64.dylib /usr/local/lib

# TODO Broken on macOS Sierra
fwtglue: fwt2d.cpp
	$(GCC) -o libFWT2D-64.dylib fwt2d.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++ -framework VecLib -framework Accelerate
	mv libFWT2D-64.dylib /usr/local/lib

# TODO Broken on macOS Sierra - isn't this obselete anyway?
aqtPlotter: lisp-aqtplotter.mm
	$(GCC) -o libLispAquaTermPlotter-64.dylib \
		lisp-aqtplotter.mm                      \
		-lstdc++ -laquaterm
	mv libLispAquaTermPlotter-64.dylib /usr/local/lib

plotterStuff: plotter-stuff.mm
	$(GCC) -o liblispplotterstuff-64.dylib \
		plotter-stuff.mm		\
		-lstdc++ -framework AppKit
	mv liblispplotterstuff-64.dylib /usr/local/lib

siglabGlue: siglab_glue.cpp
	$(GCC) -o liblispsiglab-64.dylib siglab_glue.cpp -lmx -lm \
		-lstdc++  -framework Accelerate
	mv liblispsiglab-64.dylib /usr/local/lib


# TODO Broken on macOS Sierra; you'd have to update the paths below
lwbdb: lwbdb_intf.cpp
	$(GCC) -o libLispBDB-64.dylib \
		-I/usr/local/BerkeleyDB.4.4/include \
		-I~/projects/include \
		lwbdb_intf.cpp \
		-L/usr/local/BerkeleyDB.4.4/lib \
		-lSciDS-II -lFA -ldb -lstdc++
	mv libLispBDB-64.dylib /usr/local/lib

memcof: burg.cpp
	$(GCC) -o liblispburg-64.dylib burg.cpp -lmx -lm \
		-lstdc++  -framework Accelerate
	mv liblispburg-64.dylib /usr/local/lib

fdfilestuff: fdfilestuff.cpp
	$(GCC) -o liblispfdfilestuff-64.dylib fdfilestuff.cpp -lmx -lm \
		-lstdc++
	mv liblispfdfilestuff-64.dylib /usr/local/lib

cludp: cludp.c cludp.h
	$(GCC) -o liblispcludp-64.dylib cludp.c -lmx -lm -lstdc++
	mv liblispcludp-64.dylib /usr/local/lib


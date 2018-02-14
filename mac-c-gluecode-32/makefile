LIBDIR=/usr/local/lib
GCC=g++ -dynamiclib -Iinclude -O2 -mfpmath=sse -msse -m32

# all: fftglue aqtPlotter siglabGlue lwbdb memcof svd \
# 	plotterStuff fdfilestuff cludp

all: fftglue siglabGlue memcof svd \
	plotterStuff fdfilestuff cludp

svd: svdcmp.cpp svbksb.cpp pythag.cpp nrutil.cpp nrutil.h lisp_dgesvd.cpp
	$(GCC) -o liblispnrutil.dylib svbksb.cpp svdcmp.cpp pythag.cpp \
		nrutil.cpp lisp_dgesvd.cpp -lstdc++ -framework Accelerate
	mv liblispnrutil.dylib $(LIBDIR)

# For PPC/Altivec Machines
#
# fftglue: fft_glue.cpp
# 	$(GCC) -o libLispFFT.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
# 		-lstdc++ -lfftw3 -framework VecLib -framework Accelerate
# 	mv libLispFFT.dylib $(LIBDIR)

#
# For Intel Duo-Core x86 Machines
#
fftglue: fft_glue.cpp
	$(GCC) -o liblispfft.dylib fft_glue.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++  -framework Accelerate
	mv liblispfft.dylib $(LIBDIR)

# TODO Broken build on macOS Sierra
fwtglue: fwt2d.cpp
	$(GCC) -o libFWT2D.dylib fwt2d.cpp vdsp_intf.cpp tlocker.cpp \
		-lstdc++  -framework Accelerate
	mv libFWT2D.dylib $(LIBDIR)

# TODO Broken build on macOS Sierra - is this obselete anyway?
aqtPlotter: lisp-aqtplotter.mm
	$(GCC) -o libLispAquaTermPlotter.dylib \
		lisp-aqtplotter.mm                      \
		-lstdc++ -laquaterm
	mv libLispAquaTermPlotter.dylib $(LIBDIR)

plotterStuff: plotter-stuff.mm
	$(GCC) -o liblispplotterstuff.dylib \
		plotter-stuff.mm		\
		-lstdc++ -framework AppKit
	mv liblispplotterstuff.dylib $(LIBDIR)

siglabGlue: siglab_glue.cpp
	$(GCC) -o liblispsiglab.dylib siglab_glue.cpp -lmx -lm \
		-lstdc++  -framework Accelerate
	mv liblispsiglab.dylib $(LIBDIR)

# TODO This would need to be updated to work
lwbdb: lwbdb_intf.cpp
	$(GCC) -o libLispBDB.dylib \
		-I/usr/local/BerkeleyDB.4.4/include \
		-I~/projects/include \
		lwbdb_intf.cpp \
		-L/usr/local/BerkeleyDB.4.4/lib \
		-lSciDS-II -lFA -ldb -lstdc++
	mv libLispBDB.dylib $(LIBDIR)

memcof: burg.cpp
	$(GCC) -o liblispburg.dylib burg.cpp -lmx -lm \
		-lstdc++  -framework Accelerate
	mv liblispburg.dylib $(LIBDIR)

fdfilestuff: fdfilestuff.cpp
	$(GCC) -o liblispfdfilestuff.dylib fdfilestuff.cpp -lmx -lm \
		-lstdc++
	mv liblispfdfilestuff.dylib $(LIBDIR)

cludp: cludp.c cludp.h
	$(GCC) -o liblispcludp.dylib cludp.c -lmx -lm -lstdc++
	mv liblispcludp.dylib $(LIBDIR)

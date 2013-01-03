// fft_intf.h -- This is the DLL interface to FFTX
//
// DM/MCFA  12/01
// ----------------------------------------------------

extern "C" {
  __declspec(dllexport) void GetVersionString(char *buf, int nbuf);
  __declspec(dllexport) int d2zfftf(int nx, int ny, double *src, double *dst);
  __declspec(dllexport) int z2dfftf(int nx, int ny, double *src, double *dst);
  __declspec(dllexport) int z2zfftf(int nx, int ny, double *src, double *dst, int direction);
};

// -- end of fft_intf.h -- //

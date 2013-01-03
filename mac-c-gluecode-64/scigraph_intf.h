// scigraph_intf.h -- Interface to Lisp_Plotter DLL
//
// DM/MCFA  12/01
// ---------------------------------------------------

struct Twindow_setup
{
  int wid;
  int xpos;
  int ypos;
  int xsize;
  int ysize;
  char *title;
  int bgcolor;
};

struct Timage_parms
{
  float  *parr;
  int    xsiz;
  int    ysiz;
  int    xpos;
  int    ypos;
  int    filler;
  float   minval;
  float   maxval;
  float   magn;
  int    neg;
  int    flipv;
  int    fliph;
};

struct Taxes_parms
{
  float  xmin;
  float  xmax;
  float  ymin;
  float  ymax;
  float  aspect;
  int   xlog;
  int   ylog;
  char  *xtitle;
  char  *ytitle;
  char  *title;
  int   bgcolor;
  int   fgcolor;
};

struct Tdata_parms
{
  int    npts;
  float *pxarr;
  float *pyarr;
  int    sym;
  int    color;
  int    thick;
  int    clip;
  int    penpat;
};

struct Tscale_parms
{
  int    nel;
  float  *parr;
  int    log;
  int    filler;
  float   minval;
  float   maxval;
};

struct POINT
{
  int  x;
  int  y;
};

struct VertInfo
{
  int   m_color;
  int   m_nverts;
  POINT *m_pts;     // (int x, int y)
};

struct Tpolys_parms
{
  int       npolys;
  VertInfo  *polys;
};

struct Twindow_info
{
  int xsize;
  int ysize;
};

struct Tmouse_info {
  float mousex;
  float mousey;
  float mousez;
};

typedef void *TNonmaskedBM;
typedef void *TMaskedBM;
typedef int  HBITMAP;
typedef int  HWND;

#define LISP_PLOTTER_API  __declspec(dllexport)

extern "C" {
  
  LISP_PLOTTER_API void lpSelectWindow(int wid);
  LISP_PLOTTER_API void lpShowWindow(int wid);
  LISP_PLOTTER_API void lpEraseWindow(int bg);
  LISP_PLOTTER_API void lpKillWindow(int wid);
  LISP_PLOTTER_API void lpOpenWindow(Twindow_setup *p);
  LISP_PLOTTER_API void lpAttachWindow(HWND hWnd);
  LISP_PLOTTER_API void lpShowImage(Timage_parms *p);
  LISP_PLOTTER_API void lpPlotAxes(Taxes_parms *p);
  LISP_PLOTTER_API void lpPlotData(Tdata_parms *p);
  LISP_PLOTTER_API void lpAutoscale(Tscale_parms *p);
  LISP_PLOTTER_API void lpPlotPolys(Tpolys_parms *p);
  LISP_PLOTTER_API void lpUpdateWindow();
  LISP_PLOTTER_API void lpDelayUpdateWindow();
  LISP_PLOTTER_API void lpGetWindowSize(Twindow_info *p);
  LISP_PLOTTER_API void lpSetCMap(unsigned char *cred, 
				  unsigned char *cgreen,
				  unsigned char *cblue);
  LISP_PLOTTER_API void lpGetMouse(Tmouse_info *p);
  LISP_PLOTTER_API void lpGetCoordValues(Tmouse_info *p);
  LISP_PLOTTER_API void lpCopyToClipboard();
  LISP_PLOTTER_API void lpSaveToBacking();
  LISP_PLOTTER_API int lpGetCursorHandle(char *cursor_filename);
  LISP_PLOTTER_API void lpDirectRedraw(HWND hWnd);
  LISP_PLOTTER_API void lpPromptForMultipleFiles(char *title, char *buf,
						 int buflen);
  
  // -----------------------------------------------------------------
  // Retro stuff...
  
  LISP_PLOTTER_API TNonmaskedBM* lpReadNonmaskedBM(const char* fname);
  LISP_PLOTTER_API TMaskedBM* lpReadMaskedBM(const char* fname, int trans);
  LISP_PLOTTER_API void lpDrawNonmasked(TNonmaskedBM* pbm,
					int ulc_x, int ulc_y,
					int bm_ulc_x, int bm_ulc_y,
					int bm_wd, int bm_ht);
  LISP_PLOTTER_API void lpDrawMasked(TMaskedBM* pbm,
				     int ulc_x, int ulc_y,
				     int bm_ulc_x, int bm_ulc_y,
				     int bm_wd, int bm_ht);
  LISP_PLOTTER_API void lpTileBG(TNonmaskedBM* pbm,
				 int ulc_x, int ulc_y, int wd, int ht,
				 int bm_ulc_x, int bm_ulc_y,
				 int bm_wd, int bm_ht);
  LISP_PLOTTER_API void lpDiscardBitmap(TNonmaskedBM *pbm);
  LISP_PLOTTER_API int lpClipDrawing(int clip_left, int clip_top,
				      int clip_width, int clip_height);
  LISP_PLOTTER_API void lpUnclipDrawing(int sav);
  LISP_PLOTTER_API void lpLockDrawing();
  LISP_PLOTTER_API void lpUnlockDrawing();
  LISP_PLOTTER_API HBITMAP lpGetImageBits(int left, int top,
					  int wd, int ht);
  LISP_PLOTTER_API void lpSetImageBits(HBITMAP hbm, int left,
				       int top, int wd, int ht);
  LISP_PLOTTER_API void lpDiscardImageBits(HBITMAP hbm);
  
};  // extern "C"

// -- end of scigraph_intf.h -- //


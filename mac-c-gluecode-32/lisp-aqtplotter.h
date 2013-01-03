// lisp-aqtplotter.h -- Declarations for AquaTerm Graphics under Lisp
//
// DM/MCFA  02/99
// AquaTerm mods 06/04 DM
// ---------------------------------------------------------

#ifndef __GRAPHICS_H__
#define __GRAPHICS_H__

#define DEFAULT_WIN_ID  0
#define DEFAULT_X_POS   50
#define DEFAULT_Y_POS   50
#define DEFAULT_X_SIZE  400
#define DEFAULT_Y_SIZE  300
#define DEFAULT_BG      0x0FFFFFF   // white

class TImage
{
 public:
  int            m_width;
  int            m_height;
  // int            m_xlen;
  // unsigned char *m_bits;
  float          m_xmag;
  float          m_ymag;
  int            m_xpos;
  int            m_ypos;
  float         m_sf;
  float         m_offset;
  bool           m_flipv;
  bool           m_fliph;
	
  float          m_xorg;
  float          m_xsf;
  float          m_yorg;
  float          m_ysf;

  bool           m_xlog;
  bool           m_ylog;

  float          m_xmin;  // same as m_xorg
  float          m_xmax;
  float          m_ymin; // same as m_yorg
  float          m_ymax;

  TImage(int xsiz, int ysiz, 
	 int xpos, int ypos,
	 float xmag, float ymag,
	 float off, float sf,
	 bool flipv, bool fliph,
	 float xmin, float xmax,
	 float ymin, float ymax, 
	 bool xlog, bool ylog);

  virtual ~TImage();

  int get_xpos()
  { return m_xpos; }
  
  int get_ypos()
  { return m_ypos; }

  int get_width()
  { return m_width; }

  int get_height()
  { return m_height; }

  float get_xmag()
  { return m_xmag; }

  float get_ymag()
  { return m_ymag; }

  bool needs_hflip()
  { return m_fliph; }

  bool needs_vflip()
  { return m_flipv; }

  
  // bool get_pixel(long wx, long wy, float &fx, float &fy, float &val);
};

struct PlotInfo
{
  float m_xmin;
  float m_xmax;
  float m_ymin;
  float m_ymax;
  bool   m_xlog;
  bool   m_ylog;
  char  *m_xtitle;
  char  *m_ytitle;
  char  *m_title;
  float m_aspect;
  long   m_bg;
  long   m_fg;
  bool   m_fullgrid;
  bool   m_ticks_inside;

  long get_bg()
  { return m_bg; }

  long get_fg()
  { return m_fg; }

  float get_xmin()
  { return m_xmin; }

  float get_xmax()
  { return m_xmax; }

  float get_ymin()
  { return m_ymin; }

  float get_ymax()
  { return m_ymax; }

  char* get_xtitle()
  { return m_xtitle; }

  char* get_ytitle()
  { return m_ytitle; }

  char* get_title()
  { return m_title; }

  bool has_log_xaxis()
  { return m_xlog; }

  bool has_log_yaxis()
  { return m_ylog; }
  
  bool has_ticks_inside()
  { return m_ticks_inside; }

  bool has_fullgrid()
  { return m_fullgrid; }
};

struct TPoint
{
  long  x;
  long  y;
};

struct TVertInfo
{
  long   m_color;
  long   m_nverts;
  TPoint *m_pts;
};

struct TPolysParms
{
  long       m_npolys;
  TVertInfo *m_polys;
};

struct WSizeInfo
{
  long    m_xsize;
  long    m_ysize;
  long    m_left;
  long    m_top;
};

struct DataInfo
{
  long    m_npts;
  float *m_xvals;
  float *m_yvals;
  int     m_psym;
  long    m_color;
  long    m_thick;
  bool    m_clip;
  long    m_penpat;
  long    m_alpha;

  int get_plotting_symbol()
  { return m_psym; }

  long get_color()
  { return m_color; }

  long get_npts()
  { return m_npts; }

  long get_line_thickness()
  { return m_thick; }

  float* get_xvalues()
  { return m_xvals; }

  float* get_yvalues()
  { return m_yvals; }
};

struct RECT
{
  float  m_left;
  float  m_top;
  float  m_right;
  float  m_bottom;

  void set_tlbr(float top, float left, float bottom, float right)
  { m_top = top; m_left = left; m_bottom = bottom; m_right = right; }

  float get_left()
  { return m_left; }

  float get_right()
  { return m_right; }

  float get_top()
  { return m_top; }
  
  float get_bottom()
  { return m_bottom; }

  float get_width()
  { return (m_right - m_left); }

  float get_height()
  { return (m_top - m_bottom); }
};

struct WindowInfo;
struct PlotRec
{
  float  m_xmin;
  float  m_xsf;
  float  m_ymin;
  float  m_ysf;
  RECT   m_plotrect;
  float  m_ygap;
  float  m_xgap;
  bool   m_xlog;
  bool   m_ylog;
  float  m_xmax;
  float  m_ymax;

  PlotRec(TImage *p);
  PlotRec(WindowInfo *wp, PlotInfo *p);
  PlotRec(WindowInfo *wp);
  
  virtual ~PlotRec();

  void set_plotrect(float top, float left, float bottom, float right)
  { m_plotrect.set_tlbr(top, left, bottom, right); }

  float get_xmin()
  { return m_xmin; }

  float get_xmax()
  { return m_xmax; }

  float get_ymin()
  { return m_ymin; }

  float get_ymax()
  { return m_ymax; }

  void set_xgap(float gap)
  { m_xgap = gap; }

  void set_ygap(float gap)
  { m_ygap = gap; }

  void set_xaxis_sf(float sf)
  { m_xsf = sf; }

  float get_xaxis_sf()
  { return m_xsf; }

  float get_yaxis_sf()
  { return m_ysf; }
  
  void set_yaxis_sf(float sf)
  { m_ysf = sf; }

  RECT& get_plotrect()
  { return m_plotrect; }
  
  float get_plotrect_left()
  { return m_plotrect.get_left(); }

  float get_plotrect_right()
  { return m_plotrect.get_right(); }

  float get_plotrect_top()
  { return m_plotrect.get_top(); }
  
  float get_plotrect_bottom()
  { return m_plotrect.get_bottom(); }

  float get_xgap()
  { return m_xgap; }

  float get_ygap()
  { return m_ygap; }

  bool has_log_xaxis()
  { return m_xlog; }

  bool has_log_yaxis()
  { return m_ylog; }

};

struct WindowInfo
{
  WindowInfo *m_next;
  int         m_id;
  TImage     *m_image;
  long        m_bg;
  PlotRec    *m_plot;
  long        m_delay;
  long        m_xsize;
  long        m_ysize;

  WindowInfo(int winid);
  virtual ~WindowInfo();

  void init(int xsize, int ysize, long bg);
  void reset();
  
  void set_image(TImage *img, bool discard_plot_if_present = true);
  void set_plot(PlotRec *p);

  void discard_plot();
  void discard_image();

  void set_bg(long bg)
  { m_bg = bg; }

  WindowInfo* get_next()
  { return m_next; }

  void set_next(WindowInfo *p)
  { m_next = p; }

  long get_id()
  { return m_id; }

  // -------------------------------
  void reset_delay()
  { m_delay = 0; }

  void increment_delay()
  { ++m_delay; }

  void decrement_delay()
  { --m_delay; }

  bool not_delayed()
  { return (m_delay <= 0); }

  bool is_delayed()
  { return (m_delay > 0); }
  
  // -------------------------------
  long get_width()
  { return m_xsize; }

  long get_height()
  { return m_ysize; }
};

// -----------------------------------------------------
// Primitive graphics operations

enum { DATA_UNITS     = 1,
       FRACTION_UNITS = 2,
       PIXEL_UNITS    = 3 };

union drawable_point
{
  float f[2];
  long  i[2];
};

struct drawable_line
{
  drawable_point* origin;
  drawable_point* extent;
};

struct drawable_rectangle
{
  drawable_point* origin;
  drawable_point* extent;
};

struct drawable_rounded_rectangle
{
  drawable_point* origin;
  drawable_point* extent;
};

struct drawable_ellipse
{
  drawable_point* origin;
  drawable_point* extent;
};

struct drawable_arc
{
  drawable_point* origin;
  drawable_point* extent;
  float*         start_angle;
  float*         stop_angle;
};

enum { FACE_BOLD,
       FACE_ITALIC,
       FACE_UNDERLINE };

struct text_face
{
  char* face_name;
  long  face_size;
  // face_styles list
};


struct drawable_text
{
  drawable_point* origin;
  text_face*      face;
  char*           str;
};

union drawable_element
{
  drawable_point             point;
  drawable_line              line;
  drawable_rectangle         rectangle;
  drawable_rounded_rectangle rounded_rectangle;
  drawable_ellipse           ellipse;
  drawable_arc               arc;
  drawable_text              text;
};

enum { GROP_SRC_COPY  = 0,
       GROP_SRC_OR    = 1,
       GROP_SRC_AND   = 2,
       GROP_SRC_XOR   = 3,
       GROP_SRC_CLEAR = 4 };

enum { ANCHOR_CTR,
       ANCHOR_N,
       ANCHOR_NE,
       ANCHOR_E,
       ANCHOR_SE,
       ANCHOR_S,
       ANCHOR_SW,
       ANCHOR_W,
       ANCHOR_NW };

enum { PENPAT_SOLID,
       PENPAT_DASH,
       PENPAT_DOT,
       PENPAT_DASHDOT,
       PENPAT_DASHDOTDOT,
       PENPAT_NULL,
       PENPAT_INSIDE_FRAME };

enum { DR_POINT,
       DR_LINE,
       DR_RECTANGLE,
       DR_ROUNDED_RECTANGLE,
       DR_ELLIPSE,
       DR_ARC,
       DR_CHORD,
       DR_PIE,
       DR_TEXT };

struct drawable
{
  drawable_element* elt;
  long   grop;         // int
  long   pen_pat;      // int
  long   thick;        // int
  long   border_color; // long
  long   is_filled;    // bool
  long   fill_color;   // long
  long   anchor;       // int
};

#endif // __GRAPHICS_H__

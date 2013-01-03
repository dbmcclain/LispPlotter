// lisp-aqtplotter.mm -- Graphics and Plotting for Lisp
// DM/MCFA  06/04
//
// make with:
//
// gcc -dynamiclib -o libLispAquaTermPlotter.dylib \
//     lisp-aqtplotter.mm                          \
//     -lstdc++ -laquaterm

extern "C" {
#import <aquaterm/aquaterm.h>
};

#include <stdio.h>
#include <math.h>
#include <float.h>
#include <iostream>

#include "lisp-aqtplotter.h"

// -----------------------------------------------
// TSmartWindowInfo -- a class that encapsulates a WindowInfo pointer
// so that if it is ever used in a dereference it will automatically
// construct a default window if it has never been used before....
//
// This avoids incessant checking for non-null pointers...
//
class TSmartWindowInfo
{
  WindowInfo* p;

  void ensure_window_exists();
  
public:
  TSmartWindowInfo()
  { p = 0; }

  WindowInfo* operator=(WindowInfo* aPtr)
  { p = aPtr; }

  operator WindowInfo*();
  
  WindowInfo* operator->();
};

// ------------------------------------------------
extern WindowInfo* _open_default_window(int wid);

void TSmartWindowInfo::ensure_window_exists()
{
  if(0 == p)
    p = _open_default_window(0);
}

TSmartWindowInfo::operator WindowInfo*()
{
  ensure_window_exists();
  return p;
}

WindowInfo* TSmartWindowInfo::operator->()
{
  ensure_window_exists();
  return p;
}

TSmartWindowInfo hWndMain;	// handle to current top window

// ----------------------------------------------------------
WindowInfo *winlist = 0;

void NI(char *msg)
{
  std::cerr << "Not Implemented:" << msg << std::endl;
}

extern WindowInfo* _open_window(int winid, int xsize, int ysize,
				const char *title, int bg);

extern "C" void lpSetHeatColormap(void);

extern "C"
int lpInitAQT()
{
  int ok = (0 == aqtInit());
  if(ok)
    {
      // std::cerr << "*** Starting Lispworks Aquaterm Connection ***" << std::endl;
      lpSetHeatColormap();
#if 0
      _open_window(1, 400, 300, "SciGraph #1", 0x0ffffff);
      aqtAddLabel("Hello from LispWorks!", 200.0, 150.0, 45.0, AQTAlignCenter);
      aqtRenderPlot();
#endif	
    }
  return ok;
}

// -----------------------------------------------------

TImage::TImage(int xsiz, int ysiz, 
	       int xpos, int ypos,
	       float xmag, float ymag,
	       float off, float sf,
	       bool flipv, bool fliph,
	       float xmin, float xmax,
	       float ymin, float ymax, 
	       bool xlog, bool ylog)
  : m_width(xsiz), m_height(ysiz), 
    m_xpos(xpos), m_ypos(ypos),
    m_xmag(xmag), m_ymag(ymag), 
    m_sf(sf), m_offset(off),
    m_flipv(flipv), m_fliph(fliph),
    m_xorg(xmin), m_yorg(ymin), 
    m_xlog(xlog), m_ylog(ylog),
    m_xmin(xmin), m_xmax(xmax), 
    m_ymin(ymin), m_ymax(ymax)
{
  if(m_xlog)
    {
      m_xsf = log((float)xsiz) / (float)xsiz;
      m_xorg = xmax / (float)xsiz;
    }
  else
    m_xsf = (xmax - xmin) / xsiz;
  
  if(m_ylog)
    {
      m_ysf = log((float)ysiz) / (float)ysiz;
      m_yorg = ymax / (float)ysiz;
    }
  else
    m_ysf = (ymax - ymin) / ysiz;
}

TImage::~TImage()
{ }

// -----------------------------------------------------
PlotRec::PlotRec(TImage *p)
{
  m_plotrect.set_tlbr(p->get_ypos() + int(p->get_ymag() * p->get_height()),
		      p->get_xpos(),
		      p->get_ypos(),
		      p->get_xpos() + int(p->get_xmag() * p->get_width()));
  
  m_ygap = 0;
  m_xgap = 0;
  m_xlog = false;
  m_ylog = false;
  m_xmin = p->needs_hflip() ? (float)(p->m_width - 1) : 0;
  m_ymin = p->needs_vflip() ? 0 : (float)(p->m_height - 1);
  m_xmax = p->needs_hflip() ? 0 : (float)(p->m_width - 1);
  m_ymax = p->needs_vflip() ? (float)(p->m_height-1) : 0;
  m_xsf  = p->needs_hflip() ? -p->m_xmag : p->m_xmag;
  m_ysf  = p->needs_vflip() ? p->m_ymag  : -p->m_ymag;	
}

PlotRec::PlotRec(WindowInfo *wp, PlotInfo *p)
{
  m_xmin     = p->get_xmin();
  m_xmax     = p->get_xmax();
  m_xsf      = 0.0;
  m_ymin     = p->get_ymin();
  m_ymax     = p->get_ymax();
  m_ysf      = 0.0;
  m_plotrect.set_tlbr(wp->get_height(), 0, 0, wp->get_width());
  m_ygap     = 0;
  m_xgap     = 0;
  m_xlog     = p->has_log_xaxis();
  m_ylog     = p->has_log_yaxis();
}

PlotRec::PlotRec(WindowInfo *wp)
{
  m_xmin = 0.0;
  m_xmax = wp->get_width();
  m_xsf  = 1.0;
  m_ymin = 0.0;
  m_ymax = wp->get_height();
  m_ysf  = 1.0;
  m_plotrect.set_tlbr(wp->get_height(), 0, 0, wp->get_width());
  m_ygap = 0;
  m_xgap = 0;
  m_xlog = false;
  m_ylog = false;
}

PlotRec::~PlotRec()
{ }

// -----------------------------------------------------

WindowInfo::WindowInfo(int winid)
{
  m_next = winlist;
  m_image = 0;
  m_plot = 0;
  m_id = winid;
  m_bg = 0;
  m_delay = 0;
  m_xsize = 0;
  m_ysize = 0;
  winlist = this;
}

WindowInfo::~WindowInfo()
{  
  WindowInfo *p, *q;
  for(q = 0, p = winlist; p; q = p, p = p->get_next())
    {
      if(p == this)
	{
	  if(q)
	    q->set_next(m_next);
	  else
	    winlist = m_next;
	  break;
	}
    }
  if(hWndMain->get_id() == m_id)
    hWndMain = winlist ? winlist->get_next() : 0;
  
  reset();
}

void WindowInfo::reset()
{
  discard_image();
  discard_plot();
}

void WindowInfo::init(int xsize, int ysize, int bg)
{
  m_xsize = xsize;
  m_ysize = ysize;
  m_bg = bg;
}

void WindowInfo::discard_image()
{
  if(m_image)
    {
      delete m_image;
      m_image = 0;
    }
}

void WindowInfo::set_image(TImage *img, bool discard_plot_if_present)
{
  discard_image();
  if(discard_plot_if_present)
    discard_plot();
  m_image = img;
}

void WindowInfo::discard_plot()
{
  if(m_plot)
    {
      delete m_plot;
      m_plot = 0;
    }
}

void WindowInfo::set_plot(PlotRec *p)
{
  discard_image();
  discard_plot();
  m_plot = p;
}

// -----------------------------------------------------

WindowInfo* find_window_from_id(int wid)
{
  WindowInfo *p;
  for(p = winlist; p; p = p->get_next())
    {
      if(p->get_id() == wid)
	break;
    }
  return p;
}

void setPenColor(int fg)
{
  int alpha = ((fg & 0xFF000000) >> 24);
  if(0 == alpha)
    alpha = 255;
  aqtSetAlphaColor(((float)(fg & 0x0FF)/255),
		   ((float)((fg & 0x0FF00) >> 8)/255),
		   ((float)((fg & 0x0FF0000) >> 16)/255),
		   (float)alpha/255);
}

void setPenColor(int red, int green, int blue, int alpha = 255)
{
  aqtSetAlphaColor((float)red/255,
		   (float)green/255,
		   (float)blue/255,
		   (float)alpha/255);
}

void setBGColor(int fg)
{
  aqtSetBackgroundColor(((float)(fg & 0x0FF)/255),
			((float)((fg & 0x0FF00) >> 8)/255),
			((float)((fg & 0x0FF0000) >> 16)/255));
}

void setBGColor(int red, int green, int blue)
{
  aqtSetBackgroundColor((float)red/255,
			(float)green/255,
			(float)blue/255);  
}

WindowInfo* create_window(int winid, int xsize, int ysize,
			  const char *title, int bg)
{
  WindowInfo *pinfo = find_window_from_id(winid);
  if(0 == pinfo)
    pinfo = new WindowInfo(winid);
  pinfo->init(xsize, ysize, bg);
  aqtOpenPlot(winid);
  aqtSetPlotSize(xsize, ysize);
  aqtSetPlotTitle(title);
  setBGColor(bg);
  return pinfo;
}

void maybe_render()
{
  if(hWndMain->not_delayed())
    aqtRenderPlot();
}

// ---------------------------------------------------------------
float calc_delta(float range)
{
  float x = fabsf(range);
  float y = powf(10.0,floorf(log10f(x)-1.0));
  return (x > 60.0 * y) ? 10.0*y
    : ((x > 24.0 * y) ? 5.0*y
       : ((x > 12.0 * y) ? 2.0*y
	  : y));
}

inline double max(double a, double b)
{
  return (a < b) ? b : a;
}

inline double min(double a, double b)
{
  return (a < b) ? a : b;
}

inline int sgn(float x)
{
  return (x < 0.0) ? (-1) : ((x > 0.0) ? 1 : 0);
}

class TDeltaInfo
{
public:
  double m_x0;
  double m_dx;
  int   m_nl;
  int   m_nu;

  TDeltaInfo(float a, float b);
};

TDeltaInfo::TDeltaInfo(float a, float b)
{
  double sf = pow(10.0,-ceil(log10(max(fabs(a),fabs(b)))));
  double c;

  for(;;)
    {
      double sfa = sf * a;
      double sfb = sf * b;
      double diff = fabs(sfb - sfa);
      
      c = 10.0*ceil(0.1*min(sfa,sfb));
      if(diff > 1.0 &&
	 fabs(c-sfa) <= diff)
	break;
      sf *= 10.0;
    }
  for(double sf2 = 1.0;; sf2 *= 0.1)
    {
      double sfa = sf * sf2 * a;
      double sfb = sf * sf2 * b;
      double sfc = sf2 * c;
      double range = fabs(sfb - sfa);
      
      if(range <= 10.0)
	{
	  double ddiv = ((range > 5.0) ? 1.0
			 : ((range < 2.0) ? 0.2 : 0.5));

	  m_nl = floor(fabs(sfc - sfa) / ddiv);
	  m_nu = floor(fabs(sfb - sfc) / ddiv);
	  
	  m_x0 = (sgn(a)*sgn(b) <= 0 ? 0.0 : sfc / (sf * sf2));
	  m_dx = ddiv / (sf * sf2);

#if 0
	  fprintf(stderr, "x0 = %f, dx = %f, nl = %d, nu = %d\n",
		  m_x0, m_dx, m_nl, m_nu);
#endif  
	  break;
	}
    }
}

void drawLineSegment(float x0, float y0, float x1, float y1)
{
  aqtMoveTo(x0,y0);
  aqtAddLineTo(x1,y1);
}

static
char* fmtval(float val, char *buf)
{
  float absval = fabsf(val);
  
  if(0.0 != absval && (absval < 1.0e-2 || 1.0e4 <= absval))
    {
      sprintf(buf, "%.2e", val);
      char *pos = index(buf, 'e');
      
      if('0' == pos[2])
	strcpy(pos+2, pos+3);	// shorten exponent if leading zero
      if('0' == pos[-1])
	{
	  if('0' == pos[-2])
	    strcpy(pos-3,pos); 	// remove decimal point and trailing zeros
	  else
	    strcpy(pos-1,pos);	// remove trailing zero
	}
    }
  else if(floor(absval) == absval)
    sprintf(buf, "%.0f", val);
  else
    {
      int len = sprintf(buf, "%.3f", val);
      while(--len > 0 && '0' == buf[len])
	;			// remove trailing zeros
      if('.' == buf[len])
	buf[len] = 0;
      else
	buf[len+1] = 0;
    }
  return(buf);
}

inline void chkpt(const char *msg)
{
  //  std::cerr << "Checkpoint: " << msg << std::endl;
}

struct TPlotFrame {
  float  m_xorg;
  float  m_yorg;
  float  m_wd;
  float  m_ht;
  float  m_xgap;
  float  m_ygap;
  bool   m_fullgrid;
  bool   m_ticks_inside;
  int   m_fgcolor;

  float get_xorg()
  { return m_xorg; }

  float get_yorg()
  { return m_yorg; }

  float get_width()
  { return m_wd; }

  float get_height()
  { return m_ht; }

  float get_xgap()
  { return m_xgap; }

  float get_ygap()
  { return m_ygap; }

  bool has_fullgrid()
  { return m_fullgrid; }

  bool has_ticks_inside()
  { return m_ticks_inside; }

  int get_fg()
  { return m_fgcolor; }
};

class TAxisWriter
{
public:
  enum {X_AXIS, Y_AXIS};

  int   m_which;
  float m_vmin;
  float m_vmax;
  float m_sf;
  TPlotFrame *m_frame;
  const char *m_title;
  bool  m_islog;
  int  m_align;
  float m_angle;
  float m_wd;
  float m_ht;
  float m_xorg;
  float m_yorg;
  
  TAxisWriter(int which,
	      float vmin, float vmax,
	      TPlotFrame *pframe,
	      const char *title,
	      bool  islog);
  void draw();
  void cv_scaled_coord(float &x, float &y);
  void cv_absolute_coord(float &x, float &y);
  void draw_scaled_line(float x0, float y0, float x1, float y1);
  void draw_absolute_line(float x0, float y0, float x1, float y1);
  void absolute_label(const char *lbl, float x, float y);
  void scaled_label(const char *lbl, float x, float y);

  float get_scale_factor()
  { return m_sf; }
};

TAxisWriter::TAxisWriter(int which,
			 float vmin, float vmax,
			 TPlotFrame *pframe,
			 const char *title,
			 bool  islog)
{
  m_which = which;
  m_vmin  = vmin;
  m_vmax  = vmax;
  m_frame = pframe;
  m_title = title;
  m_islog = islog;
  if(X_AXIS == which)
    {
      m_align = (AQTAlignCenter|AQTAlignTop);
      m_angle = 0.0;
      m_wd = pframe->get_width() - pframe->get_xgap();
      m_ht = pframe->get_height();
      m_xorg = pframe->get_xorg() + pframe->get_xgap();
      m_yorg = pframe->get_yorg();
    }
  else
    {
      m_align = (AQTAlignCenter|AQTAlignBottom);
      m_angle = 90.0;
      m_wd = pframe->get_height() - pframe->get_ygap();
      m_ht = pframe->get_width();
      m_xorg = pframe->get_yorg() + pframe->get_ygap();
      m_yorg = pframe->get_xorg();
    }
  m_sf = m_wd/(m_vmax - m_vmin);  
}

void TAxisWriter::cv_scaled_coord(float &x, float &y)
{
  x = m_sf * (x - m_vmin);
  cv_absolute_coord(x,y);
}

void TAxisWriter::cv_absolute_coord(float &x, float &y)
{
  x += m_xorg;
  y += m_yorg;
  if(Y_AXIS == m_which)
    {
      float xsav = x;
      x = y;
      y = xsav;
    }
}

void TAxisWriter::draw_scaled_line(float x0, float y0, float x1, float y1)
{
  cv_scaled_coord(x0,y0);
  cv_scaled_coord(x1,y1);
  drawLineSegment(x0,y0,x1,y1);
}

void TAxisWriter::draw_absolute_line(float x0, float y0, float x1, float y1)
{
  cv_absolute_coord(x0,y0);
  cv_absolute_coord(x1,y1);
  drawLineSegment(x0,y0,x1,y1);
}

void TAxisWriter::scaled_label(const char *lbl, float x, float y)
{
  cv_scaled_coord(x,y);
  aqtAddLabel(lbl,x,y,m_angle,m_align);
}

void TAxisWriter::absolute_label(const char *lbl, float x, float y)
{
  cv_absolute_coord(x,y);
  aqtAddLabel(lbl,x,y,m_angle,m_align);
}

void TAxisWriter::draw()
{
  aqtSetFontname("Times-Roman");
  
  // draw the main axis line
  setPenColor(m_frame->get_fg());
  aqtSetLinewidth(1);
  
  draw_absolute_line(0.0, 0.0, m_wd, 0.0);

  // if title is explicitly set to a zero length string
  // then all we do is draw the basic axis
  // but no labels nor subdivisions
  if(m_title && 0 != strlen(m_title))
    {
      // draw the axis label
      float fontheight = 12;
      aqtSetFontsize(fontheight);
      absolute_label(m_title, m_wd/2, -fontheight-4);

      float range = m_vmax - m_vmin;
      if(m_vmax == m_vmin)
	{
	  if(0 == m_vmax)
	    range = 1.0e-2;
	  else
	    range = m_vmax * 1.0e-2;
	  m_vmax += 0.5*range;
	  m_vmin -= 0.5*range;
	}
      else if(fabsf(range/(m_vmax + m_vmin)) < 0.5e-3)
	{
	  float avg = 0.5*(m_vmax + m_vmin);
	  range = sgn(range) * 1.0e-3 * fabsf(avg);
	  // std::cerr << "*** Range check: minrange = " << range << std::endl;
	  m_vmax += 0.5*range;
	  m_vmin -= 0.5*range;
	}

      TDeltaInfo di(m_vmin, m_vmax);
      double dx = di.m_dx;
      
      if(m_islog)
	dx = ceil(dx);
      dx *= sgn(range);
      
      int    start = int(ceil(m_vmin/dx));
      int    limit = int(floor(m_vmax/dx));
      
      float tickstart, tickstop;
      if(m_frame->has_fullgrid())
	{
	  tickstart = 1.0;
	  tickstop  = m_ht;
	  setPenColor(192,192,192); // gray
	}
      else
	{
	  tickstart = (m_frame->has_ticks_inside() ? 0.0 : -5.0);
	  tickstop  = (m_frame->has_ticks_inside() ? 5.0 : 0.0);
	}
      
      // draw logarithmic subdivisions
      if(m_islog && fabs(dx) == 1.0)
	{
	  int start = int(dx*floor(m_vmin))-1;
	  int limit = int(dx*ceil(m_vmax))+1;
	  for(int ix = start; ix < limit; ++ix)
	    {
	      float fx = ix * dx;
	      if(dx < 0)
		{
		  for(int v = 9; v > 1; --v)
		    {
		      float fxv = fx - 1.0 + log10(v);
		      if(fxv < m_vmax)
			break;
		      if(fxv < m_vmin)
			draw_scaled_line(fxv, tickstart, fxv, tickstop);
		    }}
	      else {
		for(int v = 2; v < 10; ++v)
		  {
		    float fxv = fx + log10(v);
		    if(fxv > m_vmax)
		      break;
		    if(fxv > m_vmin)
		      draw_scaled_line(fxv, tickstart, fxv, tickstop);
		  }}}}
      
      // draw major divisions
      for(int ix = start; ix <= limit; ++ix)
	{
	  float fx = ix * dx;
	  draw_scaled_line(fx, tickstart, fx, tickstop);
	}
      
      // if range crosses origin then draw the zero axis in light blue
      if(sgn(m_vmin)*sgn(m_vmax) < 0)
	{
	  setPenColor(176,176,255); // light blue
	  draw_scaled_line(0.0, 1.0, 0.0, m_ht);
	}
      
      // draw numeric labels
      setPenColor(m_frame->get_fg());
      float lastx = -1e30;
      float extra =
	((m_frame->has_ticks_inside() || m_frame->has_fullgrid()) ? 0 : 5);
      float y0 = -2.0-extra;
      
      for(int ix = 0; ix <= di.m_nu; ++ix)
	{
	  float fx = di.m_x0 + ix * dx;
	  float x  = m_sf * fx;
	  
	  // avoid placing labels too close together
	  if(x - lastx > 2.5*fontheight)
	    {
	      char buf[80];
	      fmtval(m_islog ? pow(10.0,fx) : fx, buf);
	      scaled_label(buf, fx, y0);
	      lastx = x;
	    }
	}
      
      lastx = m_sf * di.m_x0;
      
      for(int ix = 0; ++ix <= di.m_nl;)
	{
	  float fx = di.m_x0 - ix * dx;
	  float x  = m_sf * fx;
	  
	  // avoid placing labels too close together
	  if(lastx - x > 2.5*fontheight)
	    {
	      char buf[80];
	      fmtval(m_islog ? pow(10.0,fx) : fx, buf);
	      scaled_label(buf, fx, y0);
	      lastx = x;
	    }
	}
    }
}

float gapsize(float vmin, float vmax)
{
  return ((sgn(vmin) == sgn(vmax)) ? 20.0 : 0.0);
}

void draw_axes(PlotInfo *p)
{
  float wwd = hWndMain->get_width();
  float wht = hWndMain->get_height();
  
  // establish a new plot record
  PlotRec *pr = new PlotRec(hWndMain, p);
  
  // erase the background to white
  hWndMain->set_bg(p->get_bg()); // RGB(255,255,255);

  float fontheight = 12;
  
  // compute the interior clipping region
  float extra = ((p->has_ticks_inside() || p->has_fullgrid())? 0 : 2);
  float xorg = 5 + 2*fontheight + extra;
  float yorg = xorg;

  float pwd = wwd - 10 - xorg;
  float pht = wht - 20 - yorg;

  pr->set_plotrect(yorg + pht, xorg, yorg, xorg + pwd);  // tlbr
  
  // if range doesn't cross origin,
  // then leave a gap between the axes
  float xgap = gapsize(pr->get_xmin(), pr->get_xmax());
  float ygap = gapsize(pr->get_ymin(), pr->get_ymax());

  pr->set_xgap(xgap);
  pr->set_ygap(ygap);
  
  TPlotFrame frame = {
    xorg, yorg,
    pwd,  pht,
    xgap, ygap,
    p->has_fullgrid(),
    p->has_ticks_inside(),
    p->get_fg() };

  TAxisWriter xaxis(TAxisWriter::X_AXIS,
		    p->get_xmin(), p->get_xmax(),
		    &frame,
		    p->get_xtitle(),
		    p->has_log_xaxis());
  
  TAxisWriter yaxis(TAxisWriter::Y_AXIS,
		    p->get_ymin(), p->get_ymax(),
		    &frame,
		    p->get_ytitle(),
		    p->has_log_yaxis());

  if(0.0 != p->m_aspect)
    {
      if(p->m_aspect <= 1.0)
	{
	  double scale = min(xaxis.m_sf, yaxis.m_sf);
	  xaxis.m_sf = p->m_aspect * scale;
	  yaxis.m_sf = scale;
	}
      else
	{
	  double scale = max(xaxis.m_sf, yaxis.m_sf);
	  xaxis.m_sf = scale;
	  yaxis.m_sf = scale / p->m_aspect;
	}
    }
      
  pr->set_xaxis_sf(xaxis.get_scale_factor());
  pr->set_yaxis_sf(yaxis.get_scale_factor());
  
  setBGColor(p->get_bg());
  // aqtClearPlot();
  // aqtEraseRect(0.0, 0.0, wwd, wht);
  
  xaxis.draw();
  yaxis.draw();

  // draw the plot title
  aqtSetFontsize(14.0);
  aqtAddLabel(p->get_title(), wwd/2, wht-4, 0.0,
	      (AQTAlignTop | AQTAlignCenter));
  
  hWndMain->set_plot(pr);
}  

enum {	PSYM_CIRCLE = 1,
	PSYM_BOX    = 2,
	PSYM_DOT    = 3,
	PSYM_CROSS  = 4,
	PSYM_TRNG   = 5,
	PSYM_HISTO  = 10 };

void plot_symbol_interior(float x, float y, int symbol)
{
  switch(symbol)
    {
    case PSYM_CIRCLE:
      {
	float xs[] = {x,  x+2.6,x+2.6,x,  x-2.6,x-2.6};
	float ys[] = {y+3,y+1.5,y-1.5,y-3,y-1.5,y+1.5};
	aqtAddPolygon(xs,ys,6);
      }
      break;
      
    case PSYM_BOX:
      {
	float xs[] = {x-3,x-3,x+3,x+3};
	float ys[] = {y-3,y+3,y+3,y-3};
	aqtAddPolygon(xs, ys, 4);
      }
      break;
      
    case PSYM_DOT:
    case PSYM_CROSS:
    default:
      break;
      
    case PSYM_TRNG:
      {
	float xs[] = {x, x-3, x+3};
	float ys[] = {y+3, y-3, y-3};
	aqtAddPolygon(xs, ys, 3);
      }
      break;
    }
}

void plot_symbol_outline(float x, float y, int symbol)
{
  switch(symbol)
    {
    case PSYM_CIRCLE:
      {
	float xs[] = {x,  x+2.6,x+2.6,x,  x-2.6,x-2.6, x};
	float ys[] = {y+3,y+1.5,y-1.5,y-3,y-1.5,y+1.5, y+3};
	aqtAddPolyline(xs,ys,7);
      }
      break;
      
    case PSYM_BOX:
      {
	float xs[] = {x-3,x-3,x+3,x+3,x-3};
	float ys[] = {y-3,y+3,y+3,y-3,y-3};
	aqtAddPolyline(xs,ys,5);
      }
      break;
      
    case PSYM_DOT:
      {
	float xs[] = {x-0.5,x-0.5,x+0.5,x+0.5};
	float ys[] = {y-0.5,y+0.5,y+0.5,y-0.5};
	aqtAddPolygon(xs,ys,4);
      }
      break;
      
    case PSYM_CROSS:
    default:
      aqtMoveTo(x-3,y);
      aqtAddLineTo(x+3,y);
      aqtMoveTo(x,y-3);
      aqtAddLineTo(x,y+3);
      break;
      
    case PSYM_TRNG:
      {
	float xs[] = {x,   x-3, x+3, x};
	float ys[] = {y+3, y-3, y-3, y+3};
	aqtAddPolyline(xs,ys,4);
      }
      break;
    }
}

class CAutoReleaseClipper
{
public:
  CAutoReleaseClipper(RECT &clipRect);
  ~CAutoReleaseClipper();
};

CAutoReleaseClipper::CAutoReleaseClipper(RECT &clipRect)
{
  aqtSaveContext();
  aqtSetClipRect(clipRect.get_left(),
		 clipRect.get_bottom(),
		 clipRect.get_width(),
		 clipRect.get_height());
}

CAutoReleaseClipper::~CAutoReleaseClipper()
{
  aqtRestoreContext();
}

class TScanner
{
public:
  float  *m_p;
  int    m_nel;
  float   m_vmin;
  float   m_sf;
  float   m_off;
  bool    m_log;
  int    m_ix;
  
  TScanner(float *p, int nel, float vmin, float sf, float off, bool islog);

  bool  isempty()
  { return (m_ix >= m_nel); }

  void  reset()
  { m_ix = 0; }

  float next();
};

TScanner::TScanner(float *p, int nel, float vmin, float sf,
		   float off, bool islog)
{
  m_p    = p;
  m_nel  = nel;
  m_vmin = vmin;
  m_sf   = sf;
  m_off  = off;
  m_log  = islog;
  m_ix   = 0;
}

float TScanner::next()
{
  if(m_ix >= m_nel)
    throw("TScanner empty");

  float v = (m_p ? m_p[m_ix] : (float)m_ix);
  ++m_ix;
  if(m_log)
    v = log10(v);
  v = m_off + m_sf * (v - m_vmin);
  if(v < -16000.0)
    v = -16000.0;
  else if(v > 16000.0)
    v = 16000.0;
  return v;
}

void wplot_data(PlotRec *pr, DataInfo *dp)
{
  int  limit  = dp->get_npts();
  float xlef   = pr->get_plotrect_left() + pr->get_xgap();
  float ybot   = pr->get_plotrect_bottom() + pr->get_ygap();
  int   symbol = dp->get_plotting_symbol();
  int  color  = dp->get_color();

  CAutoReleaseClipper autoclip(pr->get_plotrect());

  setPenColor(color);
  aqtSetLinewidth((float)dp->get_line_thickness());

  TScanner xs(dp->get_xvalues(),
	      limit,
	      pr->get_xmin(),
	      pr->get_xaxis_sf(),
	      xlef,
	      pr->has_log_xaxis());
  
  TScanner ys(dp->get_yvalues(),
	      limit,
	      pr->get_ymin(),
	      pr->get_yaxis_sf(),
	      ybot,
	      pr->has_log_yaxis());
  
  if(PSYM_HISTO == symbol)
    {
      bool pen = false;
      float xprev, yprev;
      xprev = 0.0;	// compiler dummy
      while(!xs.isempty())
	{
	  float x = xs.next();
	  float y = ys.next();
	  if(finite(x) && finite(y))
	    {
	      if(pen)
		{
		  float xmid = 0.5*(x+xprev);
		  aqtAddLineTo(xmid, yprev);
		  aqtAddLineTo(xmid, y);
		  aqtAddLineTo(x,y);
		}
	      else
		{
		  aqtMoveTo(x,y);
		  pen = true;
		}
	      xprev = x;
	      yprev = y;
	    }}}
  else {
    if(symbol <= 0)
      {
	bool pen = false;
	while(!xs.isempty())
	  {
	    float x = xs.next();
	    float y = ys.next();
	    if(finite(y) && finite(x))
	      {
		if(pen)
		  aqtAddLineTo(x,y);
		else
		  {
		    aqtMoveTo(x,y);
		    pen = true;
		  }
	      }
	    else
	      pen = false;
	  }}
    symbol = abs(symbol);
    switch(symbol)
      {
      case 0:
	break;
	
      case PSYM_CIRCLE:
      case PSYM_BOX:
      case PSYM_TRNG:
	xs.reset();
	ys.reset();
	setPenColor(0x080ffffff);
	while(!xs.isempty())
	  {
	    float x = xs.next();
	    float y = ys.next();
	    if(finite(y) && finite(x))
	      plot_symbol_interior(x,y,symbol);
	  }
	// fall into default
	
      default:
	xs.reset();
	ys.reset();
	setPenColor(color);
	while(!xs.isempty())
	  {
	    float x = xs.next();
	    float y = ys.next();
	    if(finite(y) && finite(x))
	      plot_symbol_outline(x,y,symbol);
	  }
	break;
      }}}

void internal_plot_polys(WindowInfo *wp, int *p)
{
  int npolys = *p++;
  float *xs = 0;
  float *ys = 0;
  int   npts = 0;

  // std::cerr << "PlotPolys: npoly = " << npolys << std::endl;
  for(int ix = 0; ix < npolys; ++ix)
    {
      int nverts = *p++;

      // std::cerr << "poly #" << ix << " nverts = " << nverts << std::endl;
      
      setPenColor(*p++);
      if(npts < nverts+1)
	{
	  if(xs)
	    {
	      delete xs;
	      delete ys;
	    }
	  npts = nverts+1;
	  xs = new float[npts];
	  ys = new float[npts];
	}
#if 0
      if(0 == xs || 0 == ys || npts < nverts + 1)
	{
	  std::cerr << "***!!!*** PolyPlot memory failure!" << std::endl;
	  return;
	}
#endif
      for(int iy = 0; iy < nverts; ++iy)
	{
	  ys[iy] = *p++;
	  xs[iy] = *p++;
	  // std::cerr << "vertex " << iy << " x = " << xs[iy] << " y = " << ys[iy] << std::endl;
	}
      aqtAddPolygon(xs,ys,nverts);
      xs[nverts] = xs[0];
      ys[nverts] = ys[0];
      setPenColor(0);
      aqtAddPolyline(xs,ys,nverts+1);
    }
  if(xs)
    {
      delete xs;
      delete ys;
    }
}

// --------------------------------------------------------
struct Ttext_parms {
  char *m_str;
  int  m_coord_type;
  float m_xorg;
  float m_yorg;
  int  m_color;
  int  m_alpha;
  char *m_fontName;
  float m_fontSize;
  bool  m_clip;
  int  m_anchor;
  float m_angle;

  float get_angle()
  { return m_angle; }

  char* get_label()
  { return m_str; }

  float get_xorg()
  { return m_xorg; }

  float get_yorg()
  { return m_yorg; }

  int get_coord_type()
  { return m_coord_type; }

  int get_anchor()
  { return m_anchor; }

  int get_text_color()
  { return m_color; }

  int get_alpha()
  { return m_alpha; }

  char *get_font_name()
  { return m_fontName; }
  
  float get_font_size()
  { return m_fontSize; }

  bool needs_clipping()
  { return m_clip; }
};

inline float limit_pixrange(float v)
{
  if (v < -16000.0) v = -16000.0;
  else if (v > 16000.0) v = 16000.0;
  return (int)round(v);
}  

inline float cv_plot(float val, float minv, float sf, float off = 0.0)
{
  return limit_pixrange(off + sf * (val - minv));
}

inline int cv_plot_pix(int val, int off)
{
  return (int)limit_pixrange((double)(off + val));
}

inline int cv_plot_frac(double val, double sf, int off)
{
  return (int)limit_pixrange(off + sf * val);
}

void cv_coord_x(int coord_type, float xf, 
	      WindowInfo *wp, PlotRec *p, int &x)
{
  switch(coord_type)
    {
    case DATA_UNITS:
      {
	if(p->m_xlog) xf = log10(xf);
	x = (int)cv_plot(xf,
			  p->get_xmin(), 
			  p->get_xaxis_sf(),
			  p->get_plotrect_left() + p->get_xgap());
      }
      break;
      
    case PIXEL_UNITS:
      {
	x = cv_plot_pix((int)xf, 0);
      }
      break;
      
    case FRACTION_UNITS:
      {
	x = cv_plot_frac(xf, wp->get_width() - 1, 0);
      }
      break;
    }
}

void cv_coord_y(int coord_type, float yf,
	      WindowInfo *wp, PlotRec *p, int &y)
{
  switch(coord_type)
    {
    case DATA_UNITS:
      {
	if(p->m_ylog) yf = log10(yf);
	y = (int)cv_plot(yf,
			  p->get_ymin(), 
			  p->get_yaxis_sf(), 
			  p->get_plotrect_bottom() + p->get_ygap());
      }
      break;
      
    case PIXEL_UNITS:
      {
	y = cv_plot_pix((int)yf, 0);
      }
      break;
      
    case FRACTION_UNITS:
      {
	y = cv_plot_frac(yf, wp->get_height() - 1, 0);
      }
      break;
    }
}

void cv_coord(int coord_type, float xf, float yf,
	      WindowInfo *wp, PlotRec *p, int &x, int &y)
{
  cv_coord_x((coord_type & 3),        xf, wp, p, x);
  cv_coord_y(((coord_type >> 2) & 3), yf, wp, p, y);
}

void draw_text(Ttext_parms *p,
	       WindowInfo *wp, PlotRec *pr, bool get_size = false)
{
  int x0, y0;
  cv_coord(p->get_coord_type(),
	   p->get_xorg(),
	   p->get_yorg(),
	   wp, pr, x0, y0);

  int align = 0;
  switch(p->get_anchor())
    {
    case ANCHOR_N:
      align = AQTAlignCenter + AQTAlignTop;
      break;

    case ANCHOR_NE:
      align = AQTAlignRight + AQTAlignTop;
      break;

    case ANCHOR_E:
      align = AQTAlignRight + AQTAlignMiddle;
      break;

    case ANCHOR_SE:
      align = AQTAlignRight + AQTAlignBaseline;
      break;

    case ANCHOR_S:
      align = AQTAlignCenter + AQTAlignBaseline;
      break;

    case ANCHOR_SW:
      align = AQTAlignLeft + AQTAlignBaseline;
      break;

    case ANCHOR_W:
      align = AQTAlignLeft + AQTAlignMiddle;
      break;

    case ANCHOR_NW:
      align = AQTAlignLeft + AQTAlignTop;
      break;

    case ANCHOR_CTR:
      align = AQTAlignMiddle + AQTAlignCenter;
      break;
    }
  
  setPenColor(p->get_text_color() | (p->get_alpha() << 24));
  aqtSetFontname(p->get_font_name()); // NIL is okay here
  aqtSetFontsize(p->get_font_size());
  if(p->needs_clipping())
    {
      CAutoReleaseClipper autoclip(pr->get_plotrect());
      aqtAddLabel(p->get_label(), x0, y0, p->get_angle(), align);
    }
  else
    aqtAddLabel(p->get_label(), x0, y0, p->get_angle(), align);
}

// ----------------------------------------------------
#if 0
int grOrgX, grOrgY, grExtX, grExtY;

void scale_coord(int coord_type, float xf, float yf,
		 WindowInfo *wp, PlotRec *p, int &dx, int &dy)
{
  switch(coord_type)
    {
    case DATA_UNITS:
      {
	dx = (int)round(p->m_xsf * xf);
	dy = (int)round(p->m_ysf * yf);
      }
      break;
      
    case PIXEL_UNITS:
      {
	dx = (int)xf;
	dy = (int)yf;
      }
      break;
      
    case FRACTION_UNITS:
      {
	int xsf = wp->m_xsize - 1;
	int ysf = wp->m_ysize - 1;
	dx = (int)round(xf * xsf);
	dy = (int)round(yf * ysf);
      }
      break;
    }
}

void adjust_origin(int &x0, int &y0, int dx, int dy, int anchor)
{
  switch(anchor)
    {
    case ANCHOR_CTR:
      x0 -= dx/2;
      y0 -= dy/2;
      break;
      
    case ANCHOR_N:
      x0 -= dx/2;
      break;
      
    case ANCHOR_NE:
      x0 -= dx;
      break;
      
    case ANCHOR_E:
      x0 -= dx;
      y0 -= dy/2;
      break;
      
    case ANCHOR_SE:
      x0 -= dx;
      y0 -= dy;
      break;
      
    case ANCHOR_S:
      x0 -= dx/2;
      y0 -= dy;
      break;
      
    case ANCHOR_SW:
      y0 -= dy;
      break;

    case ANCHOR_W:
      y0 -= dy/2;
      break;
      
    case ANCHOR_NW:
      break;
    }
}

void compute_rays(value velt, int x0, int y0, int dx, int dy,
				  int& x1, int& y1, int& x2, int& y2)
{
  double ang_start = Double_val(Field(velt,2));
  double ang_stop  = Double_val(Field(velt,3));
  if(ang_stop < ang_start)
    {
      double tmp = ang_start;
      ang_start = ang_stop;
      ang_stop = tmp;
    }
  double dtor = acos(-1.0)/180.0;
  double xc = x0 + dx / 2.0;
  double yc = y0 + dy / 2.0;
  x1 = (int)round(xc + 1000 * cos(ang_start * dtor));
  y1 = (int)round(yc - 1000 * sin(ang_start * dtor));
  x2 = (int)round(xc + 1000 * cos(ang_stop * dtor));
  y2 = (int)round(yc - 1000 * sin(ang_stop * dtor));
}

int cv_penpat(int penspec)
{
  switch(penspec)
    {
    default:
    case PENPAT_SOLID:
      return PS_SOLID;
      
    case PENPAT_DASH:
      return PS_DASH;
      
    case PENPAT_DOT:
      return PS_DOT;
      
    case PENPAT_DASHDOT:
      return PS_DASHDOT;
      
    case PENPAT_DASHDOTDOT:
      return PS_DASHDOTDOT;
      
    case PENPAT_NULL:
      return PS_NULL;
      
    case PENPAT_INSIDE_FRAME:
      return PS_INSIDEFRAME;
    }
}
#endif

#if 0
void draw_object(value dp, WindowInfo *wp, PlotRec *pr, bool get_size = false)
{
#if 0
	HDC hDC = wp->m_backing;
	SetGraphicsMode(hDC,GM_ADVANCED);

	int grop     = Int_val(Field(dp,1));
	int penpat   = Int_val(Field(dp,2));
	int penthick = Int_val(Field(dp,3));
	int pencolor = Int_val(Field(dp,4));

	HPEN hPrevPen = (HPEN)SelectObject(
		hDC,
		CreatePen(cv_penpat(penpat), penthick, pencolor));

	bool filled = (Bool_val(Field(dp,5)) != 0);
	HBRUSH hPrevBrush;
	HBRUSH hFillBrush;

	if(filled)
	{
		int fillcolor = Int_val(Field(dp,6));
		hFillBrush = CreateSolidBrush(fillcolor);
		hPrevBrush = (HBRUSH)SelectObject(hDC, hFillBrush);
		SetBkColor(hDC, fillcolor);
		SetBkMode(hDC, OPAQUE);
	}
	else
	{
		hFillBrush = (HBRUSH)GetStockObject(NULL_BRUSH);
		hPrevBrush = (HBRUSH)SelectObject(hDC, hFillBrush);
		SetBkMode(hDC, TRANSPARENT);
	}

	int prevGrop = GetROP2(hDC);
	int grops[16] = 
		{R2_BLACK, R2_COPYPEN, 
		 R2_MASKNOTPEN, R2_MASKPEN, R2_MASKPENNOT,
		 R2_MERGENOTPEN, R2_MERGEPEN, R2_MERGEPENNOT,
		 R2_NOP, R2_NOT,
		 R2_NOTCOPYPEN, R2_NOTMASKPEN, R2_NOTXORPEN,
		 R2_WHITE, R2_XORPEN};
	SetROP2(hDC, grops[grop]);
#endif
	
	value velt  = Field(dp,0);
	value vorg  = Field(velt, 0);

	int anchor = Int_val(Field(dp, 7));

	int x0, y0;
	cv_coord(vorg, wp, pr, x0, y0);
	int boff = wp->m_ysize - 1;

	switch(Tag_val(velt)) // drawable element
	{
	default:
	  NI();
	  break;

	case DR_TEXT:
	  {
	    value vfont = Field(velt, 1);
	    char *str = String_val(Field(velt, 2));
	    int  fontSize = Int_val(Field(vfont, 1));
	    int penColor = Int_val(Field(dp, 4));
	    setPenColor(penColor);
	    aqtSetFontsize(fontSize);
	    aqtAddLabel(str, x0, y0, 0.0, 0);
	  }
	  break;
	  
#if 0
	case DR_POINT:
		{
			if(get_size)
			{
				grOrgX = x0 - penthick/2;
				grOrgY = boff - (y0 - penthick/2 + penthick);
				grExtX = grExtY = penthick;
			}
			else
			{
				if(penthick == 1)
					SetPixel(hDC, x0, y0, pencolor);
				else
				{
					Rectangle(hDC, x0-penthick/2, y0-penthick/2,
						x0 - penthick/2 + penthick,
						y0 - penthick/2 + penthick);
				}
			}
		}
		break;

	case DR_LINE:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
			{
				MoveToEx(hDC, x0, y0, NULL);
				LineTo(hDC, dx + x0, dy + y0);
			}
		}
		break;

	case DR_RECTANGLE:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
				Rectangle(hDC, x0, y0, x0+dx, y0+dy);
		}
		break;

	case DR_ROUNDED_RECTANGLE:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
			{
				int xrad, yrad;
				value vrad = Field(velt, 2);
				scale_coord(vrad, wp, pr, xrad, yrad);
				RoundRect(hDC, x0, y0, x0+dx, y0+dy, xrad, yrad);
			}
		}
		break;

	case DR_ELLIPSE:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
				Ellipse(hDC, x0, y0, x0+dx, y0+dy);
		}
		break;

	case DR_ARC:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
			{
				int x1, y1, x2, y2;
				compute_rays(velt, x0, y0, dx, dy, x1, y1, x2, y2);
				Arc(hDC, x0, y0, x0+dx, y0+dy,
					x1, y1, x2, y2);
			}
		}
		break;

	case DR_CHORD:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
			{
				int x1, y1, x2, y2;
				compute_rays(velt, x0, y0, dx, dy, x1, y1, x2, y2);
				Chord(hDC, x0, y0, x0+dx, y0+dy,
					x1, y1, x2, y2);
			}
		}
		break;

	case DR_PIE:
		{
			int dx, dy;
			value vext = Field(velt, 1);
			scale_coord(vext, wp, pr, dx, dy);
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
			{
				int x1, y1, x2, y2;
				compute_rays(velt, x0, y0, dx, dy, x1, y1, x2, y2);
				Pie(hDC, x0, y0, x0+dx, y0+dy,
					x1, y1, x2, y2);
			}
		}
		break;

	case DR_TEXT:
		{
			LOGFONT lfnt;
			memset(&lfnt,0,sizeof(LOGFONT));
			
			value vfont = Field(velt, 1);
			char *fontName = String_val(Field(vfont,0));
			int   fontSize = Int_val(Field(vfont,1));
			value vstyle = Field(vfont,2);

			lfnt.lfWeight = FW_NORMAL;
			while(vstyle != Val_unit)
			{
				switch(Int_val(Field(vstyle, 0)))
				{
				case FACE_BOLD:
					lfnt.lfWeight = FW_BOLD;
					break;

				case FACE_ITALIC:
					lfnt.lfItalic = true;
					break;

				case FACE_UNDERLINE:
					lfnt.lfUnderline = true;
					break;
				}
				vstyle = Field(vstyle, 1);
			}

			strcpy(lfnt.lfFaceName, fontName);	
			lfnt.lfHeight = -MulDiv(fontSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
			lfnt.lfEscapement = 0;
			lfnt.lfOrientation = 0;
			lfnt.lfCharSet = ANSI_CHARSET;
			lfnt.lfOutPrecision = OUT_OUTLINE_PRECIS;
			lfnt.lfClipPrecision = CLIP_DEFAULT_PRECIS;
			lfnt.lfQuality = PROOF_QUALITY;
			lfnt.lfPitchAndFamily = FF_SWISS | DEFAULT_PITCH;
			HANDLE hfnt = CreateFontIndirect(&lfnt);

			char *str = String_val(Field(velt, 2));
			int slen = strlen(str);
			HANDLE hPrevFont = SelectObject(hDC, hfnt);
			SetTextColor(hDC, pencolor);

			SIZE sz;
			GetTextExtentPoint32(hDC, str, slen, &sz);
			int dx = sz.cx;
			int dy = sz.cy;
			adjust_origin(x0,y0,dx,dy,anchor);
			if(get_size)
			{
				grOrgX = x0;
				grOrgY = boff - (y0 + dy);
				grExtX = dx;
				grExtY = dy;
			}
			else
				TextOut(hDC, x0, y0, str, slen);
			DeleteObject(SelectObject(hDC, hPrevFont));
		}
		break;

#endif
	}

#if 0
	DeleteObject(SelectObject(hDC,hPrevPen));
	SelectObject(hDC,hPrevBrush);
	if(filled)
		DeleteObject(hFillBrush);
	SetROP2(hDC, prevGrop);
#endif
}
#endif


// -----------------------------------------------------
// --------------------------------------------------------------
WindowInfo* _open_window(int winid, int xsize, int ysize,
			 const char *title, int bg)
{
  WindowInfo* p = create_window(winid, xsize, ysize, title, bg);

  // aqtOpenPlot(winid);
  // aqtSetPlotSize((float)xsize, (float)ysize);
  // aqtSetPlotTitle(title);
  // setBGColor(bg);
  aqtRenderPlot();
  return p;
}

struct TWindowSetup
{
  int    m_wid;
  int    m_xpos; // not used by Aquaterm
  int    m_ypos; // not used by Aquaterm
  int    m_xsize;
  int    m_ysize;
  char   *m_title;
  int    m_bgcolor;

  int get_id()
  { return m_wid; }

  int get_width()
  { return m_xsize; }

  int get_height()
  { return m_ysize; }

  char* get_title()
  { return m_title; }

  int get_bg_color()
  { return m_bgcolor; }
};

extern "C"
void lpOpenWindow(TWindowSetup *p)
{
  hWndMain = _open_window(p->get_id(),
			  p->get_width(),
			  p->get_height(),
			  p->get_title(),
			  p->get_bg_color());
}


WindowInfo* _open_default_window(int wid)
{
  char buf[16];

  sprintf(buf, "SciGraph #%d", wid);
  return _open_window(wid, DEFAULT_X_SIZE, DEFAULT_Y_SIZE, buf, DEFAULT_BG);
}

extern "C"
void lpSelectWindow(int wid)
{
    if(!aqtSelectPlot(wid))
      hWndMain = _open_default_window(wid);
    else
      hWndMain = find_window_from_id(wid);
}

extern "C"
void lpShowWindow(int wid)
{
  lpSelectWindow(wid);
}

extern "C"
void lpEraseWindow(int bg)
{
  hWndMain->set_bg(bg);
  setBGColor(bg);
  //aqtClearPlot();
  aqtEraseRect(0.0, 0.0, hWndMain->get_width(), hWndMain->get_height());
  // hWndMain->reset_delay();
  maybe_render();
}

extern "C"
void lpKillWindow(int wid)
{
  NI("lpKillWindow");
}

extern "C"
void lpDelayUpdateWindow(void)
{
  hWndMain->increment_delay();
}

extern "C"
void lpUpdateWindow(void)
{
  if(hWndMain->is_delayed())
    {
      hWndMain->decrement_delay();
      maybe_render();
    }
}

void get_min_max(float *arr, int npts,
		 float& minval, float& maxval,
		 bool islog)
{
  float minv = 0.0;
  float maxv = 0.0;

  while(--npts >= 0)
    {
      float val = *arr++;
      if(finite(val) && !(islog && val <= 0.0))
	{
	  minv = maxv = val;
	  break;
	}
    }
  while(--npts >= 0)
    {
      float val = *arr++;
      if(finite(val) && !(islog && val <= 0.0))
	{
	  if(val > maxv)
	    maxv = val;
	  else if(val < minv)
	    minv = val;
	}
    }
  if(islog)
    {
      if(minv > 0.0)
	minv = log10(minv);
      if(maxv > 0.0)
	maxv = log10(maxv);
    }
  if(minv == maxv)
    {
      if(0.0 == minv)
	{
	  minv = -0.05;
	  maxv = 0.05;
	}
      else
	{
	  minv *= 0.95;
	  maxv *= 1.05;
	}
    }
  minval = minv;
  maxval = maxv;
}

unsigned char Rcolormap[256];
unsigned char Gcolormap[256];
unsigned char Bcolormap[256];

struct TImageParms {
  float    *parr;
  int      xsiz;
  int      ysiz;
  int      xpos;
  int      ypos;
  float     minlimit;
  float     maxlimit;
  float     xmagn;
  float     ymagn;
  int      invert;
  int      flipv;
  int      fliph;
  float     xmin;
  float     xmax;
  float     ymin;
  float     ymax;
  int      xlog;
  int      ylog;
  int      zlog;
  int      xoff;
};

enum { SHOW_IMG, PLOT_IMG };

inline int clip(int x, int xmin, int xmax)
{
  return ((x > xmax) ? xmax : ((x < xmin) ? xmin : x));
}

void show_or_plot_image(TImageParms *p, int oper)
{
  int limit = p->xsiz * p->ysiz;
	
  // Autoscale the data if limits have not been provided
  if(p->minlimit == p->maxlimit)
	  get_min_max(p->parr, limit, p->minlimit, p->maxlimit, p->zlog);
  else if(p->zlog)
  {
	  p->minlimit = log10(p->minlimit);
	  p->maxlimit = log10(p->maxlimit);
  }
  float range = p->maxlimit - p->minlimit;
  float sf = 255.0 / range;
	
  TImage *image = new TImage(p->xsiz,p->ysiz,p->xpos,p->ypos,
			     p->xmagn,p->ymagn,
			     p->minlimit,sf,
			     p->flipv,p->fliph,
			     p->xmin,p->xmax,
			     p->ymin,p->ymax,
			     p->xlog, p->ylog);
  
  // Conversion of image data to bitmap form
  // A Mac OS X bitmap has 3 unsigned char items (R,G,B) per pixel.
  // These are laid out as interleaved image planes in each color.
  unsigned char *pixels = new unsigned char[3*limit];
  float         *q = p->parr;
  unsigned char *pix = pixels;
  for(int ix = 0; ix < limit; ++ix, pix += 3, ++q)
    {
      float val = p->zlog ? log10(*q) : *q;
      if(finite(val))
	{ 
	  val -= p->minlimit;
	  if(val < 0) 
	    val = 0.0;
	  else if(val > range) 
	    val = range;
	  if(p->invert)
	    val = range - val;
	}
      else
	val = 0.0;
      int cx = clip((int)roundf(val*sf),0,255);
      pix[0] = Rcolormap[cx];
      pix[1] = Gcolormap[cx];
      pix[2] = Bcolormap[cx];
    }

  if(SHOW_IMG == oper)
    {
      aqtSetImageTransform(p->fliph ? -p->xmagn : p->xmagn,  0.0,
			   0.0, p->flipv ? -p->ymagn : p->ymagn,
			   p->fliph ? p->xmagn*p->xsiz : 0.0,
			   p->flipv ? p->ymagn*p->ysiz : 0.0);
      aqtAddTransformedImageWithBitmap(pixels,
				       p->xsiz, p->ysiz,
				       0.0, 0.0,
				       p->xmagn*p->xsiz, p->ymagn*p->ysiz);
    }
  else
    {
      PlotRec *pr = hWndMain->m_plot;
      if(pr)
	{
	  float lf = pr->get_plotrect_left() + pr->get_xgap();
	  float bt = pr->get_plotrect_bottom() + pr->get_ygap();
	  float wd = pr->get_plotrect_right() - lf;
	  float ht = pr->get_plotrect_top() - bt;
	  float sfx = wd/p->xsiz;
	  float sfy = -ht/(p->ymax - p->ymin);
	  float yoff = sfy * (p->ysiz - p->ymax);
	  float xoff = sfx * p->xoff;
	  float xwd  = wd - xoff;

	  setBGColor(hWndMain->m_bg);
	  aqtEraseRect(lf,bt,wd,ht);
	  
	  // draw front of image first
	  aqtSetImageTransform(sfx, 0.0,
			       0.0, sfy,
			       ceilf(lf-xoff),  bt+ht-yoff);
	  aqtAddTransformedImageWithBitmap(pixels,
					   p->xsiz, p->ysiz,
					   lf, bt,
					   ceilf(xwd), ht);
	  if(p->xoff)
	    {
	      // draw back of image next
	      aqtSetImageTransform(sfx, 0.0,
				   0.0, sfy,
				   floorf(lf+xwd),  bt+ht-yoff);
	      aqtAddTransformedImageWithBitmap(pixels,
					       p->xsiz, p->ysiz,
					       floorf(lf+xwd), bt,
					       ceilf(xoff), ht);
	    }
	}
    }
  maybe_render();
  delete pixels;
}

extern "C"
void lpShowImage(TImageParms *parms)
{
  show_or_plot_image(parms, SHOW_IMG);
}

extern "C"
void lpPlotImage(TImageParms *parms)
{
  show_or_plot_image(parms, PLOT_IMG);
}

struct TAxesParms {
  float    xmin;
  float    xmax;
  float    ymin;
  float    ymax;
  float    aspect;
  int     xlog;
  int     ylog;
  int     bgcolor;
  int     fgcolor;
  int     grid;
  int     ticks_inside;
  char    *xtitle;
  char    *ytitle;
  char    *title;
};

extern "C"
void lpPlotAxes(TAxesParms *p)
{
  PlotInfo info;
  info.m_xmin = p->xmin;
  info.m_xmax = p->xmax;
  info.m_ymin = p->ymin;
  info.m_ymax = p->ymax;
  info.m_xlog = p->xlog;
  info.m_ylog = p->ylog;
  info.m_xtitle = p->xtitle;
  info.m_ytitle = p->ytitle;
  info.m_title  = p->title;
  info.m_aspect = p->aspect;
  info.m_bg   = p->bgcolor;
  info.m_fg   = p->fgcolor;
  info.m_fullgrid = p->grid;
  info.m_ticks_inside = p->ticks_inside;
  
  draw_axes(&info);
  maybe_render();
}

struct TDataParms
{
  int   npts;
  float *pxarr;
  float *pyarr;
  int   sym;
  int   color;
  int   thick;
  int   clip;
  int   penpat;
  int   alpha;
};

extern "C"
void lpPlotData(TDataParms *p)
{
  DataInfo info;
  info.m_npts  = p->npts;
  info.m_xvals = p->pxarr;
  info.m_yvals = p->pyarr;
  info.m_psym  = p->sym;
  info.m_color = p->color | (p->alpha << 24);
  info.m_thick = p->thick;
  info.m_clip  = p->clip;
  info.m_penpat = p->penpat;

  if(hWndMain->m_plot)
    {
      wplot_data(hWndMain->m_plot, &info);
    }
  else if(hWndMain->m_image)
    {
      PlotRec plt(hWndMain->m_image);
      wplot_data(&plt, &info);
    }
  else
    {
      PlotRec plt(hWndMain);
      wplot_data(&plt, &info);
    }
  maybe_render();
}
	
extern "C"
void lpPlotData1(TDataParms *p)
{
  p->pxarr = 0; // nil out the x data
  lpPlotData(p);
}

struct TScaleParms
{
  int  nel;
  float *parr;
  int  log;
  float minval;
  float maxval;
};

extern "C"
void lpAutoScale(TScaleParms *p)
{
  float xmin, xmax;

  get_min_max(p->parr, p->nel, xmin, xmax, p->log);
  p->minval = xmin;
  p->maxval = xmax;
 }

extern "C"
void lpPlotPolys(int *p)
{
  internal_plot_polys(hWndMain, p);
  maybe_render();
}

struct TMouseInfo
{
  float  mousex;
  float  mousey;
  float  mousez;
};

extern "C"
void lpGetMouse(TMouseInfo *p)
{
  NI("lpGetMouse");
}

struct TWindowInfo
{
  int  xsize;
  int  ysize;
  int  left;
  int  top;
};

extern "C"
void lpGetWindowSize(TWindowInfo *p)
{
  p->xsize = hWndMain->m_xsize;
  p->ysize = hWndMain->m_ysize;
  p->left  = 0;
  p->top   = 0;
}

extern "C"
void lpSetCMap(unsigned char *pred, unsigned char *pgreen, unsigned char *pblue)
{
  // cout << "Colormap size = " << aqtColormapSize() << endl;
  for(int ix = 0; ix < 256; ++ix)
    {
      Rcolormap[ix] = pred[ix];
      Gcolormap[ix] = pgreen[ix];
      Bcolormap[ix] = pblue[ix];
      aqtSetColormapEntry(ix,
			  (float)pred[ix]/255,
			  (float)pgreen[ix]/255,
			  (float)pblue[ix]/255);
    }
}

extern "C"
void lpSetHeatColormap(void)
{
  unsigned char r[256];
  unsigned char g[256];
  unsigned char b[256];

  for(int ix = 0; ix < 256; ++ix)
    {
      int rv = ix*255/176;
      int gv = (ix-120)*255/135;
      int bv = (ix-190)*255/65;
      
      r[ix] = clip(rv,0,255);
      g[ix] = clip(gv,0,255);
      b[ix] = clip(bv,0,255);
    }
  lpSetCMap(r, g, b);
}

extern "C"
void lpSetGrayColormap(void)
{
  unsigned char c[256];

  for(int ix = 0; ix < 256; ++ix)
    c[ix] = ix;
  lpSetCMap(c, c, c);
}

// -----------------------------------------------------
// Primitive graphics operations

extern "C"
void lpDrawText(Ttext_parms *p)
{
  if(hWndMain->m_plot)
    draw_text(p, hWndMain, hWndMain->m_plot);
  else if(hWndMain->m_image)
    {
      PlotRec plt(hWndMain->m_image);
      draw_text(p, hWndMain, &plt);
    }
  else
    {
      PlotRec plt(hWndMain);
      draw_text(p, hWndMain, &plt);
    }
  maybe_render();
}

extern "C"
void lpSavePlot(const char* fileName)
{
  if(hWndMain)
    aqtSavePlot(fileName);
}

#if 0
// code used to test Lisp-C FLI Bridge
// Result shows that we need :LANGUAGE :ANSI-C  (not :C) on the Lisp side.
extern "C"
void lpTestAddLabel(const char *text, float x, float y, float angle,
		    int align)
{
  std::cerr << " text: " << text << std::endl;
  std::cerr << "    x: " << x << std::endl;
  std::cerr << "    y: " << y << std::endl;
  std::cerr << "angle: " << angle << std::endl;
  std::cerr << "align: " << align << std::endl;
  aqtAddLabel(text, x, y, angle, align);
  aqtRenderPlot();
}
#endif

  
  
  
  

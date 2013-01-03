
extern "C" {
#import <Cocoa/Cocoa.h>
};

@interface NSFont (NSFontHiddenMethods)
- (NSGlyph)_defaultGlyphForChar:(unichar)theChar;
@end

typedef struct
 {
   float  red;
   float  green;
   float  blue;
   float  alpha;
} colorStruct;

extern "C"
void pltstuff_add_label(NSView *view, char* text, float x, float y,
			char* font_name, float font_size, colorStruct *color,
			int  justification, float angle,
			int transparent, colorStruct *bgcolor)
{
  int i;
  
  NSFont *aFont = [NSFont fontWithName:[NSString stringWithCString:font_name]
		   size:font_size];
  
  int strLen = strlen(text);
  
  NSAffineTransform *aTransform = [NSAffineTransform transform];
  NSBezierPath *tmpPath = [NSBezierPath bezierPath];
  NSBezierPath *tmpPathR = [NSBezierPath bezierPath];
  NSSize tmpSize;
  NSPoint adjust = NSZeroPoint;
  NSPoint pos = NSZeroPoint;
  
  //
  // appendBezierPathWithGlyph needs a valid context...
  //
  [view lockFocus];
  //
  // Create glyphs and convert to path
  //
  [tmpPath moveToPoint:pos];

  if(!transparent)
    {
      NSBezierPath *tmpPathX = [NSBezierPath bezierPath];
      NSPoint       posX = NSZeroPoint;
      [tmpPathX moveToPoint: posX];
      
      for(i = 0; i < strLen; ++i)
	{
	  NSGlyph theGlyph;
	  NSSize offset;
	  
	  theGlyph = [aFont _defaultGlyphForChar:(text[i])];
	  offset = [aFont advancementForGlyph:theGlyph];
	  [tmpPathX appendBezierPathWithGlyph:theGlyph inFont:aFont];
	  posX.x += offset.width;
	  posX.y += offset.height;
	  [tmpPathX moveToPoint:posX];
	}
      NSRect bounds = [tmpPathX bounds];
      [tmpPathR moveToPoint: NSMakePoint(bounds.origin.x-1, bounds.origin.y-1)];
      [tmpPathR appendBezierPathWithRect:
       NSMakeRect(bounds.origin.x-1, bounds.origin.y-1,
		  bounds.size.width+2, bounds.size.height+2)];
    }
	  
  for(i=0; i < strLen; ++i)
    {
      NSGlyph theGlyph;
      NSSize offset;
      
      theGlyph = [aFont _defaultGlyphForChar:(text[i])];
      offset = [aFont advancementForGlyph:theGlyph];
      [tmpPath appendBezierPathWithGlyph:theGlyph inFont:aFont];
      pos.x += offset.width;
      pos.y += offset.height;
      [tmpPath moveToPoint:pos];
    }
  
  tmpSize = [tmpPath bounds].size;
  //
  // Place the path according to position, angle and align
  //   
  // hAlign:
  adjust.x = -(float)(justification & 0x03)*0.5*tmpSize.width;
  // vAlign:
  switch (justification & 0x1C)
    {
    case 0x00:// AQTAlignMiddle: // align middle wrt *font size*
      adjust.y = -([aFont descender] + [aFont capHeight])*0.5; 
      break;
    case 0x04:// AQTAlignBaseline: // align baseline (do nothing)
      break;
    case 0x08:// AQTAlignBottom: // align bottom wrt *bounding box*
      adjust.y = -[tmpPath bounds].origin.y;
      break;
    case 0x10:// AQTAlignTop: // align top wrt *bounding box*
      adjust.y = -([tmpPath bounds].origin.y + tmpSize.height) ;
      break;
    default:
      // default to align baseline (do nothing) in case of error
      break;
    }
  [aTransform translateXBy:x yBy:y];
  [aTransform rotateByDegrees:angle];
  [aTransform translateXBy:adjust.x yBy:adjust.y];
  [tmpPath transformUsingAffineTransform:aTransform];
  
  if(!transparent)
    {
      [[NSColor colorWithCalibratedRed: (bgcolor->red)
	green: (bgcolor->green)
	blue:  (bgcolor->blue)
	alpha: (bgcolor->alpha)] set];
      [tmpPathR transformUsingAffineTransform:aTransform];
      [tmpPathR fill];
    }
      
  [[NSColor colorWithCalibratedRed: (color->red)
    green:(color->green)
    blue: (color->blue)
    alpha:(color->alpha)] set];
  [tmpPath fill];
  [view unlockFocus];
}

extern "C"
void pltstuff_copy_pdf_plot(NSView *view)
{
   NSPasteboard *pasteboard = [NSPasteboard generalPasteboard];

   [pasteboard declareTypes:[NSArray arrayWithObjects:NSPDFPboardType, NSPostScriptPboardType, nil] owner:nil];
   
   [pasteboard setData:[view dataWithPDFInsideRect:[view bounds]] forType:NSPDFPboardType];
   // [pasteboard setData:[view dataWithEPSInsideRect:[view bounds]] forType:NSPostScriptPboardType];
}


extern "C"
void pltstuff_save_pdf_plot(NSView *view, char *filename)
{
   NSData   *data;
   NSString *fname;

   fname = [[NSString stringWithCString: filename] stringByDeletingPathExtension];

  data = [view dataWithPDFInsideRect: [view bounds]];
  [data writeToFile: [fname stringByAppendingPathExtension:@"pdf"] atomically: NO];
}

#if 0
extern "C"
void pltstuff_set_compositing(NSCompositingOperation mode)
{
  [NSGraphicsContext saveGraphicsState];
  NSGraphicsContext *current = [NSGraphicsContext currentContext];
  [current setCompositingOperation: mode];
}

extern "C"
void pltstuff_restore_context()
{
  [NSGraphicsContext restoreGraphicsState];
}

extern "C"
void pltstuff_draw_xor_line(NSView *view, float x0, float y0,
			    float x1, float y1,
			    colorStruct *color)
{
  [NSGraphicsContext saveGraphicsState];
  NSGraphicsContext *current = [NSGraphicsContext currentContext];
  [current setCompositingOperation: NSCompositeXOR];
  NSBezierPath *scratch = [NSBezierPath bezierPath];
  [view lockFocus];
  [scratch setLineWidth: 1.0];
  [scratch moveToPoint: NSMakePoint(x0,y0)];
  [scratch lineToPoint: NSMakePoint(x1,y1)];
  [[NSColor colorWithCalibratedRed: (color->red)
    green: (color->green)
    blue:  (color->blue)
    alpha: (color->alpha)] set];
  [scratch fill];
  [view unlockFocus];
  [NSGraphicsContext restoreGraphicsState];
}
#endif

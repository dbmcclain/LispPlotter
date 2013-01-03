#include <math.h>
#define NRANSI
#include "nrutil.h"

double pythag(double a, double b)
{
  double absa,absb;
  absa=fabs(a);
  absb=fabs(b);
  if (absa > absb)
    return absa*sqrt(1.0+DSQR(absb/absa));
  else
    return (absb == 0.0 ? 0.0 : absb*sqrt(1.0+DSQR(absa/absb)));
}
#undef NRANSI

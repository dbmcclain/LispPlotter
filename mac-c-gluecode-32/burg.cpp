// burg.cpp -- Burg's Method for AR Analysis
// DM/RAL  05/06
//

#include <math.h>
#include <Accelerate/Accelerate.h>
#include <memory.h>

inline double _DSQR(double x)
{
  return (x*x);
}
  
extern "C"
double lisp_memcof(double *data, long n, double *d, long m)
{
  double p = 0.0;
  double *wk1 = new double[n];
  double *wk2 = new double[n];
  double *wkm = new double[m];

  --data;
  --d;
  --wk1;
  --wk2;
  --wkm;
  
  long i,j,k;
  
  for(j = 1; j <= n; ++j)
    p += _DSQR(data[j]);
  
  double xms = p / n;
  
  wk1[1]   = data[1];
  wk2[n-1] = data[n];
  
  for(j = 2; j <= n-1; ++j)
    {
      wk1[j]   = data[j];
      wk2[j-1] = data[j];
    }
  
  for(k = 1; k <= m; ++k)
    {
      double num = 0.0;
      double den = 0.0;
      
      for(j = 1; j <= (n-k); ++j)
	{
	  num += wk1[j] * wk2[j];
	  den += _DSQR(wk1[j]) + _DSQR(wk2[j]);
	}
      d[k] = 2.0*num/den;
      xms *= (1.0 - _DSQR(d[k]));
      
      for(i = 1; i <= (k-1); ++i)
	d[i] = wkm[i] - d[k] * wkm[k-i];
      
      if(k < m)
	{
	  for(i = 1; i <= k; ++i)
	    wkm[i] = d[i];
	  
	  for(j = 1; j <= (n-k-1); ++j)
	    {
	      wk1[j] -= wkm[k]*wk2[j];
	      wk2[j]  = wk2[j+1] - wkm[k] * wk1[j+1];
	    }
	}
    }
  
  delete (wkm+1);
  delete (wk1+1);
  delete (wk2+1);
  
  return xms;
}

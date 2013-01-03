/* note #undef's at end of file */
#define NRANSI
#include "nrutil.h"
#include <math.h>

void svbksb(double **u, double w[], double **v,
	    int m, int n, double b[], double x[])
{
  int jj,j,i;
  double s,*tmp;
  
  tmp=dvector(1,n);
  for (j=1;j<=n;j++) {
    s=0.0;
    if (0.0 != w[j]) {
      for (i=1;i<=m;i++)
	s += u[i][j]*b[i];
      s /= w[j];
    }
    tmp[j]=s;
  }
  for (j=1;j<=n;j++) {
    s=0.0;
    for (jj=1;jj<=n;jj++)
      s += v[j][jj]*tmp[jj];
    x[j]=s;
  }
  free_dvector(tmp,1,n);
}
#undef NRANSI

extern "C"
long lisp_svbksb(double *u, double w[], double *v,
		 int nrow, int ncol, double b[], double x[])
{
  long err = 0;
  
  double **mu = 0;
  double **mv = 0;
    
  try {
    // This routine produces the SVD decomposition of a [m,n] matrix A
    // so that A = U W (trn V) with U an [m,n] column orthogonal matrix,
    // W an [n] vector of singular values, and V an [n,n] orthogonal matrix.
    // The V matrix is returned, not its transpose...
    // The routine overwrites the `a' matrix with the U matrix.

    mu = convert_dmatrix(u, 1, nrow, 1, ncol);
    mv = convert_dmatrix(v, 1, ncol, 1, ncol);

    svbksb(mu, w-1, mv, nrow, ncol, b-1, x-1);
  }
  catch(...)
    { err = 1; }

  free_convert_dmatrix(mu, 1, nrow, 1, ncol);
  free_convert_dmatrix(mv, 1, ncol, 1, ncol);
  
  return err;
}

extern void svdcmp(double **a, int nrow, int ncol, double w[], double **v);
    
extern "C"
long lisp_svdlms_solve(double *m, double *v, long nrow, long ncol,
		    double tol, double *x)
{
  long err = 0;
  double **ma = 0;
  double **mv = 0;
  double  *w  = 0;

  try {
    ma = convert_dmatrix(m, 1, nrow, 1, ncol);
    mv = dmatrix(1, ncol, 1, ncol);
    w  = dvector(1, ncol);
    svdcmp(ma, nrow, ncol, w, mv);
    for(long ix = ncol; ix >= 1; --ix)
      if(w[ix] < tol)
	w[ix] = 0.0;
    svbksb(ma, w, mv, nrow, ncol, v-1, x-1);
  }
  catch(...)
    { err = 1; }

  free_dvector(w, 1, ncol);
  free_dmatrix(mv, 1, ncol, 1, ncol);
  free_convert_dmatrix(ma, 1, nrow, 1, ncol);

  return err;
}

extern "C"
long lisp_svdpredict(double *v, double *xv, long nel, long ncol,
		     double tol, double *pred, long npred,
		     double *ll, double *hh, double *sd)
{
  long err     = 0;

  long nrowmax = nel - ncol;

  double **ma  = 0;
  double *vw   = 0;
  double **mv  = 0;
  double *vx   = 0;
  
  try
    {
      ma   = dmatrix(1, nrowmax, 1, ncol);
      vw   = dvector(1, ncol);
      mv   = dmatrix(1, ncol, 1, ncol);
      vx   = dvector(1, ncol);
      
      for(long nfut = 1; nfut <= npred; ++nfut)
	{
	  long nrow = nel - ncol - nfut + 1;
	  long row, col;
	  
	  for(row = 1; row <= nrow; ++row)
	    for(col = 1; col <= ncol; ++col)
	      ma[row][col] = xv[row+nfut+col-2];

	  svdcmp(ma, nrow, ncol, vw, mv);
	  for(col = 1; col <= ncol; ++col)
	    if(vw[col] < tol)
	      vw[col] = 0.0;
	  svbksb(ma, vw, mv, nrow, ncol, v-1, vx);

	  double vpred = 0.0;
	  for(col = 1; col <= ncol; ++col)
	    vpred += vx[col] * xv[col-1];
	  pred[nfut-1] = vpred;
	  
	  double errsq = 0.0;
	  for(row = 1; row <= 30; ++row)
	    {
	      double y = 0.0;
	      for(col = 1; col <= ncol; ++col)
		y += vx[col] * xv[row+nfut+col-2];
	      
	      double errl = y - ll[row-1];
	      double errh = y - hh[row-1];
	      double err  =
		(errl < 0.0)
		? errl
		: ((errh > 0.0)
		   ? errh
		   : 0.0);
	      
	      errsq += err * err;
	    }
	  // sd[nfut-1] = sqrt(errsq / (nrow-ncol));
	  sd[nfut-1] = sqrt(errsq / 30.0);
	}
    }
  catch(...)
    { err = 1; }

  free_dmatrix(ma, 1, nrowmax, 1, ncol);
  free_dvector(vw, 1, ncol);
  free_dmatrix(mv, 1, ncol, 1, ncol);
  free_dvector(vx, 1, ncol);

  return err;
}

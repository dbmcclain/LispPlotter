// caml_dgesvd.cpp -- Glue routine to LAPACK dgesvd() function
//
// DM/MCFA  11/06
// ---------------------------------------------------------------------------

#include <stdlib.h>
#include <stdio.h>

#include <math.h>
#include <float.h>
#include <string.h>

#include <Accelerate/Accelerate.h>

double *c_matrix_to_fortran(double *m, long nrow, long ncol)
{
  // allocate matrix and copy transpose
  double *mf = new double[nrow*ncol];

  // effectively, just a transpose from row major order to column major order
  for(long row = 0; row < nrow; ++row)
    {
      double *q = mf + row;
      for(long col = 0; col < ncol; ++col)
	{
	  *q = *m;
	  ++m;
	  q += nrow;
	}
    }
  return mf;
}

void fortran_to_c_matrix(double *mf, double *m, long nrow, long ncol)
{
  // destination matrix already allocated -- just transpose
  
  // effectively just a transpose from column major order to row major order
  for(long row = 0; row < nrow; ++row)
    {
      double *q = mf + row;
      for(long col = 0; col < ncol; ++col)
	{
	  *m = *q;
	  ++m;
	  q += nrow;
	}
    }
}

extern "C"
long lisp_dgesvd(double *a, long nrow, long ncol,
		 double *u, double *w, double *vt)
{
  // This routine produces the SVD decomposition of a [m,n] matrix A
  // so that A = U W (trn V) with:
  //   U a [m,m] column orthogonal matrix,
  //   W a [min(m,n)] vector of singular values corresponding to the
  //     diagonal elements of a [m,n] matrix which is zero except for the
  //     first min(m,n) diagonal elements. Elements of W are returned
  //     in descending order.
  //   V a [n,n] orthogonal matrix.
  //
  // On return we only return the non-singular left and right vectors in
  // U and Vt. Hence, U will receive only the first min(m,n) column vectors,
  // making it a [m,min(m,n)] matrix.
  // Vt will receive only the first min(m,n) row vectors, making it a
  // [min(m,n),n] matrix. The (trn V = Vt) matrix is returned.
  //
  // Result matrices have already been allocated by Caml caller.
  
  long nrcmin = (nrow < ncol) ? nrow : ncol;
  
  double *af  = 0;
  double *vtf = 0;
  double *uf  = 0;

  double *wrk = 0;
  long    nwrk = 0;
  
  long info = 0;
  long err  = 0;
  
  try {
    af = c_matrix_to_fortran(a, nrow, ncol);
    
    vtf = new double[ncol*ncol];
    uf  = new double[nrow*ncol];

    {
      // create workspace for LAPACK routine
      long nmin = (nrow > ncol) ? ncol : nrow;
      long nmax = (nrow > ncol) ? nrow : ncol;
      long na = 3*nmin+nmax;
      long nb = 5*nmin-4;

      nwrk = 2*((na > nb) ? na : nb);
      wrk = new double[nwrk];
    }
    
    /*
      int dgesvd_(
      char *jobu, char *jobvt,
      __CLPK_integer *m, __CLPK_integer *n, 
      __CLPK_doublereal *a, __CLPK_integer *lda,
      __CLPK_doublereal *s,
      __CLPK_doublereal *u, __CLPK_integer *ldu,
      __CLPK_doublereal *vt, __CLPK_integer *ldvt,
      __CLPK_doublereal *work, __CLPK_integer *lwork, 
      __CLPK_integer *info);
    */
    
    dgesvd_("S","S", &nrow, &ncol, af, &nrow, w, uf, &nrow, vtf, &nrcmin,
	   wrk, &nwrk, &info);

    fortran_to_c_matrix(uf, u, nrow, nrcmin);
    fortran_to_c_matrix(vtf, vt, nrcmin, ncol);
  }
  catch(...)
    { err = 1001; }

  delete wrk;
  delete vtf;
  delete uf;
  delete af;

  return (info || err);
}

extern "C"
long lisp_dgesvd_bksb(double *u, double *w, double *vt, double *y,
		      double *x, long nrow, long ncol, double tolerance)
{
  if (tolerance < 0.0)
    tolerance = 0.0;

  double *tmp = 0;
  long    err = 0;
  
  try {
    tmp = new double[ncol];

    long j;
    
    for(j = 0; j < ncol; ++j)
      {
	double sum = 0.0;
	if(w[j] > tolerance)
	  {
	    for(long i = 0; i < nrow; ++i)
	      sum += u[i*ncol+j] * y[i];
	    tmp[j] = sum / w[j];
	  }
	else
	  tmp[j] = 0.0;
      }

    for(j = 0; j < ncol; ++j)
      {
	double sum = 0.0;
	for(long jj = 0; jj < ncol; ++jj)
	  sum += vt[jj*ncol+j] * tmp[jj];
	x[j] = sum;
      }
  }
  catch(...)
    { err = 1001; }
  
  delete tmp;

  return err;
}

extern "C"
long lisp_dgesvd_solve(double *a, double *y, double *x, long nrow, long ncol,
		       double tolerance)
{
  double *u = 0;
  double *w = 0;
  double *vt = 0;

  long err = 0;
  
  try {
    long nrcmin = (nrow < ncol) ? nrow : ncol;
    
    u  = new double[nrow*nrcmin];
    w  = new double[nrcmin];
    vt = new double[nrcmin*ncol];
    
    err = lisp_dgesvd(a, nrow, ncol, u, w, vt);
    if(!err)
      err = lisp_dgesvd_bksb(u, w, vt, y, x, nrow, ncol, tolerance);
  }
  catch(...)
    {}

  delete u;
  delete w;
  delete vt;

  return err;
}

extern "C"
long lisp_dgesvd_predict(double *v, double *xv, long nel, long ncol,
			 double tolerance,
			 double *pred, long npred,
			 double *ll, double *hh,
			 double *sd)
{
  long err = 0;

  long nrowmax = nel - ncol;

  double *ma  = 0;
  double *mu  = 0;
  double *vw  = 0;
  double *mvt = 0;
  double *vx  = 0;
  
  try
    {
      ma   = new double[nrowmax * ncol];
      mu   = new double[nrowmax * ncol];
      vw   = new double[ncol];
      mvt  = new double[ncol*ncol];
      vx   = new double[ncol];
      
      for(long nfut = 1; nfut <= npred; ++nfut)
	{
	  long nrow = nel - ncol - nfut + 1;
	  long row, col;
	  
	  double *p = ma;
	  for(row = 0; row < nrow; ++row)
	    {
	      double *q = xv + row + nfut;
	      for(col = 0; col < ncol; ++col)
		{
		  *p = *q;
		  ++p;
		  ++q;
		}
	    }
	  
	  err = lisp_dgesvd(ma, nrow, ncol, mu, vw, mvt);
	  if(!err)
	    {
	      err = lisp_dgesvd_bksb(mu, vw, mvt, v, vx, nrow, ncol, tolerance);
	      if(!err)
		{
		  double vpred = 0.0;
		  for(col = 0; col < ncol; ++col)
		    vpred += vx[col] * xv[col];
		  pred[nfut-1] = vpred;
		  
		  double err_sum = 0.0;

		  // only 1% contribution from 30 days ago
		  for(row = 30; row >= 0; --row)
		    {
		      double y = 0.0;
		      for(col = 0; col < ncol; ++col)
			y += vx[col] * xv[row+nfut+col];
		      
		      double errl = y - ll[row];
		      double errh = y - hh[row];
		      double err  =
			(errl < 0.0)
			? errl
			: ((errh > 0.0)
			   ? errh
			   : 0.0);
		      
		      // err_sum += err * err;
		      // approx 30 day EMA
		      // actually 1% at 30 days
		      // 0.01 = (1-alpha)^N for N = 30
		      err_sum += 0.1423041 * (err * err - err_sum);
		    }
		  // sd[nfut-1] = sqrt(err_sum / (nrow-ncol));
		  // sd[nfut-1] = sqrt(err_sum / 30.0);
		  sd[nfut-1] = sqrt(err_sum);
		}
	    }
	}
    }
  catch(...)
    { err = 1; }
  
  delete ma;
  delete mu;
  delete vw;
  delete mvt;
  delete vx;
  
  return err;
}

// ---------------------------------------------------------

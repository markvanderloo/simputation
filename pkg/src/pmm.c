
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#ifdef _OPENMP
#include <omp.h>
#endif


SEXP pmm_impute_dbl(SEXP recipients_, SEXP donors_ ){
  double  *recipients = REAL(recipients_)
        , *donors = REAL(donors_);

  R_xlen_t nrecipients = xlength(recipients_)
    , ndonors = xlength(donors_);

  SEXP out_ = PROTECT(duplicate(recipients_));
  double *out = REAL(out_);

  # pragma omp parallel
  {
  double rec, dmin, don;
  # pragma omp for
  for ( R_xlen_t i = 0; i < nrecipients; i++){
    rec = recipients[i];
    dmin = 1.0/0.0;
    don = donors[0];
    for ( R_xlen_t j = 0; j < ndonors; j++ ){
      if (fabs(rec - donors[j]) < dmin ){
        dmin = fabs(rec - donors[j]);
        don = donors[j];
      }
    }
    out[i] = don;
  } 
  }

  UNPROTECT(1);
  return out_;

}







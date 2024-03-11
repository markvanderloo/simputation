
#include <Rdefines.h>

SEXP pmm_impute_dbl(SEXP imputed_recipients_, SEXP imputed_donors_, SEXP donors_ ){
  double *imputed_recipients = REAL(imputed_recipients_);
  double *imputed_donors = REAL(imputed_donors_);
  double *donors = REAL(donors_);

  R_xlen_t nrecipients = xlength(imputed_recipients_);
  R_xlen_t ndonors = xlength(imputed_donors_);

  SEXP out_ = PROTECT(allocVector(REALSXP, nrecipients));
  double *out = REAL(out_);

  # pragma omp parallel
  {
    # pragma omp for
    for (R_xlen_t i = 0; i < nrecipients; i++) {
      double recipient = imputed_recipients[i];
      double donor_val = NA_REAL;
      double donor_dist = R_PosInf;
      for (R_xlen_t j = 0; j < ndonors; j++) {
        double dist = fabs(recipient - imputed_donors[j]);
        if (dist < donor_dist) {
          donor_val = donors[j];
          donor_dist = dist;
        }
      }
      out[i] = donor_val;
    } 
  }

  UNPROTECT(1);
  return out_;
}

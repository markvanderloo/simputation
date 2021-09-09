
#include <math.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <stdlib.h> // for NULL

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP pmm_impute_dbl(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"pmm_impute_dbl", (DL_FUNC) &pmm_impute_dbl, 2},
    {NULL, NULL, 0}
};

void R_init_simputation(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

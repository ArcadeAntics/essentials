#include <R.h>
#include <Rinternals.h>


SEXP testing(SEXP x)
{
    if (TYPEOF(x) != ENVSXP)
        error("invalid 'x', must be an environment");
    SEXP dots = findVarInFrame(x, install("..."));
    if (dots == R_UnboundValue)
        error("invalid 'x', doesn't contain ... object");
    return PREXPR(CAR(dots));


    double k = asReal(x);
    R_xlen_t n = (R_xlen_t) k;
    Rprintf("x                     = %.0f\n", k);
    Rprintf("(R_xlen_t) x          = %.0f\n", n);
    Rprintf("(R_xlen_t) x          = %d\n"  , n);
    Rprintf("(double) (R_xlen_t) x = %.0f\n", (double) n);
    Rprintf("(int) x               = %d\n"  , (int) k);
    return ScalarReal(n);


    SEXP value = findVarInFrame3(x, R_DotsSymbol, TRUE);
    return eval(CAR(value), x);


    /*
    LOGICAL(x)[0] = FALSE;
    return x;

    return eval(lang2(install("invisible"), x), R_BaseEnv);
    return R_NilValue;


    SEXP value = PROTECT(allocVector(LANGSXP, 2));
    SEXP temp = value;
    SETCAR(value, install("Sys.getenv"));
    temp = CDR(value);
    SETCAR(temp, install("testing"));


    UNPROTECT(1);
    return value;
     */


    SEXP fcall = VectorToPairList(x);
    for (SEXP temp = fcall; temp != R_NilValue; temp = CDR(temp)) {
        if (TYPEOF(CAR(temp)) == SYMSXP ||
            TYPEOF(CAR(temp)) == LANGSXP) {
            SETCAR(temp,
  /* enquote */ lang2(
  /* base::quote */ lang3(R_DoubleColonSymbol, R_BaseSymbol, R_QuoteSymbol),
                    CAR(temp)
                )
            );
        }
    }
    return fcall;


    return lang2(
        lang3(R_DoubleColonSymbol, R_BaseSymbol, R_QuoteSymbol),
        x
    );


    return R_NewEnv(R_EmptyEnv, TRUE, 1);


    return lang3(install("="), TAG(CDR(x)), CADR(x));


    return R_NilValue;
}

#include <R.h>
#include <Rinternals.h>


SEXP do_PREXPR(SEXP x, SEXP rho)
{
    if (TYPEOF(x) != SYMSXP)
        error("argument is not a symbol");
    SEXP value = findVarInFrame(rho, x);
    if (value == R_UnboundValue) {
        error("object '%s' not found", CHAR(PRINTNAME(value)));
        return R_NilValue;
    }
    return PREXPR(value);
}


SEXP do_PRENV(SEXP x, SEXP rho)
{
    if (TYPEOF(x) != SYMSXP)
        error("argument is not a symbol");
    SEXP value = findVarInFrame(rho, x);
    if (value == R_UnboundValue) {
        error("object '%s' not found", CHAR(PRINTNAME(value)));
        return R_NilValue;
    }
    return PRENV(value);
}


SEXP do_PRINFO(SEXP x, SEXP rho)
{
    if (TYPEOF(x) != SYMSXP)
        error("argument is not a symbol");
    SEXP value = findVarInFrame(rho, x);
    if (value == R_UnboundValue) {
        error("object '%s' not found", CHAR(PRINTNAME(value)));
        return R_NilValue;
    }
    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, PREXPR(value));
    SET_VECTOR_ELT(ans, 1, PRENV(value));


    SEXP names = allocVector(STRSXP, 2);
    setAttrib(ans, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("expr"));
    SET_STRING_ELT(names, 1, mkChar("env"));


    UNPROTECT(1);
    return ans;
}

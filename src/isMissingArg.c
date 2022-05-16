#include <R.h>
#include <Rinternals.h>


SEXP isMissingArg(SEXP x, SEXP rho)
{
    if (TYPEOF(x) != SYMSXP)
        error("invalid 'x'");
    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    SEXP value;


    for (; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
        value = findVarInFrame3(rho, x, TRUE);
        if (value != R_UnboundValue) {
            if (TYPEOF(value) == PROMSXP) {
                if (TYPEOF(PREXPR(value)) != SYMSXP) {
                    value = PROTECT(eval(value, rho));
                    defineVar(x, value, rho);
                    value = ScalarLogical(value == R_MissingArg);
                    UNPROTECT(1);
                    return value;
                }
                else {
                    x = PROTECT(PREXPR(value));
                    rho = PROTECT(PRENV(value));
                    value = isMissingArg(x, rho);
                    UNPROTECT(2);
                    return value;
                }
            }
            else return ScalarLogical(value == R_MissingArg);
        }
    }
    error("object '%s' not found", CHAR(PRINTNAME(x)));
    return R_NilValue;
}


SEXP do_isMissingArg(SEXP x, SEXP rho)
{
    SEXP value;


    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    if (TYPEOF(x) != SYMSXP) {
        value = PROTECT(eval(x, rho));
        value = ScalarLogical(value == R_MissingArg);
        UNPROTECT(1);
        return value;
    }
    return isMissingArg(x, rho);
}

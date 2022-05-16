#include <R.h>
#include <Rinternals.h>


SEXP do_isMissingArg(SEXP x, SEXP rho)
{
    SEXP value;


    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    if (TYPEOF(x) != SYMSXP) {
        value = PROTECT(eval(x, rho));
        value = PROTECT(ScalarLogical(value == R_MissingArg));
        UNPROTECT(2);
        return value;
    }


    for (; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
        value = findVarInFrame3(rho, x, TRUE);
        if (value != R_UnboundValue) {
            if (TYPEOF(value) == PROMSXP) {
                value = PROTECT(eval(value, rho));
                defineVar(x, value, rho);
                value = ScalarLogical(value == R_MissingArg);
                UNPROTECT(1);
                return value;
            }
            return ScalarLogical(value == R_MissingArg);
        }
    }
    error("object '%s' not found", CHAR(PRINTNAME(x)));
}

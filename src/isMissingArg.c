#include <R.h>
#include <Rinternals.h>
#include "translations.h"


Rboolean isMissingArg(SEXP x, SEXP rho)
{
    if (TYPEOF(x) != SYMSXP)
        error(_("not a symbol"));
    if (TYPEOF(rho) != ENVSXP)
        error(_("not an environment"));
    for (; rho != R_EmptyEnv; rho = ENCLOS(rho)) {
        SEXP value = findVarInFrame(rho, x);
        if (value != R_UnboundValue) {
            if (TYPEOF(value) == PROMSXP) {
                if (PRVALUE(value) == R_UnboundValue) {
                    if (TYPEOF(PREXPR(value)) != SYMSXP)
                        return eval(value, rho) == R_MissingArg;
                    else
                        return isMissingArg(PREXPR(value), PRENV(value));
                }
                else return PRVALUE(value) == R_MissingArg;
            }
            else return value == R_MissingArg;
        }
    }
    error(_("object '%s' not found"), CHAR(PRINTNAME(x)));
    return NA_LOGICAL;
}


SEXP do_ismissingarg(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP xSymbol = NULL;
    if (xSymbol == NULL) {
        xSymbol = install("x");
    }


    SEXP x = findVarInFrame(rho, xSymbol);
    if (x == R_UnboundValue)
        error(_("object '%s' not found"), "x");
    if (x == R_MissingArg)
        error("0 arguments passed to 'isMissingArg' which requires 1");
    if (TYPEOF(x) != PROMSXP)
        error("invalid '%s', is not a promise", "x");


    if (TYPEOF(PREXPR(x)) != SYMSXP)
        return ScalarLogical(eval(x, R_EmptyEnv) == R_MissingArg);
    else
        return ScalarLogical(isMissingArg(PREXPR(x), PRENV(x)));
}

#include <R.h>
#include <Rinternals.h>





#include "defines.h"  // for enquote(x)





R_xlen_t dispatchLength(SEXP x, SEXP rho)
{
    /*
     * if the object has a class attribute, call length(x)
     * we must call it in the user's environment in case they defined any
     * length methods in said environment
     */
    if (isObject(x)) {
        SEXP expr = PROTECT(lang2(
            findVarInFrame(R_BaseEnv, install("length")),
            enquote(x)
        ));
        expr = PROTECT(eval(expr, rho));
        R_xlen_t value = (R_xlen_t)
            (TYPEOF(expr) == REALSXP ? REAL(expr)[0] : asInteger(expr));
        UNPROTECT(2);
        return value;
    }
    else return xlength(x);  /* otherwise, return the internal length */
}


R_xlen_t *dispatchLengths3(SEXP x, SEXP rho, R_xlen_t length_x)
{
    /* find lengths(x), and convert to a R_xlen_t array */


    SEXP expr = PROTECT(lang2(
        findVarInFrame(R_BaseEnv, install("lengths")),
        enquote(x)
    ));
    SEXP lengths_x = PROTECT(eval(expr, rho));


    if (length_x != xlength(lengths_x))
        error(
            "'length(X)' (%lld) and 'length(lengths(X))' (%lld) are not equal",
            (long long int) length_x,
            (long long int) xlength(lengths_x)
        );


    R_xlen_t *value = (R_xlen_t *)
        R_alloc(length_x, sizeof(R_xlen_t));
    switch (TYPEOF(lengths_x)) {
    case REALSXP:
        for (R_xlen_t i = 0; i < length_x; i++)
            value[i] = (R_xlen_t) (REAL(lengths_x)[i]);
        break;
    case INTSXP:
        for (R_xlen_t i = 0; i < length_x; i++)
            value[i] = (R_xlen_t) (INTEGER(lengths_x)[i]);
        break;
    default:
        error("invalid 'lengths(x)' of type '%s'", type2char(TYPEOF(lengths_x)));
    }
    UNPROTECT(2);
    return value;
}


R_xlen_t *dispatchLengths(SEXP x, SEXP rho)
{
    return dispatchLengths3(x, rho, dispatchLength(x, rho));
}


SEXP dispatchNames(SEXP x, SEXP rho)
{
    /*
     * if the object has a class attribute, call names(x)
     * we must call it in the user's environment in case they defined any
     * names methods in said environment
     */
    if (isObject(x)) {
        SEXP expr = PROTECT(lang2(
            findVarInFrame(R_BaseEnv, R_NamesSymbol),
            enquote(x)
        ));
        SEXP value = eval(expr, rho);
        UNPROTECT(1);
        return value;
    }
    else return getAttrib(x, R_NamesSymbol);  /* otherwise, return the internal names */
}

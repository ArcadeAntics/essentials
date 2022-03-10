#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

/*
SEXP do_RK4(SEXP independent, SEXP initialConditions, SEXP fun, SEXP xname)
{
    switch (TYPEOF(independent)) {
    case INTSXP:
    case LGLSXP:
    case NILSXP:
    case REALSXP:
        break;
    default:
        error("'%s' must be numeric", "independent");
    }


    switch (TYPEOF(initialConditions)) {
    case INTSXP:
    case LGLSXP:
    case NILSXP:
    case REALSXP:
        break;
    default:
        error("'%s' must be numeric", "initialConditions");
    }


    R_xlen_t tmp1 = xlength(independent),
             tmp2 = xlength(initialConditions);
    if (tmp1 > INT_MAX)
        error("'length(independent)' (%.0f) cannot be greater than '.Machine$integer.max' (%d)",
            (double) tmp1, INT_MAX);
    if (tmp2 > INT_MAX)
        error("'length(initialConditions)' (%.0f) cannot be greater than '.Machine$integer.max' (%d)",
            (double) tmp2, INT_MAX);
    int length_independent       = (int) tmp1,
        length_initialConditions = (int) tmp2;


    independent       = PROTECT(coerceVector(independent      , REALSXP));
    initialConditions = PROTECT(coerceVector(initialConditions, REALSXP));


    SEXP fun_call = PROTECT(lang4(
        install("fun"),
        R_NilValue,
        R_NilValue,
        R_DotsSymbol
    ));


    R_xlen_t len  = xlength(independent),
             I    = len - 1;



    for (R_xlen_t i = 0; i < I; i++) {
        h =
    }




}
 */

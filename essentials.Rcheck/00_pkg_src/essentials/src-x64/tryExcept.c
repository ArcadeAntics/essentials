#include <R.h>
#include <Rinternals.h>


SEXP do_tryExcept_onexit_setup(SEXP x, SEXP rho)
{
    R_xlen_t i, len = xlength(x);
    SEXP expr;


    if (TYPEOF(x) != VECSXP)
        error("invalid '%s', must be a list", "x");
    if (!isEnvironment(rho))
        error("invalid '%s', must be an environment", "rho");


    PROTECT(expr = lang3(
        install("on.exit"),
        R_NilValue,
        ScalarLogical(TRUE)
    ));


    for (i = 0; i < len; i++) {
        SETCADR(expr, VECTOR_ELT(x, i));
        eval(expr, rho);
    }


    UNPROTECT(1);
    return(R_NilValue);
}

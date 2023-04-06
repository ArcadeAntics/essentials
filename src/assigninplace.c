#include <R.h>
#include <Rinternals.h>


#include "defines.h"  // includes UNIMPLEMENTED_TYPE()


extern Rbyte asRaw(SEXP x);


SEXP do_assigninplace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP value = CAR(args); args = CDR(args);
    switch (TYPEOF(x)) {
    case LGLSXP:
        LOGICAL(x)[0] = asLogical(value);
        break;
    case INTSXP:
        INTEGER(x)[0] = asInteger(value);
        break;
    case REALSXP:
        REAL(x)[0] = asReal(value);
        break;
    case CPLXSXP:
        COMPLEX(x)[0] = asComplex(value);
        break;
    case STRSXP:
        SET_STRING_ELT(x, 0, asChar(value));
        break;
    case RAWSXP:
        RAW(x)[0] = asRaw(value);
        break;
    default:
        UNIMPLEMENTED_TYPE("C_assigninplace", x);
        break;
    }
    return x;
}

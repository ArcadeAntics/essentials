#include <R.h>
#include <Rinternals.h>
#include "defines.h"





#define remove_attrib(X)                                       \
    if (ATTRIB(X) != R_NilValue) {                             \
        SET_ATTRIB(X, R_NilValue);                             \
        if (OBJECT(X)) SET_OBJECT(X, 0);                       \
        if (IS_S4_OBJECT(X)) UNSET_S4_OBJECT(X);               \
    }                                                          \


// test if a complex number is purely real
#define purelyReal(X) (cISNAN(X) || X.i == 0.0)


#define asInteger2(X) (TYPEOF(X) != RAWSXP ? asInteger(X) :    \
    (xlength(X) >= 1 ? (int)    RAW_ELT(X, 0) : NA_INTEGER))


#define asReal2(X)    (TYPEOF(X) != RAWSXP ? asReal(X)    :    \
    (xlength(X) >= 1 ? (double) RAW_ELT(X, 0) : NA_REAL   ))


Rcomplex asComplex2(SEXP x)
{
    if (TYPEOF(x) != RAWSXP) return asComplex(x);
    Rcomplex value;
    if (xlength(x) >= 1) {
        value.r = (double) RAW_ELT(x, 0);
        value.i = 0.0;
    }
    else {
        value.r = NA_REAL;
        value.i = NA_REAL;
    }
    return value;
}


SEXP asChar2(SEXP x)
{
    if (TYPEOF(x) != RAWSXP) return asChar(x);
    if (xlength(x) >= 1) {
        char buf[3];
        sprintf(buf, "%02x", RAW_ELT(x, 0));
        return mkChar(buf);
    }
    else return NA_STRING;
}


Rbyte asRaw(SEXP x)
{
    if (TYPEOF(x) == RAWSXP) {
        if (xlength(x) >= 1)
            return RAW_ELT(x, 0);
        else return (Rbyte) 0;
    }
    int value = asInteger(x);
    if (value == NA_INTEGER || value < 0 || value > 255) {
        value = 0;
        warning("out-of-range values treated as 0 in coercion to raw");
    }
    return (Rbyte) value;
}





// as.scalar.logical(x) :
SEXP do_asscalarlogical(SEXP x)
{
    return ScalarLogical(asLogical(x));
}


// as.scalar.integer(x) :
SEXP do_asscalarinteger(SEXP x)
{
    return ScalarInteger(asInteger2(x));
}


// as.scalar.real(x)
// as.scalar.double(x)
// as.scalar.numeric(x) :
SEXP do_asscalarreal(SEXP x)
{
    return ScalarReal(asReal2(x));
}


// as.scalar.complex(x) :
SEXP do_asscalarcomplex(SEXP x)
{
    return ScalarComplex(asComplex2(x));
}


// as.scalar.string(x)
// as.scalar.character(x) :
SEXP do_asscalarstring(SEXP x)
{
    return ScalarString(asChar2(x));
}


// as.scalar.raw(x) :
SEXP do_asscalarraw(SEXP x)
{
    return ScalarRaw(asRaw(x));
}


// as.scalar.number(x) :
SEXP do_asscalarnumber(SEXP x, SEXP strict)
{
    switch(TYPEOF(x)) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case RAWSXP:
        return do_asscalarreal(x);
    default:
        if (asLogical(strict)) {
            Rcomplex value = asComplex(x);
            if (cISNAN(value))
                return ScalarReal(NA_REAL);
            else if (value.i == 0)
                return ScalarReal(value.r);
            else return ScalarComplex(value);
        }
        return do_asscalarcomplex(x);
    }
}


// as.scalar(x, mode) :
SEXP do_asscalar(SEXP x, SEXP mode)
{
    if (!isString(mode) || xlength(mode) != 1) {
        error("invalid 'mode' argument");
        return R_NilValue;
    }
    int type = str2type(CHAR(STRING_ELT(mode, 0)));
    if (type == ANYSXP) type = TYPEOF(x);
    switch(type) {
    case LGLSXP: return do_asscalarlogical(x);
    case INTSXP: return do_asscalarinteger(x);
    case REALSXP: return do_asscalarreal(x);
    case CPLXSXP: return do_asscalarcomplex(x);
    case RAWSXP: return do_asscalarraw(x);
    default: return do_asscalarstring(x);
    }
}


int isScalar(SEXP x)
{
    if (xlength(x) != 1) return 0;
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
        return 1;
    default:
        return 0;
    }
}


// is.scalar(x, mode) :
SEXP do_isscalar(SEXP x, SEXP mode)
{
    SEXP a, value;
    const char *type;


    if (!isString(mode) || xlength(mode) != 1) {
        error("invalid 'mode' argument");
        return R_NilValue;
    }
    type = CHAR(STRING_ELT(mode, 0));


    PROTECT(value = allocVector(LGLSXP, 1));


    if (!isScalar(x)) {
        LOGICAL0(value)[0] = 0;
    }
    else if (streql(type, "any")) {
        LOGICAL0(value)[0] = 1;
    }
    else if (streql(type, "numeric")) {
        switch(TYPEOF(x)) {
        case INTSXP:
        case REALSXP:
            LOGICAL0(value)[0] = 1;
            break;
        default:
            LOGICAL0(value)[0] = 0;
            break;
        }
    }
    else if (streql(type, type2char(TYPEOF(x)))) {
        LOGICAL0(value)[0] = 1;
    }
    else LOGICAL0(value)[0] = 0;


    if (LOGICAL0(value)[0] && ATTRIB(x) != R_NilValue) {
        for (a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
            if (TAG(a) != R_NamesSymbol) {
                LOGICAL0(value)[0] = 0;
                break;
            }
        }
    }
    UNPROTECT(1);
    return value;
}





SEXP do_as_numbers(SEXP x, SEXP strict)
{
    SEXP value;

    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
        value = PROTECT(coerceVector(x, REALSXP));
        remove_attrib(value);
        UNPROTECT(1);
        return value;
    case REALSXP:
    {
        value = PROTECT(Rf_lazy_duplicate(x));
        remove_attrib(value);
        UNPROTECT(1);
        return value;
    }
    case CPLXSXP:
        value = PROTECT(Rf_lazy_duplicate(x));
        break;
    default:
        value = PROTECT(coerceVector(x, CPLXSXP));
        break;
    }

    remove_attrib(value);

    // possible coerce from 'CPLXSXP' to 'REALSXP'
    if (asLogical(strict)) {
        int coerce = 1;
        Rcomplex *cvalue = COMPLEX(value);
        R_xlen_t i, len = xlength(value);
        for (i = 0; i < len; i++) {
            if (!purelyReal(cvalue[i])) {
                coerce = 0;
                break;
            }
        }
        if (coerce) {
            UNPROTECT(1);
            return coerceVector(value, REALSXP);
        }
    }

    UNPROTECT(1);
    return value;
}

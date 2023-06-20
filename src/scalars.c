#include <R.h>
#include <Rinternals.h>
#include "defines.h"
#include "translations.h"





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
        snprintf(buf, 3, "%02x", RAW_ELT(x, 0));
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





SEXP do_asscalarlogical(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarLogical(asLogical(CADR(args)));
}


SEXP do_asscalarinteger(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarInteger(asInteger2(CADR(args)));
}


SEXP do_asscalardouble(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarReal(asReal2(CADR(args)));
}


SEXP do_asscalarcomplex(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarComplex(asComplex2(CADR(args)));
}


SEXP do_asscalarcharacter(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarString(asChar2(CADR(args)));
}


SEXP do_asscalarraw(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarRaw(asRaw(CADR(args)));
}


SEXP do_asscalarnumber(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    switch(TYPEOF(CAR(args))) {
    case NILSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case RAWSXP:
        return ScalarReal(asReal2(CAR(args)));
    default:
        if (asLogical(CADR(args))) {
            Rcomplex value = asComplex(CAR(args));
            if (cISNAN(value))
                return ScalarReal(NA_REAL);
            else if (value.i == 0)
                return ScalarReal(value.r);
            else return ScalarComplex(value);
        }
        return ScalarComplex(asComplex2(CAR(args)));
    }
}


SEXP do_asscalar(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP mode = CAR(args); args = CDR(args);
    if (TYPEOF(mode) != STRSXP || XLENGTH(mode) != 1)
        error(_("invalid '%s' argument"), "mode");
    SEXPTYPE type = str2type(CHAR(STRING_ELT(mode, 0)));
    if (type == ANYSXP) type = TYPEOF(x);
    switch(type) {
    case LGLSXP: return ScalarLogical(asLogical(x));
    case INTSXP: return ScalarInteger(asInteger2(x));
    case REALSXP: return ScalarReal(asReal2(x));
    case CPLXSXP: return ScalarComplex(asComplex2(x));
    case RAWSXP: return ScalarRaw(asRaw(x));
    default: return ScalarString(asChar2(x));
    }
}


Rboolean isscalar(SEXP x, const char *mode)
{
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
        break;
    default:
        return FALSE;
    }


    if (xlength(x) != 1) return FALSE;


    if (streql(mode, "any"));
    else if (streql(mode, "numeric")) {
        switch(TYPEOF(x)) {
        case INTSXP:
        case REALSXP:
            break;
        default:
            return FALSE;
        }
    }
    else if (streql(mode, type2char(TYPEOF(x))));
    else return FALSE;


    if (ATTRIB(x) != R_NilValue) {
        for (SEXP a = ATTRIB(x); a != R_NilValue; a = CDR(a)) {
            if (TAG(a) != R_NamesSymbol) {
                return FALSE;
            }
        }
    }


    return TRUE;
}


SEXP do_isscalar(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP mode = CAR(args); args = CDR(args);


    if (TYPEOF(mode) != STRSXP || XLENGTH(mode) != 1)
        error(_("invalid '%s' argument"), "mode");


    return ScalarLogical(isscalar(x, CHAR(STRING_ELT(mode, 0))));
}





SEXP do_asnumbers(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP strict = CAR(args); args = CDR(args);


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
            value = coerceVector(value, REALSXP);
            UNPROTECT(1);
            return value;
        }
    }

    UNPROTECT(1);
    return value;
}

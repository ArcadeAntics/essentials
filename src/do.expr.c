#include <R.h>
#include <Rinternals.h>





#define my_return(X) {                                                \
    value = (X);                                                      \
    UNPROTECT(np);                                                    \
    return value;                                                     \
}





#define Rprint(X) (eval(lang2(install("print"), lang2(install("quote"), (X))), R_BaseEnv))





/*
SEXP quoteLang(SEXP X, SEXP quote)
{
    if (X == R_MissingArg)
        return R_MissingArg;
    SEXP value = PROTECT(X);
    switch (TYPEOF(value)) {
    case SYMSXP:
        value = lang2(quote, value);
        break;
    case LANGSXP:
        if (!inherits(value, "formula"))
            value = lang2(quote, value);
        break;
    }
    UNPROTECT(1);
    return value;
}
 */





#define quoteLang(X) (                                                 \
    (X) == R_MissingArg ? R_MissingArg : (                             \
        TYPEOF((X)) == SYMSXP || (TYPEOF((X)) == LANGSXP && !inherits((X), "formula")) ? lang2(quote, (X)) : (X)\
    )                                                                  \
)





#define my_SET_TAG                                                     \
    {                                                                  \
        if (CHAR(STRING_ELT(names, i))[0] != '\0')                     \
            SET_TAG(expr, installTrChar(STRING_ELT(names, i)));        \
    }





#define _unpack_iter_other                                             \
    {                                                                  \
        tmp2 = PROTECT(allocVector(INTSXP, 1));                        \
        iarg = INTEGER(tmp2);                                          \
        tmp3 = PROTECT(lang3(                                          \
            R_Bracket2Symbol,                                          \
            quoteLang(arg),                                            \
            tmp2                                                       \
        ));                                                            \
        for (int i = (int) len - 1; i >= 0; i--) {                     \
            iarg[0] = i + 1;                                           \
            tmp = PROTECT(eval(tmp3, R_BaseEnv));                      \
            tmp = PROTECT(quoteLang(tmp));                             \
            REPROTECT(expr = LCONS(tmp, expr), indx);                  \
            UNPROTECT(2);                                              \
        }                                                              \
        UNPROTECT(2);                                                  \
    }





#define _unpack_dict_other                                             \
    {                                                                  \
        tmp2 = PROTECT(allocVector(INTSXP, 1));                        \
        iarg = INTEGER(tmp2);                                          \
        tmp3 = PROTECT(lang3(                                          \
            R_Bracket2Symbol,                                          \
            quoteLang(arg),                                            \
            tmp2                                                       \
        ));                                                            \
        for (int i = (int) len - 1; i >= 0; i--) {                     \
            iarg[0] = i + 1;                                           \
            tmp = PROTECT(eval(tmp3, R_BaseEnv));                      \
            tmp = PROTECT(quoteLang(tmp));                             \
            REPROTECT(expr = LCONS(tmp, expr), indx);                  \
            UNPROTECT(2);                                              \
            my_SET_TAG                                                 \
        }                                                              \
        UNPROTECT(2);                                                  \
    }





#define _unpack_iter                                                   \
    if (isObject(arg)) {                                               \
        _unpack_iter_other                                             \
    }                                                                  \
    else {                                                             \
        switch (TYPEOF(arg)) {                                         \
        case NILSXP:                                                   \
            break;                                                     \
        case LGLSXP:                                                   \
            iarg = INTEGER(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarLogical(iarg[i]), expr), indx);\
            }                                                          \
            break;                                                     \
        case INTSXP:                                                   \
            iarg = INTEGER(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarInteger(iarg[i]), expr), indx);\
            }                                                          \
            break;                                                     \
        case REALSXP:                                                  \
            rarg = REAL(arg);                                          \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarReal(rarg[i]), expr), indx);\
            }                                                          \
            break;                                                     \
        case CPLXSXP:                                                  \
            carg = COMPLEX(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarComplex(carg[i]), expr), indx);\
            }                                                          \
            break;                                                     \
        case STRSXP:                                                   \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarString(STRING_ELT(arg, i)), expr), indx);\
            }                                                          \
            break;                                                     \
        case RAWSXP:                                                   \
            barg = RAW(arg);                                           \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarRaw(barg[i]), expr), indx);\
            }                                                          \
            break;                                                     \
        case VECSXP:                                                   \
        case EXPRSXP:                                                  \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                tmp = PROTECT(VECTOR_ELT(arg, i));                     \
                tmp = PROTECT(quoteLang(tmp));                         \
                REPROTECT(expr = LCONS(tmp, expr), indx);              \
                UNPROTECT(2);                                          \
            }                                                          \
            break;                                                     \
        case LISTSXP:                                                  \
        case LANGSXP:                                                  \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                tmp = PROTECT(CAR(nthcdr(arg, i)));                    \
                tmp = PROTECT(quoteLang(tmp));                         \
                REPROTECT(expr = LCONS(tmp, expr), indx);              \
                UNPROTECT(2);                                          \
            }                                                          \
            break;                                                     \
        default:                                                       \
            _unpack_iter_other                                         \
            break;                                                     \
        }                                                              \
    }





#define _unpack_dict                                                   \
    if (isObject(arg)) {                                               \
        _unpack_dict_other                                             \
    }                                                                  \
    else {                                                             \
        switch (TYPEOF(arg)) {                                         \
        case NILSXP:                                                   \
            break;                                                     \
        case LGLSXP:                                                   \
            iarg = INTEGER(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarLogical(iarg[i]), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case INTSXP:                                                   \
            iarg = INTEGER(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarInteger(iarg[i]), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case REALSXP:                                                  \
            rarg = REAL(arg);                                          \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarReal(rarg[i]), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case CPLXSXP:                                                  \
            carg = COMPLEX(arg);                                       \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarComplex(carg[i]), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case STRSXP:                                                   \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarString(STRING_ELT(arg, i)), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case RAWSXP:                                                   \
            barg = RAW(arg);                                           \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                REPROTECT(expr = LCONS(ScalarRaw(barg[i]), expr), indx);\
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case VECSXP:                                                   \
        case EXPRSXP:                                                  \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                tmp = PROTECT(VECTOR_ELT(arg, i));                     \
                tmp = PROTECT(quoteLang(tmp));                         \
                REPROTECT(expr = LCONS(tmp, expr), indx);              \
                UNPROTECT(2);                                          \
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        case LISTSXP:                                                  \
        case LANGSXP:                                                  \
            for (int i = (int) len - 1; i >= 0; i--) {                 \
                tmp = PROTECT(CAR(nthcdr(arg, i)));                    \
                tmp = PROTECT(quoteLang(tmp));                         \
                REPROTECT(expr = LCONS(tmp, expr), indx);              \
                UNPROTECT(2);                                          \
                my_SET_TAG                                             \
            }                                                          \
            break;                                                     \
        default:                                                       \
            _unpack_dict_other                                         \
            break;                                                     \
        }                                                              \
    }





#define unpack_iter                                                    \
    {                                                                  \
        if (isObject(arg)) {                                           \
            tmp = PROTECT(lang2(install("length"), lang2(install("quote"), arg)));\
            tmp = PROTECT(eval(tmp, R_BaseEnv));                       \
            len = (R_xlen_t)                                           \
                (TYPEOF(tmp) == REALSXP ? REAL(tmp)[0] : asInteger(tmp));\
            UNPROTECT(2);                                              \
        }                                                              \
        else len = xlength(arg);                                       \
        if (len > 0) {                                                 \
            if (len >= INT_MAX || len + xlength(expr) >= INT_MAX)      \
                error("too many arguments");                           \
            _unpack_iter                                               \
        }                                                              \
    }





#define unpack_dict                                                    \
    {                                                                  \
        if (isObject(arg)) {                                           \
            tmp = PROTECT(lang2(install("length"), lang2(install("quote"), arg)));\
            tmp = PROTECT(eval(tmp, R_BaseEnv));                       \
            len = (R_xlen_t)                                           \
                (TYPEOF(tmp) == REALSXP ? REAL(tmp)[0] : asInteger(tmp));\
            UNPROTECT(2);                                              \
            if (len > 0) {                                             \
                tmp = PROTECT(lang2(install("names"), lang2(install("quote"), arg)));\
                names = PROTECT(eval(tmp, R_BaseEnv));                 \
            }                                                          \
        }                                                              \
        else {                                                         \
            len = xlength(arg);                                        \
            if (len > 0) {                                             \
                names = PROTECT(getAttrib(arg, R_NamesSymbol));        \
            }                                                          \
        }                                                              \
        if (len > 0) {                                                 \
            if (len >= INT_MAX || len + xlength(expr) >= INT_MAX)      \
                error("too many arguments");                           \
            if (names == R_NilValue) {                                 \
                _unpack_iter                                           \
            }                                                          \
            else {                                                     \
                if (TYPEOF(names) != STRSXP || length(names) != len)   \
                    error("names(arg) must be a character vector of the same length as arg");\
                _unpack_dict                                           \
            }                                                          \
            UNPROTECT(isObject(arg) ? 2 : 1);                          \
        }                                                              \
    }




SEXP do_do_expr(SEXP sexpr, SEXP rho, SEXP visible)
{
    if (TYPEOF(sexpr) != LANGSXP)
        error("invalid 'expr', must be a call");
    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho', must be an environment");
    if (TYPEOF(visible) != LGLSXP || LENGTH(visible) != 1)
        error("invalid '%s'", "visible");


    int np = 0;
    SEXP value;


    SEXP tmp, tmp2, tmp3;
    SEXP sargs, tag, sarg, arg, names;
    sargs = CDR(sexpr);


    PROTECT_INDEX indx;
    SEXP expr = R_NilValue;
    PROTECT_WITH_INDEX(expr, &indx); np++;


    int *iarg;
    double *rarg;
    Rcomplex *carg;
    Rbyte *barg;


    R_xlen_t len;
    SEXP pairlist    = PROTECT(eval(install("pairlist"   ), R_BaseEnv)); np++;
    SEXP quote       = PROTECT(eval(install("quote"      ), R_BaseEnv)); np++;
    SEXP withVisible = PROTECT(eval(install("withVisible"), R_BaseEnv)); np++;


    for (int n = length(sargs) - 1; n >= 0; n--) {
        tmp = nthcdr(sargs, n);
        tag = TAG(tmp);
        sarg = CAR(tmp);
        if (sarg == R_MissingArg) {
            REPROTECT(expr = LCONS(R_MissingArg, expr), indx);
            if (!isNull(tag))
                SET_TAG(expr, tag);
        }
        else if (TYPEOF(sarg) == LANGSXP && CAR(sarg) == install("*") && length(sarg) == 2) {
            if (!isNull(tag))
                error("do not name arguments which are being unpacked");
            sarg = CADR(sarg);
            if (TYPEOF(sarg) == SYMSXP && sarg == R_DotsSymbol) {
                sarg = PROTECT(lang2(pairlist, sarg));
                arg = PROTECT(eval(sarg, rho));
                unpack_iter
                UNPROTECT(2);
            }
            else {
                arg = PROTECT(eval(sarg, rho));
                unpack_iter
                UNPROTECT(1);
            }
        }
        else if (TYPEOF(sarg) == LANGSXP && CAR(sarg) == install("**") && length(sarg) == 2) {
            if (!isNull(tag))
                error("do not name arguments which are being unpacked");
            sarg = CADR(sarg);
            if (TYPEOF(sarg) == SYMSXP && sarg == R_DotsSymbol) {
                sarg = PROTECT(lang2(pairlist, sarg));
                arg = PROTECT(eval(sarg, rho));
                unpack_dict
                UNPROTECT(2);
            }
            else {
                arg = PROTECT(eval(sarg, rho));
                unpack_dict
                UNPROTECT(1);
            }
        }
        else {
            REPROTECT(expr = LCONS(sarg, expr), indx);
            if (!isNull(tag))
                SET_TAG(expr, tag);
        }
    }
    REPROTECT(expr = LCONS(CAR(sexpr), expr), indx);
    REPROTECT(expr = lang2(withVisible, expr), indx);
    value = eval(expr, rho);
    LOGICAL(visible)[0] = LOGICAL(VECTOR_ELT(value, 1))[0];
    UNPROTECT(np);
    return VECTOR_ELT(value, 0);
}

#include <R.h>
#include <Rinternals.h>


#include "defines.h"





#define my_SET_TAG do {                                        \
    if (CHAR(STRING_ELT(names, i))[0] != '\0')                 \
        SET_TAG(expr, installTrChar(STRING_ELT(names, i)));    \
} while (0)


#define _unpack_iter_other do {                                \
    tmp2 = PROTECT(allocVector(INTSXP, 1));                    \
    iarg = INTEGER(tmp2);                                      \
    tmp3 = PROTECT(lang3(                                      \
        findVarInFrame(R_BaseEnv, R_Bracket2Symbol),           \
        quoteLang(arg),                                        \
        tmp2                                                   \
    ));                                                        \
    for (int i = (int) len - 1; i >= 0; i--) {                 \
        iarg[0] = i + 1;                                       \
        tmp = PROTECT(eval(tmp3, args_rho));                   \
        tmp = PROTECT(quoteLang(tmp));                         \
        REPROTECT(expr = LCONS(tmp, expr), indx);              \
        UNPROTECT(2);                                          \
    }                                                          \
    UNPROTECT(2);                                              \
} while (0)


#define _unpack_dict_other do {                                \
    tmp2 = PROTECT(allocVector(INTSXP, 1));                    \
    iarg = INTEGER(tmp2);                                      \
    tmp3 = PROTECT(lang3(                                      \
        findVarInFrame(R_BaseEnv, R_Bracket2Symbol),           \
        quoteLang(arg),                                        \
        tmp2                                                   \
    ));                                                        \
    for (int i = (int) len - 1; i >= 0; i--) {                 \
        iarg[0] = i + 1;                                       \
        tmp = PROTECT(eval(tmp3, args_rho));                   \
        tmp = PROTECT(quoteLang(tmp));                         \
        REPROTECT(expr = LCONS(tmp, expr), indx);              \
        UNPROTECT(2);                                          \
        my_SET_TAG;                                            \
    }                                                          \
    UNPROTECT(2);                                              \
} while (0)


#define _unpack_iter do {                                      \
    if (isObject(arg)) {                                       \
        _unpack_iter_other;                                    \
    }                                                          \
    else {                                                     \
        switch (TYPEOF(arg)) {                                 \
        case NILSXP:                                           \
            break;                                             \
        case LGLSXP:                                           \
            iarg = INTEGER(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarLogical(iarg[i]), expr), indx);\
            }                                                  \
            break;                                             \
        case INTSXP:                                           \
            iarg = INTEGER(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarInteger(iarg[i]), expr), indx);\
            }                                                  \
            break;                                             \
        case REALSXP:                                          \
            rarg = REAL(arg);                                  \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarReal(rarg[i]), expr), indx);\
            }                                                  \
            break;                                             \
        case CPLXSXP:                                          \
            carg = COMPLEX(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarComplex(carg[i]), expr), indx);\
            }                                                  \
            break;                                             \
        case STRSXP:                                           \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarString(STRING_ELT(arg, i)), expr), indx);\
            }                                                  \
            break;                                             \
        case RAWSXP:                                           \
            barg = RAW(arg);                                   \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarRaw(barg[i]), expr), indx);\
            }                                                  \
            break;                                             \
        case VECSXP:                                           \
        case EXPRSXP:                                          \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                tmp = PROTECT(VECTOR_ELT(arg, i));             \
                tmp = PROTECT(quoteLang(tmp));                 \
                REPROTECT(expr = LCONS(tmp, expr), indx);      \
                UNPROTECT(2);                                  \
            }                                                  \
            break;                                             \
        case LISTSXP:                                          \
        case LANGSXP:                                          \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                tmp = PROTECT(CAR(nthcdr(arg, i)));            \
                tmp = PROTECT(quoteLang(tmp));                 \
                REPROTECT(expr = LCONS(tmp, expr), indx);      \
                UNPROTECT(2);                                  \
            }                                                  \
            break;                                             \
        default:                                               \
            _unpack_iter_other;                                \
            break;                                             \
        }                                                      \
    }                                                          \
} while (0)


#define _unpack_dict do {                                      \
    if (isObject(arg)) {                                       \
        _unpack_dict_other;                                    \
    }                                                          \
    else {                                                     \
        switch (TYPEOF(arg)) {                                 \
        case NILSXP:                                           \
            break;                                             \
        case LGLSXP:                                           \
            iarg = INTEGER(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarLogical(iarg[i]), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case INTSXP:                                           \
            iarg = INTEGER(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarInteger(iarg[i]), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case REALSXP:                                          \
            rarg = REAL(arg);                                  \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarReal(rarg[i]), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case CPLXSXP:                                          \
            carg = COMPLEX(arg);                               \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarComplex(carg[i]), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case STRSXP:                                           \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarString(STRING_ELT(arg, i)), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case RAWSXP:                                           \
            barg = RAW(arg);                                   \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                REPROTECT(expr = LCONS(ScalarRaw(barg[i]), expr), indx);\
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case VECSXP:                                           \
        case EXPRSXP:                                          \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                tmp = PROTECT(VECTOR_ELT(arg, i));             \
                tmp = PROTECT(quoteLang(tmp));                 \
                REPROTECT(expr = LCONS(tmp, expr), indx);      \
                UNPROTECT(2);                                  \
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        case LISTSXP:                                          \
        case LANGSXP:                                          \
            for (int i = (int) len - 1; i >= 0; i--) {         \
                tmp = PROTECT(CAR(nthcdr(arg, i)));            \
                tmp = PROTECT(quoteLang(tmp));                 \
                REPROTECT(expr = LCONS(tmp, expr), indx);      \
                UNPROTECT(2);                                  \
                my_SET_TAG;                                    \
            }                                                  \
            break;                                             \
        default:                                               \
            _unpack_dict_other;                                \
            break;                                             \
        }                                                      \
    }                                                          \
} while (0)


extern R_xlen_t dispatchLength(SEXP x, SEXP rho);


#define unpack_iter do {                                       \
    len = dispatchLength(arg, args_rho);                       \
    if (len > 0) {                                             \
        if (len >= INT_MAX || len + xlength(expr) >= INT_MAX)  \
            error("too many arguments");                       \
        _unpack_iter;                                          \
    }                                                          \
} while (0)


extern SEXP dispatchNames(SEXP x, SEXP rho);


#define unpack_dict do {                                       \
    len = dispatchLength(arg, args_rho);                       \
    if (len > 0) {                                             \
        if (len >= INT_MAX || len + xlength(expr) >= INT_MAX)  \
            error("too many arguments");                       \
        names = PROTECT(dispatchNames(arg, args_rho));         \
        if (names == R_NilValue) {                             \
            _unpack_iter;                                      \
        }                                                      \
        else {                                                 \
            if (TYPEOF(names) != STRSXP || length(names) != len)\
                error("names(arg) must be a character vector of the same length as arg");\
            _unpack_dict;                                      \
        }                                                      \
        UNPROTECT(1);                                          \
    }                                                          \
} while (0)





SEXP do_doexpr(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP sexpr = findVarInFrame(rho, install("expr"));
    if (sexpr == R_UnboundValue)
        error("could not find 'expr'; should never happen, please report!");
    sexpr = PREXPR(sexpr);
    if (sexpr == R_MissingArg)
        error("argument \"expr\" is missing, with no default");


    /* environment in which we should evaluate the entire expression */
    SEXP sexpr_rho = CADR(args);
    if (TYPEOF(sexpr_rho) != ENVSXP)
        error("invalid 'sexpr_rho', must be an environment");


    if (TYPEOF(sexpr) != LANGSXP) {
        SEXP value = eval(sexpr, sexpr_rho);
        UNPROTECT(nprotect);
        return value;
    }


    /* environment in which we should evaluate the arguments to unpack */
    SEXP args_rho = PROTECT(eval(lang1(install("parent.frame")), rho)); nprotect++;


    SEXP value;


    SEXP tmp, tmp2, tmp3;
    SEXP sargs, tag, sarg, arg, names;
    sargs = CDR(sexpr);


    PROTECT_INDEX indx;
    SEXP expr = R_NilValue;
    PROTECT_WITH_INDEX(expr, &indx); nprotect++;


    int *iarg;
    double *rarg;
    Rcomplex *carg;
    Rbyte *barg;


    R_xlen_t len;
    SEXP pairlist    = findVarInFrame(R_BaseEnv, install("pairlist"   ));
    SEXP withVisible = findVarInFrame(R_BaseEnv, install("withVisible"));


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
            if (sarg == R_DotsSymbol) {
                sarg = PROTECT(lang2(pairlist, sarg));
                arg = PROTECT(eval(sarg, args_rho));
                unpack_iter;
                UNPROTECT(2);
            }
            else {
                arg = PROTECT(eval(sarg, args_rho));
                unpack_iter;
                UNPROTECT(1);
            }
        }
        else if (TYPEOF(sarg) == LANGSXP && CAR(sarg) == install("**") && length(sarg) == 2) {
            if (!isNull(tag))
                error("do not name arguments which are being unpacked");
            sarg = CADR(sarg);
            if (sarg == R_DotsSymbol) {
                sarg = PROTECT(lang2(pairlist, sarg));
                arg = PROTECT(eval(sarg, args_rho));
                unpack_dict;
                UNPROTECT(2);
            }
            else {
                arg = PROTECT(eval(sarg, args_rho));
                unpack_dict;
                UNPROTECT(1);
            }
        }
        else {
            REPROTECT(expr = LCONS(sarg, expr), indx);
            if (!isNull(tag))
                SET_TAG(expr, tag);
        }
    }
    // REPROTECT(expr = LCONS(eval(CAR(sexpr), sexpr_rho), expr), indx);
    REPROTECT(expr = LCONS(CAR(sexpr), expr), indx);
    REPROTECT(expr = lang2(withVisible, expr), indx);
    value = eval(expr, sexpr_rho);
    set_R_Visible(LOGICAL_ELT(VECTOR_ELT(value, 1), 0));
    UNPROTECT(nprotect);
    return VECTOR_ELT(value, 0);
}

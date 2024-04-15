#include <R.h>
#include <Rinternals.h>





// #define debug      // print extra information about how plapply and pvapply are behaving
#define int_nargs  // can an integer number of arguments be passed to a function?? as of 2022-01-11, yes


#include "defines.h"





extern R_xlen_t dispatchLength(SEXP x, SEXP rho);
extern R_xlen_t *dispatchLengths3(SEXP x, SEXP rho, R_xlen_t length_x);
extern SEXP dispatchNames(SEXP x, SEXP rho);


#define GET_LENGTH_X do {                                      \
    length_X = dispatchLength(X, rho);                         \
    PRINT_LENGTH_X;                                            \
    CHECK_LENGTH_X;                                            \
} while (0)


#define GET_LENGTHS_X do {                                     \
    lengths_X = dispatchLengths3(X, rho, length_X);            \
    PRINT_LENGTHS_X;                                           \
} while (0)


#define GET_COMMONLENGTH_X do {                                \
    commonLength = 1;                                          \
    for (R_xlen_t i = 0; i < length_X; i++) {                  \
        if (commonLength) {                                    \
            if (lengths_X[i] == 0 ||                           \
                lengths_X[i] > commonLength)                   \
            {                                                  \
                commonLength = lengths_X[i];                   \
            }                                                  \
        }                                                      \
        else break;                                            \
    }                                                          \
    PRINT_COMMONLENGTH_X;                                      \
} while (0)


#define MAKE_FUN_CALL do {                                     \
    X_index1 = PROTECT(allocVector(VECSXP, length_X)); nprotect++;\
    X_index2 = PROTECT(allocVector(VECSXP, length_X)); nprotect++;\
                                                               \
                                                               \
    nForce = length_X;                                         \
    FUN_call = R_NilValue;                                     \
    PROTECT_WITH_INDEX(FUN_call, &FUN_call_index); nprotect++; \
    if (dots == NULL) {                                        \
        REPROTECT(FUN_call = LCONS(dotsSymbol, FUN_call), FUN_call_index);\
    }                                                          \
    else {                                                     \
        PRINT_DOTS;                                            \
                                                               \
                                                               \
        length_dots = dispatchLength(dots, rho);               \
                                                               \
                                                               \
        PRINT_LENGTH_DOTS;                                     \
                                                               \
                                                               \
        if (length_dots) {                                     \
            CHECK_LENGTH_DOTS;                                 \
            CHECK_NARGS;                                       \
            nForce += length_dots;                             \
                                                               \
                                                               \
            if (isObject(dots)) {                              \
                names_dots = dispatchNames(dots, rho);         \
                                                               \
                                                               \
                if (names_dots != R_NilValue) {                \
                    PROTECT(names_dots); nprotect++;           \
                    if (TYPEOF(names_dots) != STRSXP)          \
                        error("'names(dots)' is not NULL or a character vector");\
                    else if (xlength(names_dots) != length_dots)\
                        error("'length(dots)' and 'length(names(dots))' are not equal");\
                }                                              \
            }                                                  \
            else {                                             \
                names_dots = PROTECT(getAttrib(dots, R_NamesSymbol)); nprotect++;\
            }                                                  \
                                                               \
                                                               \
            PRINT_NAMES_DOTS;                                  \
                                                               \
                                                               \
            INIT_DOTS_REALINDX;                                \
            dots_has_names = names_dots != R_NilValue;         \
                                                               \
                                                               \
            for (R_xlen_t j = length_dots - 1; j >= 0; j--) {  \
                tmp1 = PROTECT(lang3(  /* dots[[j + 1]] */     \
                    R_Bracket2Symbol,                          \
                    dotsSymbol,                                \
                    INDEX(j + 1, dots_realIndx)                \
                ));                                            \
                REPROTECT(FUN_call = LCONS(tmp1, FUN_call), FUN_call_index);\
                UNPROTECT(1);                                  \
                if (dots_has_names && CHAR(STRING_ELT(names_dots, j))[0] != '\0')\
                    SET_TAG(FUN_call, installTrChar(STRING_ELT(names_dots, j)));\
            }                                                  \
        }                                                      \
    }                                                          \
                                                               \
                                                               \
    for (R_xlen_t j = length_X - 1; j >= 0; j--) {             \
        SET_VECTOR_ELT(X_index1, j, INDEX(j + 1, X_realIndx1));\
        SET_VECTOR_ELT(X_index2, j, allocVector(X_realIndx2 ? REALSXP : INTSXP, 1));\
        tmp1 = PROTECT(lang3( /* X[[j + 1]] */                 \
            R_Bracket2Symbol,                                  \
            XSymbol,                                           \
            VECTOR_ELT(X_index1, j)                            \
        ));                                                    \
        tmp2 = PROTECT(lang3( /* X[[j + 1]][[i + 1]] */        \
            R_Bracket2Symbol,                                  \
            tmp1,                                              \
            VECTOR_ELT(X_index2, j)                            \
        ));                                                    \
                                                               \
                                                               \
        REPROTECT(FUN_call = LCONS(tmp2, FUN_call), FUN_call_index);\
        UNPROTECT(2);                                          \
        if (X_has_names && CHAR(STRING_ELT(names_X, j))[0] != '\0')\
            SET_TAG(FUN_call, installTrChar(STRING_ELT(names_X, j)));\
    }                                                          \
                                                               \
                                                               \
    REPROTECT(FUN_call = LCONS(FUNSymbol, FUN_call), FUN_call_index);\
} while (0)


#define GET_NAMES_X do {                                       \
    if (isObject(X)) {                                         \
        names_X = dispatchNames(X, rho);                       \
                                                               \
                                                               \
        if (names_X != R_NilValue) {                           \
            PROTECT(names_X); nprotect++;                      \
            if (TYPEOF(names_X) != STRSXP)                     \
                error("'names(X)' is not NULL or a character vector, but of type '%s'",\
                    type2char(TYPEOF(names_X)));               \
            else if (xlength(names_X) != length_X)             \
                error("'length(X)' (%.0f) and 'length(names(X))' (%.0f) are not equal",\
                    (double) length_X,                         \
                    (double) xlength(names_X));                \
        }                                                      \
    }                                                          \
    else {                                                     \
        names_X = PROTECT(getAttrib(X, R_NamesSymbol)); nprotect++;\
    }                                                          \
    X_has_names = names_X != R_NilValue;                       \
} while (0)


#define DO_FRACTIONAL_RECYCLING_WARNING do {                   \
    if (commonLength) {                                        \
        for (R_xlen_t j = 0; j < length_X; j++) {              \
            if (commonLength % lengths_X[j] != 0) {            \
                warning("an argument will be fractionally recycled");\
                break;                                         \
            }                                                  \
        }                                                      \
    }                                                          \
} while(0)


#define INCREMENT_COUNTERS do {                                \
    for (R_xlen_t j = 0; j < length_X; j++) {                  \
        if (++counters[j] > lengths_X[j])                      \
            counters[j] = 1;                                   \
        if (X_realIndx2)                                       \
            REAL(VECTOR_ELT(X_index2, j))[0] = (double) counters[j];\
        else                                                   \
            INTEGER(VECTOR_ELT(X_index2, j))[0] = (int) counters[j];\
    }                                                          \
} while (0)





#ifdef debug


    #define PRINT_LENGTH_X Rprintf("length(X)    = %.0f\n", (double) length_X)
    #define PRINT_LENGTHS_X do {                                   \
        Rprintf("lengths(X)   =");                                 \
        for (R_xlen_t i = 0; i < length_X; i++)                    \
            Rprintf(" %.0f", (double) lengths_X[i]);               \
        Rprintf("\n");                                             \
    } while (0)
    #define PRINT_COMMONLENGTH_X Rprintf("commonLength = %.0f\n", (double) commonLength)
    #define PRINT_DOTS do {                                        \
        Rprintf("> print(dots)\n");                                \
        eval(lang2(install("print"), dotsSymbol), rho);            \
    } while (0)
    #define PRINT_LENGTH_DOTS Rprintf("length(dots) = %.0f\n", (double) length_dots)
    #define PRINT_NAMES_DOTS do {                                  \
        Rprintf("> print(names(dots))\n");                         \
        eval(lang2(install("print"), names_dots), rho);            \
    } while (0)
    #define PRINT_REALINDX(name, realIndx) Rprintf("%s = %s\n", name, (realIndx) ? "TRUE" : "FALSE")
    #define PRINT_X_REALINDX1   PRINT_REALINDX("X_realIndx1 ", X_realIndx1)
    #define PRINT_X_REALINDX2   PRINT_REALINDX("X_realIndx2 ", X_realIndx2)
    #define PRINT_DOTS_REALINDX PRINT_REALINDX("dots_realIndx ", dots_realIndx)
    #define PRINT_WHEN_LAZY_DUPLICATE Rprintf("Made a lazy duplicate\n")


#else


    #define PRINT_LENGTH_X                 do {} while (0)
    #define PRINT_LENGTHS_X                do {} while (0)
    #define PRINT_COMMONLENGTH_X           do {} while (0)
    #define PRINT_DOTS                     do {} while (0)
    #define PRINT_LENGTH_DOTS              do {} while (0)
    #define PRINT_NAMES_DOTS               do {} while (0)
    #define PRINT_REALINDX(name, realIndx) do {} while (0)
    #define PRINT_X_REALINDX1              do {} while (0)
    #define PRINT_X_REALINDX2              do {} while (0)
    #define PRINT_DOTS_REALINDX            do {} while (0)
    #define PRINT_WHEN_LAZY_DUPLICATE      do {} while (0)


#endif





#define INIT_REALINDX(x, length, name) do {                    \
    (x = length > INT_MAX);                                    \
    PRINT_REALINDX(name, x);                                   \
} while (0)
#define INIT_X_REALINDX2 INIT_REALINDX(X_realIndx2, commonLength, "X_realIndx2 ")





#ifdef int_nargs


    #define CHECK_LENGTH(length, name) do {                        \
        if ((length) > INT_MAX)                                    \
            error("'length(%s)' (%.0f) cannot be greater than '.Machine$integer.max' (%d)",\
                name, (double) (length), INT_MAX);                 \
    } while (0)
    #define CHECK_LENGTH_X    CHECK_LENGTH(length_X, "X")
    #define CHECK_LENGTH_DOTS CHECK_LENGTH(length_dots, "dots")
    #define CHECK_NARGS do {                                       \
        if (((double) INT_MAX) - length_X - length_dots < 0)       \
            error("too many arguments");                           \
    } while (0)
    #define INIT_X_REALINDX1   do {} while (0)
    #define INIT_DOTS_REALINDX do {} while (0)
    #define INDEX(i, REALINDX) ScalarInteger(i)


#else


    #define CHECK_LENGTH      do {} while (0)
    #define CHECK_LENGTH_X    do {} while (0)
    #define CHECK_LENGTH_DOTS do {} while (0)
    #define CHECK_NARGS do {                                       \
        if (R_LEN_T_MAX - length_X - length_dots < 0)              \
            error("too many arguments");                           \
    } while (0)
    #define INIT_X_REALINDX1 INIT_REALINDX(X_realIndx1, length_X, "X_realIndx1 ")
    #define INIT_DOTS_REALINDX INIT_REALINDX(dots_realIndx, length_dots, "dots_realIndx ")
    #define INDEX(i, REALINDX) (REALINDX ? ScalarReal(i) : ScalarInteger(i))


#endif


#define set_R_Visible_TRUE (eval(R_NilValue, R_EmptyEnv))





SEXP do_plapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP X   , XSymbol   ,
         FUN , FUNSymbol ,
         dots, dotsSymbol;


    SEXP syms = CDDR(call);  /* skip past .External and C_plapply */
    args = CDR(args);  /* skip past C_plapply */


    int nargs = length(args);
    if (nargs == 2) {
        XSymbol    = CAR(syms);
        FUNSymbol  = CADR(syms);
        dotsSymbol = R_DotsSymbol;
        X    = CAR(args);
        FUN  = CADR(args);
        dots = NULL;
    }
    else if (nargs == 3) {
        XSymbol    = CAR(syms);
        FUNSymbol  = CADR(syms);
        dotsSymbol = CADDR(syms);
        X    = CAR(args);
        FUN  = CADR(args);
        dots = CADDR(args);
    }
    else errorcall(call, (nargs == 1) ? "%d argument passed to .External(%s) which requires %s" :
                                        "%d arguments passed to .External(%s) which requires %s",
                                        nargs, "C_plapply", "2 or 3");


    if (TYPEOF(XSymbol) != SYMSXP)
        errorcall(call, "first argument must be a symbol");
    if (TYPEOF(FUNSymbol) != SYMSXP)
        errorcall(call, "second argument must be a symbol");
    if (TYPEOF(dotsSymbol) != SYMSXP)
        errorcall(call, "third argument must be a symbol");


    if (!isFunction(FUN))
        errorcall(call, "second argument must be a function");


    SEXP X_index1, X_index2, names_X,
        names_dots,
        tmp, tmp1, tmp2,
        expr, FUN_call, value;
    Rboolean X_realIndx2, X_has_names, dots_has_names;


#ifndef int_nargs
    Rboolean X_realIndx1, dots_realIndx;
#endif


    PROTECT_INDEX FUN_call_index;
    R_xlen_t length_X, *lengths_X, commonLength, *counters,
        length_dots,
        nForce;
    int nprotect;  // number of protected arguments


    nprotect = 0;


    GET_LENGTH_X;


    if (length_X == 0) {
        set_R_Visible_TRUE;
        UNPROTECT(nprotect);
        return allocVector(VECSXP, 0);
    }


    GET_LENGTHS_X;


    // from lengths(X), determine the output vector length


    GET_COMMONLENGTH_X;


    DO_FRACTIONAL_RECYCLING_WARNING;


    INIT_X_REALINDX1;


    // create the output list with length 'commonLength'
    // copy names from elements of 'X' with length
    //   'commonLength' to output list


    value = PROTECT(allocVector(VECSXP, commonLength)); nprotect++;
    for (R_xlen_t i = 0; i < length_X; i++) {
        if (lengths_X[i] == commonLength) {
            expr = PROTECT(lang2( // names(X[[i + 1]])
                R_NamesSymbol,
                lang3( // X[[i + 1]]
                    R_Bracket2Symbol,
                    XSymbol,
                    INDEX(i + 1, X_realIndx1)
                )
            ));
            tmp = eval(expr, rho);
            UNPROTECT(1);
            if (tmp != R_NilValue) {
                if (TYPEOF(tmp) != STRSXP)
                    error("'names(X[[%.0f]])' is not NULL or a character vector, but of type '%s'",
                        (double) (i + 1), type2char(TYPEOF(tmp)));
                else if (xlength(tmp) != commonLength)
                    error("'length(X[[%.0f]])' (%.0f) and 'length(names(X[[%.0f]]))' (%.0f) are not equal",
                        (double) (i + 1), (double) (lengths_X[i]),
                        (double) (i + 1), (double) commonLength);
                setAttrib(value, R_NamesSymbol, tmp);
                break;
            }
        }
    }


    if (commonLength == 0) {
        set_R_Visible_TRUE;
        UNPROTECT(nprotect);
        return value;
    }


    // at this point, we will evaluate FUN at least once
    //
    // length(X) isn't 0 and the common length also isn't 0
    //
    // get names(X) so that we can start building the call
    // to FUN


    GET_NAMES_X;


    INIT_X_REALINDX2;


    MAKE_FUN_CALL;


    counters = (R_xlen_t *) R_alloc(length_X, sizeof(R_xlen_t));
    if (length_X) memset(counters, 0, length_X * sizeof(R_xlen_t));


    for (R_xlen_t i = 0; i < commonLength; i++) {
        INCREMENT_COUNTERS;


        /*
           imma be real wichyu, i have no idea why we use
           forceAndCall, but both do_lapply and do_mapply
           do so i will too
         */
        tmp = R_forceAndCall(FUN_call, nForce, rho);
        if (MAYBE_REFERENCED(tmp)) {
            tmp = lazy_duplicate(tmp);
            PRINT_WHEN_LAZY_DUPLICATE;
        }
        SET_VECTOR_ELT(value, i, tmp);
    }


    set_R_Visible_TRUE;
    UNPROTECT(nprotect);
    return value;
}


SEXP do_pvapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP X        , XSymbol   ,
         FUN      , FUNSymbol ,
         FUN_VALUE,
         dots     , dotsSymbol,
         USE_NAMES;


    SEXP syms = CDDR(call);  /* skip past .External2 and C_pvapply */
    args = CDR(args); /* skip past C_pvapply */


    int nargs = length(args);
    if (nargs == 4) {
        XSymbol    = CAR(syms);
        FUNSymbol  = CADR(syms);
        dotsSymbol = R_DotsSymbol;
        X         = CAR(args);
        FUN       = CADR(args);
        FUN_VALUE = CADDR(args);
        dots      = NULL;
        USE_NAMES = CADDDR(args);
    }
    else if (nargs == 5) {
        XSymbol    = CAR(syms);
        FUNSymbol  = CADR(syms);
        dotsSymbol = CADDDR(syms);
        X         = CAR(args);
        FUN       = CADR(args);
        FUN_VALUE = CADDR(args);
        dots      = CADDDR(args);
        USE_NAMES = CAD4R(args);
    }
    else errorcall(call, (nargs == 1) ? "%d argument passed to .External(%s) which requires %s" :
                                        "%d arguments passed to .External(%s) which requires %s",
                                        nargs, "C_pvapply", "4 or 5");


    if (TYPEOF(XSymbol) != SYMSXP)
        errorcall(call, "first argument must be a symbol");
    if (TYPEOF(FUNSymbol) != SYMSXP)
        errorcall(call, "second argument must be a symbol");
    if (TYPEOF(dotsSymbol) != SYMSXP)
        errorcall(call, "fourth argument must be a symbol");


    if (!isFunction(FUN))
        errorcall(call, "second argument must be a function");


    SEXP X_index1, X_index2, names_X,
        names_dots,
        tmp, tmp1, tmp2,
        expr, FUN_call, value;
    Rboolean X_realIndx2, X_has_names, dots_has_names;


#ifndef int_nargs
    Rboolean X_realIndx1, dots_realIndx;
#endif


    PROTECT_INDEX FUN_call_index;
    R_xlen_t length_X, *lengths_X, commonLength, *counters,
        length_dots,
        nForce;
    int nprotect;  // number of protected arguments


    // extra arguments specific to pvapply
    SEXP dim_FUN_VALUE, dim, rowNames, names, dimnames;
    SEXPTYPE type_FUN_VALUE, type_tmp;
    Rboolean use_names, is_array, okay;
    PROTECT_INDEX rowNames_index, tmp_index;
    R_xlen_t length_FUN_VALUE, offset;
    int rank_FUN_VALUE;


    if (!isVector(FUN_VALUE)) error("'FUN.VALUE' must be a vector");
    use_names = asLogical(USE_NAMES);
    if (use_names == NA_LOGICAL) error("invalid '%s' value", "USE.NAMES");


    nprotect = 0;


    GET_LENGTH_X;


    GET_LENGTHS_X;


    // from lengths(X), determine the output vector length


    GET_COMMONLENGTH_X;


    DO_FRACTIONAL_RECYCLING_WARNING;


    INIT_X_REALINDX1;


    length_FUN_VALUE = xlength(FUN_VALUE);
    if (length_FUN_VALUE > 1) {
        if (length_FUN_VALUE > INT_MAX || commonLength > INT_MAX)
            error("long vectors are not supported for matrix/array results");
    }
    type_FUN_VALUE = TYPEOF(FUN_VALUE);
    if (type_FUN_VALUE != CPLXSXP && type_FUN_VALUE != REALSXP &&
        type_FUN_VALUE != INTSXP  && type_FUN_VALUE != LGLSXP &&
        type_FUN_VALUE != RAWSXP  && type_FUN_VALUE != STRSXP &&
        type_FUN_VALUE != VECSXP)
        error("type '%s' is not supported", type2char(type_FUN_VALUE));
    dim_FUN_VALUE = PROTECT(getAttrib(FUN_VALUE, R_DimSymbol)); nprotect++;
    is_array = ( ( TYPEOF(dim_FUN_VALUE) == INTSXP ) &&
                 ( length(dim_FUN_VALUE) >= 1 ) );


    value = PROTECT(allocVector(type_FUN_VALUE,
        commonLength * length_FUN_VALUE)); nprotect++;
    names = R_NilValue;
    rowNames = R_NilValue;
    rowNames_index = 0;
    if (use_names) {
        for (R_xlen_t i = 0; i < length_X; i++) {
            if (lengths_X[i] == commonLength) {
                expr = PROTECT(lang2( /* names(X[[i + 1]]) */
                    R_NamesSymbol,
                    lang3( /* X[[i + 1]] */
                        R_Bracket2Symbol,
                        XSymbol,
                        INDEX(i + 1, X_realIndx1)
                    )
                ));
                names = eval(expr, rho);
                UNPROTECT(1);
                if (names != R_NilValue) {
                    if (TYPEOF(names) != STRSXP)
                        error("'names(X[[%.0f]])' is not NULL or a character vector, but of type '%s'",
                            (double) (i + 1), type2char(TYPEOF(names)));
                    else if (xlength(names) != commonLength)
                        error("'length(X[[%.0f]])' (%.0f) and 'length(names(X[[%.0f]]))' (%.0f) are not equal",
                            (double) (i + 1), (double) (lengths_X[i]),
                            (double) (i + 1), (double) commonLength);
                    break;
                }
            }
        }
        if (names == R_NilValue) for (R_xlen_t i = 0; i < length_X; i++) {
            if (lengths_X[i] == commonLength) {
                expr = PROTECT(lang3( /* X[[i + 1]] */
                    R_Bracket2Symbol,
                    XSymbol,
                    INDEX(i + 1, X_realIndx1)
                ));
                names = eval(expr, rho);
                UNPROTECT(1);
                if (TYPEOF(names) == STRSXP) {
                    PROTECT(names); nprotect++;
                    break;
                }
                else names = R_NilValue;
            }
        }
        PROTECT_WITH_INDEX(rowNames = getAttrib(FUN_VALUE,
            is_array ? R_DimNamesSymbol : R_NamesSymbol), &rowNames_index); nprotect++;
    }


    /*
       unlike plapply, we can't assign names before the loop,
       so we can't return early. but we don't want to build the call if we
       don't need to. so we wrap all of the building and evaluating the call in

       if (commonLength) {
       }

       so we will only build the call if it will be evaluated at least once
     */
    if (commonLength) {
        GET_NAMES_X;


        INIT_X_REALINDX2;


        MAKE_FUN_CALL;


        counters = (R_xlen_t *) R_alloc(length_X, sizeof(R_xlen_t));
        if (length_X) memset(counters, 0, length_X * sizeof(R_xlen_t));


        offset = 0;
        for (R_xlen_t i = 0; i < commonLength; i++) {
            INCREMENT_COUNTERS;


            /*
               imma be real wichyu, i have no idea why we use
               forceAndCall, but both do_lapply and do_mapply
               do so i will too
             */
            tmp = R_forceAndCall(FUN_call, nForce, rho);
            if (MAYBE_REFERENCED(tmp)) {
                tmp = lazy_duplicate(tmp);
                PRINT_WHEN_LAZY_DUPLICATE;
            }
            PROTECT_WITH_INDEX(tmp, &tmp_index);
            if (xlength(tmp) != length_FUN_VALUE)
                error("values must be length %.0f,\n but result %.0f is length %.0f",
                    (double) length_FUN_VALUE, (double) (i + 1), (double) xlength(tmp));
            type_tmp = TYPEOF(tmp);
            if (type_tmp != type_FUN_VALUE) {
                okay = FALSE;
                switch (type_FUN_VALUE) {
                case CPLXSXP: okay = (type_tmp == REALSXP) || (type_tmp == INTSXP) ||
                    (type_tmp == LGLSXP); break;
                case REALSXP: okay = (type_tmp == INTSXP) || (type_tmp == LGLSXP); break;
                case INTSXP: okay = (type_tmp == LGLSXP); break;
                }
                if (!okay)
                    error("values must be type '%s',\n but result %.0f is type '%s'",
                        type2char(type_FUN_VALUE), (double) (i + 1), type2char(type_tmp));
                REPROTECT(tmp = coerceVector(tmp, type_FUN_VALUE), tmp_index);
            }
            if (use_names && isNull(rowNames)) {
                if (is_array) {
                    if (conformable(tmp, FUN_VALUE))
                        REPROTECT(rowNames = getAttrib(tmp, R_DimNamesSymbol), rowNames_index);
                }
                else REPROTECT(rowNames = getAttrib(tmp, R_NamesSymbol), rowNames_index);
            }
            if (length_FUN_VALUE == 1) {
                switch (type_FUN_VALUE) {
                case CPLXSXP: COMPLEX(value)[i] = COMPLEX(tmp)[0]; break;
                case REALSXP: REAL   (value)[i] = REAL   (tmp)[0]; break;
                case INTSXP:  INTEGER(value)[i] = INTEGER(tmp)[0]; break;
                case LGLSXP:  LOGICAL(value)[i] = LOGICAL(tmp)[0]; break;
                case RAWSXP:  RAW    (value)[i] = RAW    (tmp)[0]; break;
                case STRSXP:  SET_STRING_ELT(value, i, STRING_ELT(tmp, 0)); break;
                case VECSXP:  SET_VECTOR_ELT(value, i, VECTOR_ELT(tmp, 0)); break;
                default: error("bruh?");
                }
            }
            else {
                switch (type_FUN_VALUE) {
                case REALSXP:
                    memcpy(REAL(value) + offset,
                           REAL(tmp), length_FUN_VALUE * sizeof(double)); break;
                case INTSXP:
                    memcpy(INTEGER(value) + offset,
                           INTEGER(tmp), length_FUN_VALUE * sizeof(int)); break;
                case LGLSXP:
                    memcpy(LOGICAL(value) + offset,
                           LOGICAL(tmp), length_FUN_VALUE * sizeof(int)); break;
                case RAWSXP:
                    memcpy(RAW(value) + offset,
                           RAW(tmp), length_FUN_VALUE * sizeof(Rbyte)); break;
                case CPLXSXP:
                    memcpy(COMPLEX(value) + offset,
                           COMPLEX(tmp), length_FUN_VALUE * sizeof(Rcomplex)); break;
                case STRSXP:
                    for (R_xlen_t k = 0; k < length_FUN_VALUE; k++)
                        SET_STRING_ELT(value, offset + k, STRING_ELT(tmp, k));
                    break;
                case VECSXP:
                    for (R_xlen_t k = 0; k < length_FUN_VALUE; k++)
                        SET_VECTOR_ELT(value, offset + k, VECTOR_ELT(tmp, k));
                    break;
                default:
                    error("bruh??");
                }
                offset += length_FUN_VALUE;
            }
            UNPROTECT(1);
        }
    }


    if (length_FUN_VALUE != 1) {
        rank_FUN_VALUE = is_array ? length(dim_FUN_VALUE) : 1;
        PROTECT(dim = allocVector(INTSXP, rank_FUN_VALUE + 1));
        if (is_array)
            for (int j = 0; j < rank_FUN_VALUE; j++)
                INTEGER(dim)[j] = INTEGER(dim_FUN_VALUE)[j];
        else INTEGER(dim)[0] = length_FUN_VALUE;
        INTEGER(dim)[rank_FUN_VALUE] = (int) commonLength;
        setAttrib(value, R_DimSymbol, dim);
        UNPROTECT(1);
    }


    if (use_names) {
        if (length_FUN_VALUE == 1) {
            if (!isNull(names))
                setAttrib(value, R_NamesSymbol, names);
        }
        else {
            if (!isNull(names) || !isNull(rowNames)) {
                PROTECT(dimnames = allocVector(VECSXP, rank_FUN_VALUE + 1));
                if (is_array && !isNull(rowNames)) {
                    for (int j = 0; j < rank_FUN_VALUE; j++)
                        SET_VECTOR_ELT(dimnames, j, VECTOR_ELT(rowNames, j));
                }
                else SET_VECTOR_ELT(dimnames, 0, rowNames);
                SET_VECTOR_ELT(dimnames, rank_FUN_VALUE, names);
                setAttrib(value, R_DimNamesSymbol, dimnames);
                UNPROTECT(1);
            }
        }
    }


    set_R_Visible_TRUE;
    UNPROTECT(nprotect);
    return value;
}

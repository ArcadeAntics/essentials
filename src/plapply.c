#include <R.h>
#include <Rinternals.h>





// #define debug
#define int_nargs





#define CHECK_DOTS_AS_DOTSXP_AND_RHO {                         \
    _dots_as_DOTSXP = asLogical(dots_as_DOTSXP);               \
    if (_dots_as_DOTSXP == NA_LOGICAL)                         \
        error("invalid '%s'", "dots.as.DOTSXP");               \
    if (TYPEOF(rho) != ENVSXP)                                 \
        error("invalid '%s'", "rho");                          \
}


#define GET_LENGTH_X {                                         \
    if (isObject(X)) {                                         \
        expr = PROTECT(lang2(  /* length(X) */                 \
            install("length"),                                 \
            XSymbol                                            \
        ));                                                    \
        tmp = PROTECT(eval(expr, rho));                        \
        length_X = (R_xlen_t)                                  \
            (TYPEOF(tmp) == REALSXP ? REAL(tmp)[0] : asInteger(tmp));\
        UNPROTECT(2);                                          \
    }                                                          \
    else length_X = xlength(X);                                \
    PRINT_LENGTH_X;                                            \
    CHECK_LENGTH_X;                                            \
}


#define GET_LENGTHS_X {                                        \
    /* find lengths(X), and convert to a R_xlen_t array */     \
                                                               \
                                                               \
    expr = PROTECT(lang2(  /* lengths(X) */                    \
        install("lengths"),                                    \
        XSymbol                                                \
    ));                             np++;                      \
    tmp = PROTECT(eval(expr, rho)); np++;                      \
    if (xlength(tmp) != length_X)                              \
        error("'length(X)' (%.0f) and 'length(lengths(X))' (%.0f) are not equal",\
            (double) length_X, (double) xlength(tmp));         \
                                                               \
                                                               \
    lengths_X = (R_xlen_t *) R_alloc(length_X, sizeof(R_xlen_t));\
    switch (TYPEOF(tmp)) {                                     \
    case REALSXP:                                              \
        for (R_xlen_t i = 0; i < length_X; i++)                \
            lengths_X[i] = (R_xlen_t) (REAL(tmp)[i]);          \
        break;                                                 \
    case INTSXP:                                               \
        for (R_xlen_t i = 0; i < length_X; i++)                \
            lengths_X[i] = (R_xlen_t) (INTEGER(tmp)[i]);       \
        break;                                                 \
    default:                                                   \
        error("invalid 'lengths(X)' of type '%s'", type2char(TYPEOF(tmp)));\
    }                                                          \
    PRINT_LENGTHS_X;                                           \
}


#define GET_COMMONLENGTH_X {                                   \
    commonLength = 1;                                          \
    for (R_xlen_t i = 0; i < length_X; i++) {                  \
        if (commonLength) {                                    \
            if (lengths_X[i] == 0 || lengths_X[i] > commonLength)\
                commonLength = lengths_X[i];                   \
        }                                                      \
        else break;                                            \
    }                                                          \
    PRINT_COMMONLENGTH_X;                                      \
}


#define GET_FUN_CALL {                                         \
    X_index1 = PROTECT(allocVector(VECSXP, length_X));         \
    X_index2 = PROTECT(allocVector(VECSXP, length_X));         \
    np += 2;                                                   \
                                                               \
                                                               \
    nForce = length_X;                                         \
    FUN_call = R_NilValue;                                     \
    PROTECT_WITH_INDEX(FUN_call, &FUN_call_index); np++;       \
    if (_dots_as_DOTSXP) {                                     \
        REPROTECT(FUN_call = LCONS(R_DotsSymbol, FUN_call), FUN_call_index);\
    }                                                          \
    else {                                                     \
        dotsSymbol = install("dots");                          \
        dots = PROTECT(eval(dotsSymbol, rho)); np++;           \
                                                               \
                                                               \
        PRINT_DOTS;                                            \
                                                               \
                                                               \
        if (isObject(dots)) {                                  \
            expr = PROTECT(lang2(  /* length(dots) */          \
                install("length"),                             \
                dotsSymbol                                     \
            ));                                                \
            tmp = PROTECT(eval(expr, rho));                    \
            length_dots = (R_xlen_t)                           \
                (TYPEOF(tmp) == REALSXP ? REAL(tmp)[0] : asInteger(tmp));\
            UNPROTECT(2);                                      \
        }                                                      \
        else length_dots = xlength(dots);                      \
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
                expr = PROTECT(lang2(  /* names(dots) */       \
                    install("names"),                          \
                    dotsSymbol                                 \
                ));                                            \
                names_dots = PROTECT(eval(expr, rho));         \
                                                               \
                                                               \
                if (names_dots != R_NilValue) {                \
                    if (TYPEOF(names_dots) != STRSXP)          \
                        error("'names(dots)' is not NULL or a character vector");\
                    else if (xlength(names_dots) != length_dots)\
                        error("'length(dots)' and 'length(names(dots))' are not equal");\
                    np += 2;                                   \
                }                                              \
                else UNPROTECT(2);                             \
            }                                                  \
            else {                                             \
                names_dots = PROTECT(getAttrib(dots, R_NamesSymbol));\
                np++;                                          \
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
        /*                                                     \
           SET_VECTOR_ELT(X_index2, j, allocVector(X_realIndx2 ? REALSXP : INTSXP, 1));\
           SEXP tmp2 = PROTECT(lang3(                          \
               R_Bracket2Symbol,                               \
               lang3(                                          \
                   R_Bracket2Symbol,                           \
                   XSymbol,                                    \
#ifdef int_nargs                                               \
                   ScalarInteger(j + 1)                        \
#else                                                          \
                   X_realIndx1 ? ScalarReal(j + 1) : ScalarInteger(j + 1)\
#endif                                                         \
               ),                                              \
               VECTOR_ELT(X_index2, j)                         \
           ))                                                  \
         */                                                    \
                                                               \
                                                               \
        REPROTECT(FUN_call = LCONS(tmp2, FUN_call), FUN_call_index);\
        UNPROTECT(2);                                          \
        if (X_has_names && CHAR(STRING_ELT(names_X, j))[0] != '\0')\
            SET_TAG(FUN_call, installTrChar(STRING_ELT(names_X, j)));\
    }                                                          \
                                                               \
                                                               \
    REPROTECT(FUN_call = LCONS(install("FUN"), FUN_call), FUN_call_index);\
}


#define GET_NAMES_X {                                          \
    if (isObject(X)) {                                         \
        expr = PROTECT(lang2( /* names(X) */                   \
            install("names"),                                  \
            XSymbol                                            \
        ));                                 np++;              \
        names_X = PROTECT(eval(expr, rho)); np++;              \
                                                               \
                                                               \
        if (names_X != R_NilValue) {                           \
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
        names_X = PROTECT(getAttrib(X, R_NamesSymbol));        \
        np++;                                                  \
    }                                                          \
    X_has_names = names_X != R_NilValue;                       \
}


#define FRACTIONAL_RECYCLING_WARNING {                         \
    if (commonLength) {                                        \
        for (R_xlen_t j = 0; j < length_X; j++) {              \
            if (commonLength % lengths_X[j] != 0) {            \
                warning("an argument will be fractionally recycled");\
                break;                                         \
            }                                                  \
        }                                                      \
    }                                                          \
}


#define INCREMENT_COUNTERS {                                   \
    for (R_xlen_t j = 0; j < length_X; j++) {                  \
        if (++counters[j] > lengths_X[j])                      \
            counters[j] = 1;                                   \
        if (X_realIndx2)                                       \
            REAL(VECTOR_ELT(X_index2, j))[0] = (double) counters[j];\
        else                                                   \
            INTEGER(VECTOR_ELT(X_index2, j))[0] = (int) counters[j];\
    }                                                          \
}





#ifdef debug


#define PRINT_LENGTH_X Rprintf("length(X)    = %.0f\n", (double) length_X)
#define PRINT_LENGTHS_X {                                      \
    Rprintf("lengths(X)   =");                                 \
    for (R_xlen_t i = 0; i < length_X; i++)                    \
        Rprintf(" %.0f", (double) lengths_X[i]);               \
    Rprintf("\n");                                             \
}
#define PRINT_COMMONLENGTH_X Rprintf("commonLength = %.0f\n", (double) commonLength)
#define PRINT_DOTS {                                           \
    Rprintf("> print(dots)\n");                                \
    eval(lang2(install("print"), dotsSymbol), rho);            \
}
#define PRINT_LENGTH_DOTS Rprintf("length(dots) = %.0f\n", (double) length_dots)
#define PRINT_NAMES_DOTS {                                     \
    Rprintf("> print(names(dots))\n");                         \
    eval(lang2(install("print"), names_dots), rho);            \
}
#define PRINT_REALINDX(name, realIndx) Rprintf("%s = %s\n", name, (realIndx) ? "TRUE" : "FALSE")
#define PRINT_X_REALINDX1   PRINT_REALINDX("X_realIndx1 ", X_realIndx1)
#define PRINT_X_REALINDX2   PRINT_REALINDX("X_realIndx2 ", X_realIndx2)
#define PRINT_DOTS_REALINDX PRINT_REALINDX("dots_realIndx ", dots_realIndx)
#define PRINT_WHEN_LAZY_DUPLICATE Rprintf("Made a lazy duplicate\n")


#else


#define PRINT_LENGTH_X
#define PRINT_LENGTHS_X
#define PRINT_COMMONLENGTH_X
#define PRINT_DOTS
#define PRINT_LENGTH_DOTS
#define PRINT_NAMES_DOTS
#define PRINT_REALINDX(name, realIndx)
#define PRINT_X_REALINDX1
#define PRINT_X_REALINDX2
#define PRINT_DOTS_REALINDX
#define PRINT_WHEN_LAZY_DUPLICATE


#endif





#define INIT_REALINDX(x, length, name) {                       \
    (x = length > INT_MAX);                                    \
    PRINT_REALINDX(name, x);                                   \
}
#define INIT_X_REALINDX2 INIT_REALINDX(X_realIndx2, commonLength, "X_realIndx2 ")





#ifdef int_nargs


#define CHECK_LENGTH(length, name) {                           \
    if ((length) > INT_MAX)                                    \
        error("'length(%s)' (%.0f) cannot be greater than '.Machine$integer.max' (%d)",\
            name, (double) (length), INT_MAX);                 \
}
#define CHECK_LENGTH_X    CHECK_LENGTH(length_X, "X")
#define CHECK_LENGTH_DOTS CHECK_LENGTH(length_dots, "dots")
#define CHECK_NARGS {                                          \
    if (((double) INT_MAX) - length_X - length_dots < 0)       \
        error("too many arguments");                           \
}
#define INIT_X_REALINDX1
#define INIT_DOTS_REALINDX
#define INDEX(i, REALINDX) ScalarInteger(i)


#else


#define CHECK_LENGTH
#define CHECK_LENGTH_X
#define CHECK_LENGTH_DOTS
#define CHECK_NARGS {                                          \
    if (R_LEN_T_MAX - length_X - length_dots < 0)              \
        error("too many arguments");                           \
}
#define INIT_X_REALINDX1 INIT_REALINDX(X_realIndx1, length_X, "X_realIndx1 ")
#define INIT_DOTS_REALINDX INIT_REALINDX(dots_realIndx, length_dots, "dots_realIndx ")
#define INDEX(i, REALINDX) (REALINDX ? ScalarReal(i) : ScalarInteger(i))


#endif





SEXP do_plapply(SEXP X, SEXP FUN, SEXP dots_as_DOTSXP, SEXP rho)
{
    SEXP XSymbol, X_index1, X_index2, names_X,
        dots, dotsSymbol, names_dots,
        tmp, tmp1, tmp2,
        expr, FUN_call, value;
    Rboolean _dots_as_DOTSXP, X_realIndx2, X_has_names, dots_has_names;


#ifndef int_nargs
    Rboolean X_realIndx1, dots_realIndx;
#endif


    PROTECT_INDEX FUN_call_index;
    R_xlen_t length_X, *lengths_X, commonLength, *counters,
        length_dots,
        nForce;
    int np;  // number of protected arguments


    CHECK_DOTS_AS_DOTSXP_AND_RHO;


    XSymbol = install("X");
    np = 0;


    GET_LENGTH_X;


    if (length_X == 0) {
        UNPROTECT(np);
        return allocVector(VECSXP, 0);
    }


    GET_LENGTHS_X;


    // from lengths(X), determine the output vector length


    GET_COMMONLENGTH_X;


    FRACTIONAL_RECYCLING_WARNING;


    INIT_X_REALINDX1;


    // create the output list with length 'commonLength'
    // copy names from elements of 'X' with length
    //   'commonLength' to output list


    value = PROTECT(allocVector(VECSXP, commonLength)); np++;
    for (R_xlen_t i = 0; i < length_X; i++) {
        if (lengths_X[i] == commonLength) {
            expr = PROTECT(lang2( // names(X[[i + 1]])
                install("names"),
                lang3( // X[[i + 1]]
                    R_Bracket2Symbol,
                    XSymbol,
                    INDEX(i + 1, X_realIndx1)
                )
            ));
            tmp = PROTECT(eval(expr, rho));
            if (tmp != R_NilValue) {
                if (TYPEOF(tmp) != STRSXP)
                    error("'names(X[[%.0f]])' is not NULL or a character vector, but of type '%s'",
                        (double) (i + 1), type2char(TYPEOF(tmp)));
                else if (xlength(tmp) != commonLength)
                    error("'length(X[[%.0f]])' (%.0f) and 'length(names(X[[%.0f]]))' (%.0f) are not equal",
                        (double) (i + 1), (double) (lengths_X[i]),
                        (double) (i + 1), (double) commonLength);
                setAttrib(value, R_NamesSymbol, tmp);
                np += 2;
                break;
            }
            UNPROTECT(2);
        }
    }


    if (commonLength == 0) {
        UNPROTECT(np);
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


    GET_FUN_CALL;


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


    UNPROTECT(np);
    return value;
}


SEXP do_pvapply(SEXP X, SEXP FUN, SEXP FUN_VALUE, SEXP USE_NAMES,
    SEXP dots_as_DOTSXP, SEXP rho)
{
    SEXP XSymbol, X_index1, X_index2, names_X,
        dots, dotsSymbol, names_dots,
        tmp, tmp1, tmp2,
        expr, FUN_call, value;
    Rboolean _dots_as_DOTSXP, X_realIndx2, X_has_names, dots_has_names;


#ifndef int_nargs
    Rboolean X_realIndx1, dots_realIndx;
#endif


    PROTECT_INDEX FUN_call_index;
    R_xlen_t length_X, *lengths_X, commonLength, *counters,
        length_dots,
        nForce;
    int np;  // number of protected arguments


    // extra arguments specific to pvapply
    SEXP dim_FUN_VALUE, dim, rowNames, names, dimnames;
    SEXPTYPE type_FUN_VALUE, type_tmp;
    Rboolean use_names, is_array, okay;
    PROTECT_INDEX rowNames_index, tmp_index;
    R_xlen_t length_FUN_VALUE, offset;
    int rank_FUN_VALUE;


    CHECK_DOTS_AS_DOTSXP_AND_RHO;


    if (!isVector(FUN_VALUE)) error("'FUN.VALUE' must be a vector");
    use_names = asLogical(USE_NAMES);
    if (use_names == NA_LOGICAL) error("invalid '%s' value", "USE.NAMES");


    XSymbol = install("X");
    np = 0;


    GET_LENGTH_X;


    GET_LENGTHS_X;


    // from lengths(X), determine the output vector length


    GET_COMMONLENGTH_X;


    FRACTIONAL_RECYCLING_WARNING;


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
    dim_FUN_VALUE = PROTECT(getAttrib(FUN_VALUE, R_DimSymbol)); np++;
    is_array = TYPEOF(dim_FUN_VALUE) == INTSXP && length(dim_FUN_VALUE) >= 1;


    value = PROTECT(allocVector(type_FUN_VALUE,
        commonLength * length_FUN_VALUE)); np++;
    names = R_NilValue;
    rowNames = R_NilValue;
    rowNames_index = 0;
    if (use_names) {
        for (R_xlen_t i = 0; i < length_X; i++) {
            if (lengths_X[i] == commonLength) {
                expr = PROTECT(lang2( /* names(X[[i + 1]]) */
                    install("names"),
                    lang3( /* X[[i + 1]] */
                        R_Bracket2Symbol,
                        XSymbol,
                        INDEX(i + 1, X_realIndx1)
                    )
                ));
                names = PROTECT(eval(expr, rho));
                if (names != R_NilValue) {
                    if (TYPEOF(names) != STRSXP)
                        error("'names(X[[%.0f]])' is not NULL or a character vector, but of type '%s'",
                            (double) (i + 1), type2char(TYPEOF(names)));
                    else if (xlength(names) != commonLength)
                        error("'length(X[[%.0f]])' (%.0f) and 'length(names(X[[%.0f]]))' (%.0f) are not equal",
                            (double) (i + 1), (double) (lengths_X[i]),
                            (double) (i + 1), (double) commonLength);
                    np += 2;
                    break;
                }
                UNPROTECT(2);
            }
        }
        if (names == R_NilValue) for (R_xlen_t i = 0; i < length_X; i++) {
            if (lengths_X[i] == commonLength) {
                expr = PROTECT(lang3( /* X[[i + 1]] */
                    R_Bracket2Symbol,
                    XSymbol,
                    INDEX(i + 1, X_realIndx1)
                ));
                names = PROTECT(eval(expr, rho));
                if (TYPEOF(names) == STRSXP) {
                    np += 2;
                    break;
                }
                else names = R_NilValue;
                UNPROTECT(2);
            }
        }
        PROTECT_WITH_INDEX(rowNames = getAttrib(FUN_VALUE,
            is_array ? R_DimNamesSymbol : R_NamesSymbol), &rowNames_index); np++;
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


        GET_FUN_CALL;


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
        dim = PROTECT(allocVector(INTSXP, rank_FUN_VALUE + 1));
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
                dimnames = PROTECT(allocVector(VECSXP, rank_FUN_VALUE + 1));
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


    UNPROTECT(np);
    return value;
}

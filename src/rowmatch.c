#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "defines.h"


SEXP _match_transform(SEXP x, SEXP rho)
{
    /*
     * transform a factor or POSIXlt to a character vector
     */


    if (OBJECT(x)) {
        if (inherits(x, "factor")) {
            SEXP r = asCharacterFactor(x);
            setAttrib(r, R_DimSymbol, getAttrib(x, R_DimSymbol));
            return r;
        }

        else if (inherits(x, "POSIXlt")) {
            SEXP call, r;
            PROTECT(call = lang2(eval(R_AsCharacterSymbol, R_BaseEnv), x));
            r = eval(call, rho);
            UNPROTECT(1);
            return r;
        }

        else {
            SEXP call, r;
            PROTECT(call = lang2(eval(install("mtfrm")   , R_BaseEnv), quoteLang(x)));
            r = eval(call, rho);
            UNPROTECT(1);
            return r;
        }
    }
    /* theoretically, we shouldn't need to duplicate, will revert if needed */
    //return duplicate(x);
    return x;
}


SEXP match_transform(SEXP x, SEXP rho)
{
    /*
     * transform each element of 'x' with _match_transform
     * specifically, do not update 'x', but return a new list
     */


    R_xlen_t i, len = xlength(x);
    SEXP y;


    /* check that each column is a vector */
    for (i = 0; i < len; i++) {
        y = VECTOR_ELT(x, i);
        if (!isVector(y) && !isNull(y))
            error("'row.match.data.frame' requires vector columns");
    }


    /* allocate the new list */
    SEXP value = PROTECT(allocVector(VECSXP, len));


    /* transform each element of 'x' and put into 'value' */
    for (i = 0; i < len; i++)
        SET_VECTOR_ELT(value, i, _match_transform(VECTOR_ELT(x, i), rho));


    UNPROTECT(1);
    return value;
}


int match_type(SEXP x, SEXP table)
{
    /*
     * determine the common type of 'x' and 'table'
     * anything higher than STRSXP will be converted to STRSXP
     */


    if (TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP) return STRSXP;
    else return TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
}


SEXP do_match_type(SEXP x, SEXP table)
{
    /*
     * match_type(x, table) but at the R level,
     * for use in 'rowmatch' at the R level
     */


    return ScalarString(type2str(match_type(x, table)));
}


SEXP as_data_frame(SEXP x, SEXP rho)
{
    SEXP call, r;
    PROTECT(call = lang2(eval(install("as.data.frame"), R_BaseEnv), quoteLang(x)));
    r = eval(call, rho);
    UNPROTECT(1);
    return r;
}


SEXP dim(SEXP x, SEXP rho)
{
    SEXP call, r;
    PROTECT(call = lang2(findVarInFrame(R_BaseEnv, R_DimSymbol), enquote(x)));
    r = eval(call, rho);
    UNPROTECT(1);
    return r;
}


int nrow(SEXP x, SEXP rho)
{
    return INTEGER_ELT(dim(x, rho), 0);
}


SEXP fixup_lengthdim2(SEXP x, int n, SEXP rho)
{
    /*
     * as.character(lapply(seq_len(nrow(x)), function(i) x[i, ]))
     */
    SEXP value = PROTECT(allocVector(VECSXP, n));
    /* x[, ] */
    SEXP call = PROTECT(lang4(
        findVarInFrame(R_BaseEnv, R_BracketSymbol),
        x,
        R_MissingArg,
        R_MissingArg
    ));

    for (int i = 0; i < n; i++) {
        /* x[i, ] */
        SETCADDR(call, ScalarInteger(i + 1));
        SET_VECTOR_ELT(value, i, eval(call, rho));
    }

    value = coerceVector(value, STRSXP);
    UNPROTECT(2);
    return value;
}


SEXP row_split(SEXP x, int n, SEXP rho)
{
    /*
     * essentialy, doing something like this:
     *
     * lapply(seq_len(nrow(x)), function(i) as.list(x[i, ]))
     *
     * just at the C level instead
     */


    int i, j, len = xlength(x);
    SEXP call, value, a;
    PROTECT(call = lang3(
        findVarInFrame(R_BaseEnv, R_Bracket2Symbol),
        R_NilValue,
        R_NilValue
    ));
    PROTECT(value = allocVector(VECSXP, n));

    for (i = 0; i < n; i++) {
        SETCADDR(call, ScalarInteger(i + 1));
        PROTECT(a = allocVector(VECSXP, len));
        for (j = 0; j < len; j++) {
            /* x[[j]][[i]] */
            SETCADR(call, VECTOR_ELT(x, j));
            SET_VECTOR_ELT(a, j, eval(call, rho));
        }
        SET_VECTOR_ELT(value, i, a);
        UNPROTECT(1);
    }

    UNPROTECT(2);
    return value;
}


// row.match(x, table, nomatch, incomparables)
SEXP do_rowmatchdataframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x             = CADR(args),
         table         = CADDR(args),
         nomatch       = CADDDR(args),
         incomparables = CAD4R(args);


    /*
     * xi
     *
     *     element i of 'x'
     *
     * ti
     *
     *     element i of 'table'
     *
     * ii
     *
     *     element i of 'incomparables'
     *
     * expr
     *
     *     the call to base::match
     *
     * value
     *
     *     value returned by evaluating expr
     */
    SEXP xi, ti, ii, expr, value;


    /*
     * the commonType between columns of 'x' and 'table'
     * for example, x[[3L]] is type integer while table[[3L]] is type double,
     * so their common type is double
     */
    SEXPTYPE type;


    /*
     * i
     *
     *     index for looping
     *
     * nrx
     *
     *     number of rows of 'x'
     *
     * nrtable
     *
     *     number of rows of 'table'
     *
     * nrincomparables
     *
     *     number of rows of 'incomparables'
     *
     * nmatch
     *
     *     'nomatch' after being converted to integer
     *
     * fix_xi
     *
     *     does 'xi' need to be converted from a 2D array to a 1D vector?
     *
     * fix_ti
     *
     *     does 'ti' need to be converted from a 2D array to a 1D vector?
     *
     * nprotect
     *
     *     number of protected objects
     *
     * has_incomparables
     *
     *     is incomparables not NULL?
     */
    int i, nrx, nrtable, nrincomparables = 0, nmatch, fix_xi, fix_ti, nprotect = 0,
        has_incomparables;


    /* when 'x' is NULL, simply return integer(0) */
    if (isNull(x)) return allocVector(INTSXP, 0);


    /* convert 'x' to a data frame and get the number of rows */
    PROTECT(x = as_data_frame(x, rho)); nprotect++;
    nrx = nrow(x, rho);


    /* there are zero rows of 'x' */
    if (!nrx) {
        UNPROTECT(nprotect);
        return allocVector(INTSXP, 0);
    }


    nmatch = asInteger(nomatch);


    /* when 'table' is NULL, return a vector full of NA_integer */
    if (isNull(table)) {
        SEXP value = allocVector(INTSXP, nrx);
        int *ivalue = INTEGER0(value);
        for (i = 0; i < nrx; i++) ivalue[i] = nmatch;
        UNPROTECT(nprotect);
        return value;
    }


    /* convert 'table' to a data frame and get the number of rows */
    PROTECT(table = as_data_frame(table, rho)); nprotect++;
    nrtable = nrow(table, rho);


    /* there are zero rows of 'table' or
       there are an unequal number of columns in 'x' and 'table' */
    if (!nrtable || xlength(x) != xlength(table)) {
        SEXP value = allocVector(INTSXP, nrx);
        int *ivalue = INTEGER0(value);
        for (i = 0; i < nrx; i++) ivalue[i] = nmatch;
        UNPROTECT(nprotect);
        return value;
    }


    if (isNull(incomparables)) {}
    /* for compatability with 'match', 'incomparables = FALSE' is equivalent to 'incomparables = NULL' */
    else if (length(incomparables) == 1 && isLogical(incomparables) &&
        LOGICAL_ELT(incomparables, 0) == 0) {
        incomparables = R_NilValue;
    }
    else {
        /* convert 'incomparables' to a data frame and get the number of rows */
        PROTECT(incomparables = as_data_frame(incomparables, rho)); nprotect++;
        nrincomparables = nrow(incomparables, rho);


        /* there are zero rows of 'incomparables' or
           there are an unequal number of columns in 'x' and 'incomparables' */
        if (!nrincomparables || xlength(x) != xlength(incomparables)) {
            incomparables = R_NilValue;
            UNPROTECT(1);
            nprotect--;
        }
    }
    has_incomparables = !isNull(incomparables);


    x     = PROTECT(match_transform(x    , rho)); nprotect++;
    table = PROTECT(match_transform(table, rho)); nprotect++;
    if (has_incomparables) {
        incomparables = PROTECT(match_transform(incomparables, rho)); nprotect++;
    }


    for (i = 0; i < xlength(x); i++) {
        xi = VECTOR_ELT(x, i);
        ti = VECTOR_ELT(table, i);

        type = match_type(xi, ti);

        PROTECT(xi = coerceVector(xi, type));
        PROTECT(ti = coerceVector(ti, type));

        fix_xi = xlength(dim(xi, rho)) == 2;
        fix_ti = xlength(dim(ti, rho)) == 2;
        if (fix_xi || fix_ti)
            type = STRSXP;

        if (fix_xi && fix_ti) {
            SET_VECTOR_ELT(x, i, fixup_lengthdim2(xi, nrx, rho));
            SET_VECTOR_ELT(table, i, fixup_lengthdim2(ti, nrtable, rho));
        }
        else if (fix_xi) {
            SET_VECTOR_ELT(x, i, fixup_lengthdim2(xi, nrx, rho));
            SET_VECTOR_ELT(table, i, coerceVector(ti, type));
        }
        else if (fix_ti) {
            SET_VECTOR_ELT(x, i, coerceVector(xi, type));
            SET_VECTOR_ELT(table, i, fixup_lengthdim2(ti, nrtable, rho));
        }
        else {
            SET_VECTOR_ELT(x, i, xi);
            SET_VECTOR_ELT(table, i, ti);
        }
        UNPROTECT(2);
        if (has_incomparables) {
            ii = VECTOR_ELT(incomparables, i);
            if (xlength(dim(ii, rho)) == 2)
                SET_VECTOR_ELT(incomparables, i, fixup_lengthdim2(ii, nrincomparables, rho));
            else SET_VECTOR_ELT(incomparables, i, coerceVector(ii, type));
        }
    }

    x     = PROTECT(row_split(x    , nrx    , rho)); nprotect++;
    table = PROTECT(row_split(table, nrtable, rho)); nprotect++;
    if (has_incomparables) {
        incomparables = PROTECT(row_split(incomparables, nrincomparables, rho)); nprotect++;
    }

    PROTECT(expr = lang5(findVarInFrame(R_BaseEnv, install("match")), x, table, nomatch, incomparables)); nprotect++;
    value = eval(expr, rho);
    UNPROTECT(nprotect);
    return value;
}

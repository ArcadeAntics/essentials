#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "defines.h"


SEXP _match_transform(SEXP x, SEXP rho)
{
    if (OBJECT(x)) {
        if (inherits(x, "factor")) {
            SEXP r = asCharacterFactor(x);
            setAttrib(r, R_DimSymbol, getAttrib(x, R_DimSymbol));
            return r;
        }
        else if (inherits(x, "POSIXlt")) {
            SEXP call, r;
            PROTECT(call = lang2(R_AsCharacterSymbol, x));
            r = eval(call, rho);
            UNPROTECT(1);
            return r;
        }
    }
    return duplicate(x);
}


SEXP match_transform(SEXP x, SEXP rho)
{
    R_xlen_t i, len = xlength(x);
    SEXP y;

    for (i = 0; i < len; i++) {
        y = VECTOR_ELT(x, i);
        if (!isVector(y) && !isNull(y))
            error("'row.match' requires vector columns");
    }

    SEXP value = PROTECT(allocVector(VECSXP, len));

    for (i = 0; i < len; i++)
        SET_VECTOR_ELT(value, i, _match_transform(VECTOR_ELT(x, i), rho));

    UNPROTECT(1);
    return value;
}


int match_type(SEXP x, SEXP table)
{
    if (TYPEOF(x) >= STRSXP || TYPEOF(table) >= STRSXP) return STRSXP;
    else return TYPEOF(x) < TYPEOF(table) ? TYPEOF(table) : TYPEOF(x);
}


SEXP do_match_type(SEXP x, SEXP table)
{
    return ScalarString(type2str(match_type(x, table)));
}


SEXP as_data_frame(SEXP x, SEXP rho)
{
    SEXP call, r;
    PROTECT(call = lang2(install("as.data.frame"), x));
    r = eval(call, rho);
    UNPROTECT(1);
    return r;
}


SEXP dim(SEXP x, SEXP rho)
{
    SEXP call, r;
    PROTECT(call = lang2(install("dim"), x));
    r = eval(call, rho);
    UNPROTECT(1);
    return r;
}


int nrow(SEXP x, SEXP rho)
{
    return INTEGER(dim(x, rho))[0];
}


SEXP fixup_lengthdim2(SEXP x, int n, SEXP rho)
{
    SEXP value = PROTECT(allocVector(VECSXP, n));
    SEXP call = PROTECT(lang4(R_BracketSymbol, x, ScalarInteger(n), R_MissingArg));

    for (int i = 0; i < n; i++) {
        SETCADDR(call, ScalarInteger(i + 1));
        SET_VECTOR_ELT(value, i, eval(call, rho));
    }

    value = coerceVector(value, STRSXP);
    UNPROTECT(2);
    return value;
}


SEXP row_split(SEXP x, int n, SEXP rho)
{
    int i, j, len = xlength(x);
    SEXP call, value, a;
    PROTECT(call = lang3(R_Bracket2Symbol, R_NilValue, R_NilValue));
    PROTECT(value = allocVector(VECSXP, n));

    for (i = 0; i < n; i++) {
        SETCADDR(call, ScalarInteger(i + 1));
        PROTECT(a = allocVector(VECSXP, len));
        for (j = 0; j < len; j++) {
            SETCADR(call, VECTOR_ELT(x, j));
            SET_VECTOR_ELT(a, j, eval(call, rho));
        }
        SET_VECTOR_ELT(value, i, a);
        UNPROTECT(1);
    }

    UNPROTECT(2);
    return value;
}



SEXP do_rowmatchdataframe(SEXP x, SEXP table, SEXP nomatch, SEXP incomparables, SEXP rho)
{
    SEXP xi, ti, ii, call, value;
    SEXPTYPE type;
    int i, nrx, nrtable, nrincomparables = 0, nmatch, fix_x, fix_t, np = 0,
        has_incomparables;

    if (isNull(x)) return allocVector(INTSXP, 0);

    PROTECT(x = as_data_frame(x, rho)); np++;
    nrx = nrow(x, rho);

    if (!nrx) {
        UNPROTECT(np);
        return allocVector(INTSXP, 0);
    }

    nmatch = asInteger(nomatch);

    if (isNull(table)) {
        SEXP value = PROTECT(allocVector(INTSXP, nrx)); np++;
        int *ivalue = INTEGER(value);
        for (i = 0; i < nrx; i++) ivalue[i] = nmatch;
        UNPROTECT(np);
        return value;
    }

    PROTECT(table = as_data_frame(table, rho)); np++;
    nrtable = nrow(table, rho);

    if (!nrtable || xlength(x) != xlength(table)) {
        SEXP value = PROTECT(allocVector(INTSXP, nrx)); np++;
        int *ivalue = INTEGER(value);
        for (i = 0; i < nrx; i++) ivalue[i] = nmatch;
        UNPROTECT(np);
        return value;
    }

    if (isNull(incomparables)) {}
    else if (length(incomparables) == 1 && isLogical(incomparables) &&
        LOGICAL_ELT(incomparables, 0) == 0) {
        incomparables = R_NilValue;
    }
    else {
        PROTECT(incomparables = as_data_frame(incomparables, rho)); np++;
        nrincomparables = nrow(incomparables, rho);
        if (!nrincomparables || xlength(x) != xlength(incomparables)) {
            incomparables = R_NilValue;
            UNPROTECT(1);
            np--;
        }
    }
    has_incomparables = !isNull(incomparables);

    PROTECT(x = match_transform(x, rho)); np++;
    PROTECT(table = match_transform(table, rho)); np++;
    if (has_incomparables) {
        PROTECT(incomparables = match_transform(incomparables, rho)); np++;
    }

    for (i = 0; i < xlength(x); i++) {
        xi = VECTOR_ELT(x, i);
        ti = VECTOR_ELT(table, i);

        type = match_type(xi, ti);

        PROTECT(xi = coerceVector(xi, type));
        PROTECT(ti = coerceVector(ti, type));

        fix_x = xlength(dim(xi, rho)) == 2;
        fix_t = xlength(dim(ti, rho)) == 2;
        if (fix_x || fix_t)
            type = STRSXP;

        if (fix_x && fix_t) {
            SET_VECTOR_ELT(x, i, fixup_lengthdim2(xi, nrx, rho));
            SET_VECTOR_ELT(table, i, fixup_lengthdim2(ti, nrtable, rho));
        }
        else if (fix_x) {
            SET_VECTOR_ELT(x, i, fixup_lengthdim2(xi, nrx, rho));
            SET_VECTOR_ELT(table, i, coerceVector(ti, type));
        }
        else if (fix_t) {
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

    PROTECT(x = row_split(x, nrx, rho)); np++;
    PROTECT(table = row_split(table, nrtable, rho)); np++;
    if (has_incomparables) {
        PROTECT(incomparables = row_split(incomparables, nrincomparables, rho)); np++;
    }

    PROTECT(call = lang5(install("match"), x, table, nomatch, incomparables)); np++;
    value = eval(call, rho);
    UNPROTECT(np);
    return value;
}

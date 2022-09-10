#include <R.h>
#include <Rinternals.h>





#define enquote(X) (lang2(findVarInFrame(R_BaseEnv, install("quote")), (X)))





R_xlen_t dispatchLength(SEXP x, SEXP rho)
{
    R_xlen_t length_x;
    if (isObject(x)) {
        SEXP expr = PROTECT(lang2(
            findVarInFrame(R_BaseEnv, install("length")),
            enquote(x)
        ));
        expr = PROTECT(eval(expr, rho));
        length_x = (R_xlen_t)
            (TYPEOF(expr) == REALSXP ? REAL(expr)[0] : asInteger(expr));
        UNPROTECT(2);
    }
    else length_x = xlength(x);
    return length_x;
}


SEXP dispatchSubset(SEXP x, R_xlen_t from, R_xlen_t length_out, SEXP rho)
{
    SEXP ans;
    SEXP expr = PROTECT(lang3(
        findVarInFrame(R_BaseEnv, R_BracketSymbol),
        enquote(x),
        length_out ? lang3(
            findVarInFrame(R_BaseEnv, install(":")),
            ScalarReal(from + 1),
            ScalarReal(from + length_out)
        ) : allocVector(INTSXP, 0)
    ));
    ans = eval(expr, rho);
    UNPROTECT(1);
    return ans;
}


SEXP dispatchSubset2(SEXP x, R_xlen_t indx, SEXP rho)
{
    SEXP ans;
    if (!isObject(x)) {
        switch (TYPEOF(x)) {
        case LGLSXP:
            ans = PROTECT(allocVector(LGLSXP, 1));
            LOGICAL(ans)[0] = LOGICAL_ELT(x, indx);
            UNPROTECT(1);
            return ans;
        case INTSXP:
            ans = PROTECT(allocVector(INTSXP, 1));
            INTEGER(ans)[0] = INTEGER_ELT(x, indx);
            UNPROTECT(1);
            return ans;
        case REALSXP:
            ans = PROTECT(allocVector(REALSXP, 1));
            REAL(ans)[0] = REAL_ELT(x, indx);
            UNPROTECT(1);
            return ans;
        case CPLXSXP:
            ans = PROTECT(allocVector(CPLXSXP, 1));
            COMPLEX(ans)[0] = COMPLEX_ELT(x, indx);
            UNPROTECT(1);
            return ans;
        case STRSXP:
            ans = PROTECT(allocVector(STRSXP, 1));
            SET_STRING_ELT(ans, 0, STRING_ELT(x, indx));
            UNPROTECT(1);
            return ans;
        case RAWSXP:
            ans = PROTECT(allocVector(RAWSXP, 1));
            RAW(ans)[0] = RAW_ELT(x, indx);
            UNPROTECT(1);
            return ans;
        }
    }
    SEXP expr = PROTECT(lang3(
        findVarInFrame(R_BaseEnv, R_Bracket2Symbol),
        enquote(x),
        ScalarInteger(indx + 1)
    ));
    ans = eval(expr, rho);
    UNPROTECT(1);
    return ans;
}





SEXP makeSyntaxError(const char *message, SEXP call)
{
    SEXP cond = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(cond, 0, mkString(message));
    SET_VECTOR_ELT(cond, 1, call);


    SEXP names = allocVector(STRSXP, 2);
    setAttrib(cond, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_STRING_ELT(names, 1, mkChar("call"));


    SEXP klass = allocVector(STRSXP, 3);
    setAttrib(cond, R_ClassSymbol, klass);
    SET_STRING_ELT(klass, 0, mkChar("syntaxError"));
    SET_STRING_ELT(klass, 1, mkChar("error"));
    SET_STRING_ELT(klass, 2, mkChar("condition"));

    UNPROTECT(1);

    return cond;
}


void throwSyntaxError(const char *message, SEXP call)
{
    SEXP expr = PROTECT(lang2(
        install("stop"),
        makeSyntaxError(message, call)
    ));
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return;
}





void checkSyntax(SEXP call, SEXP x)
{
    if (x == R_MissingArg)
        return;
    if (TYPEOF(x) == LANGSXP && TYPEOF(CAR(x)) == SYMSXP) {
        if (CAR(x) == install("list")) {
            x = CDR(x);
            int n_starred = 0;
            SEXP x_ptr, xx;
            for (x_ptr = x; x_ptr != R_NilValue; x_ptr = CDR(x_ptr)) {
                xx = CAR(x_ptr);
                if (TYPEOF(xx) == LANGSXP &&
                    TYPEOF(CAR(xx)) == SYMSXP &&
                    CAR(xx) == install("*") &&
                    xlength(xx) <= 2) {
                    if (n_starred) {
                        throwSyntaxError("multiple starred expressions in assignment", call);
                        return;
                    }
                    n_starred++;
                    if (xlength(xx) < 2) {
                    }
                    else {
                        checkSyntax(call, CADR(xx));
                    }
                }
                else {
                    checkSyntax(call, xx);
                }
            }
            return;
        }
        else if (CAR(x) == install("*") && xlength(x) <= 2) {
            throwSyntaxError("can only use starred expression inside list()", call);
            return;
        }
    }
    switch (TYPEOF(x)) {
    case STRSXP:
    case SYMSXP:
    case LANGSXP:
        break;
    default:
        throwSyntaxError("invalid (do_unpackset) left-hand side to assignment", call);
    }
    return;
}


void assignOps(SEXP call, SEXP op, SEXP x, SEXP value, SEXP rho)
{
    if (x == R_MissingArg)
        return;
    SEXP vvalue;
    if (TYPEOF(x) == LANGSXP && TYPEOF(CAR(x)) == SYMSXP) {
        if (CAR(x) == install("list")) {
            x = CDR(x);
            R_xlen_t length_x = xlength(x),
                     length_value = dispatchLength(value, rho),
                     starred_indx = -1,  /* index of the starred expression, if there is one */
                     x_indx;
            SEXP x_ptr, xx;
            /*
             * make one pass through, finding the starred expression, if there is one
             * then check that there are enough values
             *
             * make another pass through, this time actually assigning the values
             */
            for (x_indx = 0, x_ptr = x; x_ptr != R_NilValue; x_indx++, x_ptr = CDR(x_ptr)) {
                xx = CAR(x_ptr);
                if (TYPEOF(xx) == LANGSXP &&
                    TYPEOF(CAR(xx)) == SYMSXP &&
                    CAR(xx) == install("*") &&
                    xlength(xx) <= 2) {
                    if (starred_indx != -1) {
                        throwSyntaxError("multiple starred expressions in assignment\n internal error; should never happen, please report!", call);
                        return;
                    }
                    starred_indx = x_indx;
                }
            }


            /* there is no starred expression */
            if (starred_indx == -1) {
                if (length_value < length_x) {
                    error("not enough values to unpack (expected %.0f, got %.0f)",
                        (double) length_x, (double) length_value);
                    return;
                }
                if (length_value > length_x) {
                    error("too many values to unpack (expected %.0f)",
                        (double) length_x);
                    return;
                }
            }
            else {
                if (length_value < length_x - 1) {
                    error("not enough values to unpack (expected at least %.0f, got %.0f)",
                        (double) length_x - 1, (double) length_value);
                    return;
                }
            }

            R_xlen_t value_indx = 0;

            /* second pass, actually assign the values */
            for (x_indx = 0, x_ptr = x; x_ptr != R_NilValue; x_indx++, x_ptr = CDR(x_ptr)) {
                xx = CAR(x_ptr);
                if (x_indx == starred_indx) {
                    /* how many values will be taken up by the starred expression */
                    R_xlen_t length_out = length_value - length_x + 1;
                    /* consume the values but do no assigning */
                    if (xlength(xx) < 2) {
                    }
                    else {
                        vvalue = PROTECT(dispatchSubset(value, value_indx, length_out, rho));
                        assignOps(call, op, CADR(xx), vvalue, rho);
                        UNPROTECT(1);
                    }
                    /* we consumed 'length_out' values, not necessarily 1 value */
                    value_indx += length_out;
                }
                else {
                    vvalue = PROTECT(dispatchSubset2(value, value_indx, rho));
                    assignOps(call, op, xx, vvalue, rho);
                    UNPROTECT(1);
                    value_indx++;
                }
            }
            return;
        }
        else if (CAR(x) == install("*") && xlength(x) <= 2) {
            throwSyntaxError("can only use starred expression inside list()\n internal error; should never happen, please report!", call);
            return;
        }
    }
    SEXP expr = PROTECT(lang3(
        op,
        x,
        enquote(value)
    ));
    eval(expr, rho);
    UNPROTECT(1);
    return;
}





SEXP do_unpackset(SEXP call, SEXP op, SEXP x, SEXP value, SEXP rho)
{
    if (x == R_MissingArg) {
        error("argument is missing, with no default");
        return R_NilValue;
    }
    checkSyntax(call, x);
    assignOps(call, op, x, value, rho);
    return R_NilValue;
}

#include <R.h>
#include <Rinternals.h>


#include "defines.h"  // for enquote()
#include "translations.h"





extern R_xlen_t dispatchLength(SEXP x, SEXP rho);


SEXP dispatchSubset(SEXP x, R_xlen_t from, R_xlen_t length_out, SEXP env)
{
    static SEXP colonSymbol = NULL;
    if (colonSymbol == NULL) {
        colonSymbol = install(":");
    }


    SEXP expr = PROTECT(lang3(
        findVarInFrame(R_BaseEnv, R_BracketSymbol),
        enquote(x),
        length_out ? lang3(
            findVarInFrame(R_BaseEnv, colonSymbol),
            ScalarReal(from + 1),
            ScalarReal(from + length_out)
        ) : allocVector(INTSXP, 0)
    ));
    SEXP ans = eval(expr, env);
    UNPROTECT(1);
    return ans;
}


SEXP dispatchSubset2(SEXP x, R_xlen_t indx, SEXP env)
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
    ans = eval(expr, env);
    UNPROTECT(1);
    return ans;
}





SEXP makeSyntaxError(const char *message, SEXP rho)
{
    static SEXP sys_callSymbol = NULL;
    if (sys_callSymbol == NULL) {
        sys_callSymbol = install("sys.call");
    }


    SEXP cond = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(cond, 0, mkString(message));
    SET_VECTOR_ELT(cond, 1, eval(lang1(sys_callSymbol), rho));


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


void throwSyntaxError(const char *message, SEXP rho)
{
    static SEXP stopSymbol = NULL;
    if (stopSymbol == NULL) {
        stopSymbol = install("stop");
    }

    SEXP expr = PROTECT(lang2(stopSymbol, makeSyntaxError(message, rho)));
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return;
}





void checksyntax(SEXP x, SEXP rho)
{
    static SEXP listSymbol = NULL,
                asteriskSymbol = NULL;
    if (listSymbol == NULL) {
        listSymbol = install("list");
        asteriskSymbol = install("*");
    }


    if (x == R_MissingArg)
        return;
    if (TYPEOF(x) == LANGSXP && TYPEOF(CAR(x)) == SYMSXP) {
        if (CAR(x) == listSymbol) {
            x = CDR(x);
            int n_starred = 0;
            SEXP x_ptr, xx;
            for (x_ptr = x; x_ptr != R_NilValue; x_ptr = CDR(x_ptr)) {
                xx = CAR(x_ptr);
                if (TYPEOF(xx) == LANGSXP &&
                    CAR(xx) == asteriskSymbol &&
                    xlength(xx) <= 2) {
                    if (n_starred) {
                        throwSyntaxError("multiple starred expressions in assignment", rho);
                        return;
                    }
                    n_starred++;
                    if (xlength(xx) < 2) {
                    }
                    else {
                        checksyntax(CADR(xx), rho);
                    }
                }
                else {
                    checksyntax(xx, rho);
                }
            }
            return;
        }
        else if (CAR(x) == asteriskSymbol && xlength(x) <= 2) {
            throwSyntaxError("can only use starred expression inside list()", rho);
            return;
        }
    }
    switch (TYPEOF(x)) {
    case STRSXP:
    case SYMSXP:
    case LANGSXP:
        break;
    default:
        throwSyntaxError("invalid (do_unpackset) left-hand side to assignment", rho);
    }
    return;
}


void unpackset(SEXP x, SEXP assignfun, SEXP value, SEXP rho, SEXP env)
{
    static SEXP listSymbol = NULL,
                asteriskSymbol = NULL;
    if (listSymbol == NULL) {
        listSymbol = install("list");
        asteriskSymbol = install("*");
    }


    if (x == R_MissingArg)
        return;
    SEXP vvalue;
    if (TYPEOF(x) == LANGSXP && TYPEOF(CAR(x)) == SYMSXP) {
        if (CAR(x) == listSymbol) {
            x = CDR(x);
            R_xlen_t length_x = xlength(x),
                     length_value = dispatchLength(value, env),
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
                    CAR(xx) == asteriskSymbol &&
                    xlength(xx) <= 2) {
                    if (starred_indx != -1) {
                        throwSyntaxError("multiple starred expressions in assignment\n internal error; should never happen, please report!", rho);
                        return;
                    }
                    starred_indx = x_indx;
                }
            }


            /* there is no starred expression */
            if (starred_indx == -1) {
                if (length_value < length_x) {
                    error("not enough values to unpack (expected %lld, got %lld)",
                        (long long int) length_x, (long long int) length_value);
                    return;
                }
                if (length_value > length_x) {
                    error("too many values to unpack (expected %lld)",
                        (long long int) length_x);
                    return;
                }
            }
            else {
                if (length_value < length_x - 1) {
                    error("not enough values to unpack (expected at least %lld, got %lld)",
                        (long long int) (length_x - 1), (long long int) length_value);
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
                        vvalue = PROTECT(dispatchSubset(value, value_indx, length_out, env));
                        unpackset(CADR(xx), assignfun, vvalue, rho, env);
                        UNPROTECT(1);
                    }
                    /* we consumed 'length_out' values, not necessarily 1 value */
                    value_indx += length_out;
                }
                else {
                    vvalue = PROTECT(dispatchSubset2(value, value_indx, env));
                    unpackset(xx, assignfun, vvalue, rho, env);
                    UNPROTECT(1);
                    value_indx++;
                }
            }
            return;
        }
        else if (CAR(x) == asteriskSymbol && xlength(x) <= 2) {
            throwSyntaxError("can only use starred expression inside list()\n internal error; should never happen, please report!", rho);
            return;
        }
    }
    SEXP expr = PROTECT(lang3(assignfun, x, enquote(value)));
    eval(expr, env);
    UNPROTECT(1);
    return;
}





SEXP do_unpackset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP parent_frameSymbol = NULL;
    if (parent_frameSymbol == NULL)
        parent_frameSymbol = install("parent.frame");


    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP assignfun = CAR(args); args = CDR(args);
    SEXP value = CAR(args); args = CDR(args);


    if (x == R_MissingArg)
        error(_("argument is missing, with no default"));
    checksyntax(x, rho);
    unpackset(x, assignfun, value, rho, PROTECT(eval(lang1(parent_frameSymbol), rho)));


    set_R_Visible(FALSE);
    return value;
}

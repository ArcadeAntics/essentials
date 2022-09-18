#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include "defines.h"





#define Int2Real(X) ((X == NA_INTEGER) ? NA_REAL : (double) X)


/* version of 'hypot' which should return NA_REAL where appropriate */
double hypot2(double x, double y)
{
    double value = hypot(x, y);
    if (ISNAN(value) && (ISNA(x) || ISNA(y))) return NA_REAL;
    return value;
}


/* hypotenuse of a complex number */
#define chypot1(X) hypot2(X.r, X.i)


/* hypotenuse of a real and complex number */
#define chypot2(X, Y) hypot2(X, chypot1(Y))





/* phypot(..., na.rm = FALSE) */
SEXP do_phypot(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    R_xlen_t i, n, len = 1;
    int na_rm, nprotect = 0;


    // when using the R function '.External' to call a C function,
    // the 'ExternalRoutine' object (argument '.NAME' of '.External')
    // is included in 'args'
    // use 'CDR' to remove this
    args = CDR(args);


    // 'na.rm' is the first value in 'args', use 'CAR' to select it
    na_rm = asLogical(CAR(args));


    SEXP dots = findVarInFrame(rho, install("..."));
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    int dots_length = dotsLength(dots);


    // if there are no arguments, return numeric(0)
    if (dots_length == 0) return allocVector(REALSXP, 0);


    args = PROTECT(allocVector(LISTSXP, dots_length)); nprotect++;
    SEXP a = args,
         d = dots;


    // 'n' is the length of the current vector
    // 'len' will be the length of the resultant vector
    for (; d != R_NilValue; a = CDR(a), d = CDR(d)) {
        x = CAR(d);
        x = eval(x, rho);
        SETCAR(a, x);
        switch(TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
        case NILSXP:
        case REALSXP:
        case CPLXSXP:
            break;
        default:
            error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
            return R_NilValue;
        }
        if (len) {
            n = xlength(x);
            if (n == 0 || n > len)
                len = n;
        }
    }


    eval(R_NilValue, R_BaseEnv);  /* in eval(x, rho), argument might set R_Visible to FALSE, change it back to TRUE */


    if (len == 0) {
        UNPROTECT(nprotect);
        return allocVector(REALSXP, 0);
    }


    for (a = args; a != R_NilValue; a = CDR(a)) {
        if (len % xlength(CAR(a))) {
            warning("an argument will be fractionally recycled");
            break;
        }
    }


    SEXP value = PROTECT(allocVector(REALSXP, len)); nprotect++;
    double *rvalue = REAL(value);
    Memzero(rvalue, len);


    for (a = args; a != R_NilValue; a = CDR(a)) {
        x = CAR(a);
        n = xlength(x);
        switch(TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
        {
            int *ix = INTEGER(x);
            if (n == 1) { // 'x' is a scalar
                int x1 = ix[0];

                // if 'x' is 0 or would be removed by 'na.rm'
                if (x1 == 0 || (na_rm && x1 == NA_INTEGER))
                    continue;

                double rx1 = Int2Real(x1);
                for (i = 0; i < len; i++) {
                    rvalue[i] = hypot2(rvalue[i], rx1);
                }
            }
            else if (n == len) {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (ix[i] != NA_INTEGER)
                            rvalue[i] = hypot2(rvalue[i], Int2Real(ix[i]));
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = hypot2(rvalue[i], Int2Real(ix[i]));
                    }
                }
            }
            else {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (ix[i % len] != NA_INTEGER)
                            rvalue[i] = hypot2(rvalue[i], Int2Real(ix[i % len]));
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = hypot2(rvalue[i], Int2Real(ix[i % len]));
                    }
                }
            }
        }
            break;
        case REALSXP:
        {
            double *rx = REAL(x);
            if (n == 1) { // 'x' is a scalar
                double x1 = rx[0];

                // if 'x' is 0 or would be removed by 'na.rm'
                if (x1 == 0 || (na_rm && ISNAN(x1)))
                    continue;

                for (i = 0; i < len; i++) {
                    rvalue[i] = hypot2(rvalue[i], x1);
                }
            }
            else if (n == len) {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (!ISNAN(rx[i]))
                            rvalue[i] = hypot2(rvalue[i], rx[i]);
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = hypot2(rvalue[i], rx[i]);
                    }
                }
            }
            else {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (!ISNAN(rx[i % len]))
                            rvalue[i] = hypot2(rvalue[i], rx[i % len]);
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = hypot2(rvalue[i], rx[i % len]);
                    }
                }
            }
        }
            break;
        case CPLXSXP:
        {
            Rcomplex *cx = COMPLEX(x);
            if (n == 1) { // 'x' is a scalar
                Rcomplex x1 = cx[0];
                double rx1 = chypot1(x1);

                // if 'x' is 0 or would be removed by 'na.rm'
                if (rx1 == 0 || (na_rm && cISNAN(x1)))
                    continue;

                for (i = 0; i < len; i++) {
                    rvalue[i] = hypot2(rvalue[i], rx1);
                }
            }
            else if (n == len) {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (!cISNAN(cx[i]))
                            rvalue[i] = chypot2(rvalue[i], cx[i]);
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = chypot2(rvalue[i], cx[i]);
                    }
                }
            }
            else {
                if (na_rm) {
                    for (i = 0; i < len; i++) {
                        if (!cISNAN(cx[i % len]))
                            rvalue[i] = chypot2(rvalue[i], cx[i % len]);
                    }
                }
                else {
                    for (i = 0; i < len; i++) {
                        rvalue[i] = chypot2(rvalue[i], cx[i % len]);
                    }
                }
            }
        }
            break;
        default:
            error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
            return R_NilValue;
        }
    }

    // copy 'dim', 'dimnames' and 'names' attributes from appropriate arguments
    for (a = args; a != R_NilValue; a = CDR(a)) {

        x = CAR(a);
        // the argument must be the same length as 'value' in
        // order to copy 'dim', 'dimnames' and 'names' attributes
        if (xlength(x) == len) {

            // if 'value' does not have a 'dim' attribute
            if (getAttrib(value, R_DimSymbol) == R_NilValue) {

                // if 'value' does not have a 'dim' attribute
                // and 'x' has a 'dim' attribute
                if (getAttrib(x, R_DimSymbol) != R_NilValue) {

                    // set the 'dim' of 'value' to the 'dim' of 'x'
                    // remove the 'names' of 'value'
                    setAttrib(value, R_DimSymbol, getAttrib(x, R_DimSymbol));
                    setAttrib(value, R_NamesSymbol, R_NilValue);

                    // if 'x' also has a 'dimnames' attribute,
                    // copy them and immediately return 'value'
                    if (getAttrib(x, R_DimNamesSymbol) != R_NilValue) {
                        setAttrib(value, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
                        UNPROTECT(nprotect);
                        return value;
                    }
                }

                // if 'value' and 'x' do not have a 'dim' attribute
                // and 'value' does not have a 'names' attribute
                // and 'x' has a 'names' attribute
                else if (getAttrib(value, R_NamesSymbol) == R_NilValue &&
                    getAttrib(x, R_NamesSymbol) != R_NilValue) {
                    setAttrib(value, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
                }
            }

            // if 'value' has a 'dim' attribute
            // and 'x' is conformable to 'value' (equivalent 'dim' attribute)
            else if (conformable(value, x)) {

                // if 'x' has a 'dimnames' attribute,
                // copy them and immediately return 'value'
                if (getAttrib(x, R_DimNamesSymbol) != R_NilValue) {
                    setAttrib(value, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
                    UNPROTECT(nprotect);
                    return value;
                }
            }
        }
    }

    UNPROTECT(nprotect);
    return value;
}


/* hypot(..., na.rm = FALSE) */
SEXP do_hypot(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x;
    R_xlen_t i, n;
    int na_rm, nprotect = 0;


    args = CDR(args);
    na_rm = asLogical(CAR(args));


    SEXP dots = findVarInFrame(rho, install("..."));
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    int dots_length = dotsLength(dots);


    // if there are no arguments, return 0
    if (dots_length == 0) return ScalarReal(0.0);


    args = PROTECT(allocVector(LISTSXP, dots_length)); nprotect++;
    SEXP a = args,
         d = dots;


    for (; d != R_NilValue; a = CDR(a), d = CDR(d)) {
        x = CAR(d);
        x = eval(x, rho);
        SETCAR(a, x);
        switch(TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
        case NILSXP:
        case REALSXP:
        case CPLXSXP:
            break;
        default:
            error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
            return R_NilValue;
        }
    }


    eval(R_NilValue, R_BaseEnv);  /* in eval(x, rho), argument might set R_Visible to FALSE, change it back to TRUE */


    double value = 0.0;
    for (a = args; a != R_NilValue; a = CDR(a)) {
        x = CAR(a);
        n = xlength(x);

        switch(TYPEOF(x)) {
        case INTSXP:
        case LGLSXP:
        {
            int *ix = INTEGER(x);
            if (na_rm) {
                for (i = 0; i < n; i++) {
                    if (ix[i] != NA_INTEGER)
                        value = hypot2(value, Int2Real(ix[i]));
                }
            }
            else {
                for (i = 0; i < n; i++) {
                    value = hypot2(value, Int2Real(ix[i]));
                }
            }
        }
            break;
        case NILSXP:
            break;
        case REALSXP:
        {
            double *rx = REAL(x);
            if (na_rm) {
                for (i = 0; i < n; i++) {
                    if (!ISNAN(rx[i]))
                        value = hypot2(value, rx[i]);
                }
            }
            else {
                for (i = 0; i < n; i++) {
                    value = hypot2(value, rx[i]);
                }
            }
        }
            break;
        case CPLXSXP:
        {
            Rcomplex *cx = COMPLEX(x);
            if (na_rm) {
                for (i = 0; i < n; i++) {
                    if (!cISNAN(cx[i]))
                        value = chypot2(value, cx[i]);
                }
            }
            else {
                for (i = 0; i < n; i++) {
                    value = chypot2(value, cx[i]);
                }
            }
        }
            break;
        default:
            error("invalid 'type' (%s) of argument", type2char(TYPEOF(x)));
        }
    }
    UNPROTECT(nprotect);
    return ScalarReal(value);
}





/* Inverse Distance Weighting */





#define subtract(X, Y) ((ISNA(X) || ISNA(Y)) ? NA_REAL : (X - Y))
#define min(X, Y)      ((ISNA(X) || ISNA(Y)) ? NA_REAL : fmin2(X, Y))





SEXP do_IDW(SEXP x0, SEXP u0, SEXP x, SEXP p)
{
    test4NumericArgument(x0);
    test4NumericArgument(u0);
    test4NumericArgument(x);

    double pp = asReal(p);

    if (xlength(p) > 1)
        warning("first element used of '%s' argument", "p");

    if (ISNAN(pp) || pp <= 0)
        error("'%s' must be positive", "p");

    int i, j, k, nr_x0, nc_x0, nr_x, nc_x, nu = xlength(u0);
    SEXP value;

    if (isNull(x0)) {
        nr_x0 = 0;
        nc_x0 = 0;
    }
    else if (isMatrix(x0)) {
        nr_x0 = INTEGER(getAttrib(x0, R_DimSymbol))[0];
        nc_x0 = INTEGER(getAttrib(x0, R_DimSymbol))[1];
    }
    else error("invalid 'x0'");

    if (isNull(x)) {
        nr_x = 0;
        nc_x = nc_x0;
    }
    else if (isMatrix(x)) {
        nr_x = INTEGER(getAttrib(x, R_DimSymbol))[0];
        nc_x = INTEGER(getAttrib(x, R_DimSymbol))[1];
    }
    else error("invalid 'x'");

    if (nc_x0 != nc_x) error("arguments implying differing dimensions");
    if (nr_x0 != nu)   error("wrong length of 'u0'; should never happen, please report!");

    if (nr_x == 0) return allocVector(REALSXP, 0);
    if (nr_x0 == 0 || nu == 0) {
        value = PROTECT(allocVector(REALSXP, nr_x));
        double *rvalue = REAL(value);
        for (i = 0; i < nr_x; i++) rvalue[i] = NA_REAL;
        UNPROTECT(1);
        return value;
    }

    value = PROTECT(allocVector(REALSXP, nr_x));
    double *rvalue = REAL(value);
    Memzero(rvalue, nr_x);

    x0 = PROTECT(coerceVector(x0, REALSXP));
    u0 = PROTECT(coerceVector(u0, REALSXP));
    x  = PROTECT(coerceVector(x , REALSXP));
    double *rx0 = REAL(x0), *ru0 = REAL(u0), *rx = REAL(x), tmp_x[nc_x], tmp_x0[nc_x0];

    SEXP tmp = eval(lang2(install("mean"), u0), R_GlobalEnv), tmp2;
    if (!isReal(tmp) || xlength(tmp) != 1)
        error("mean(u0) should be a numeric vector of length 1");
    double mean_u0 = asReal(tmp), *rtmp2;

    double distances[nr_x0], dmin, dsum;
    int num_0_distances;

    // for each row in 'x'
    for (i = 0; i < nr_x; i++) {

        // reset the variables
        num_0_distances = 0;
        dmin = R_PosInf;
        dsum = 0;

        // select the coordinates of the i-th row of 'x'
        for (j = 0; j < nc_x; j++) {
            tmp_x[j] = rx[i + j * nr_x];
        }

        // for each row in 'x0'
        for (j = 0; j < nr_x0; j++) {

            // calculate the differences between coordinates
            // for i-th row of 'x' and j-th row of 'x0'
            for (k = 0; k < nc_x0; k++) {
                tmp_x0[k] = subtract(rx0[j + k * nr_x0], tmp_x[k]);
            }

            // calculate the distance between the
            // i-th row of 'x' and the j-th row of 'x0'
            distances[j] = fabs(tmp_x0[0]);
            for (k = 1; k < nc_x0; k++) {
                distances[j] = hypot2(distances[j], tmp_x0[k]);
            }

            // find the minimum distance (excluding NA)
            if (distances[j] == 0)
                num_0_distances++;
            else dmin = min(dmin, distances[j]);
        }

        // if the minimum distance between the i-th row of 'x' and
        // all the coordinates in 'x0' is 0, the value at the i-th position
        // should be the mean of 'u0' corresponding to those 0 distances
        if (num_0_distances > 0) {
            tmp2 = PROTECT(allocVector(REALSXP, num_0_distances));
            rtmp2 = REAL(tmp2);
            k = 0;
            for (j = 0; j < nr_x0; j++) {
                if (distances[j] == 0) {
                    rtmp2[k] = ru0[j];
                    k++;
                }
            }
            rvalue[i] = asReal(eval(lang2(install("mean"), tmp2), R_GlobalEnv));
            UNPROTECT(1);
            continue;
        }

        // if the closest coordinate to the i-th row of 'x' is infinitely far away,
        // set the i-th value to the mean of the values
        if (dmin == R_PosInf) {
            rvalue[i] = mean_u0;
            continue;
        }

        // if the minimum distance is unknown, return NA_real_ or NaN
        if (ISNAN(dmin) || ISNAN(mean_u0)) {
            rvalue[i] = subtract(dmin, mean_u0);
            continue;
        }

        // calculate the weightings and the total weight 'dsum'
        for (j = 0; j < nr_x0; j++) {
            distances[j] = 1/pow(distances[j]/dmin, pp);
            dsum += distances[j];
        }

        // if the total weight is 0, set the i-th value to the mean of the values
        if (dsum == 0) {
            rvalue[i] = mean_u0;
            continue;
        }

        for (j = 0; j < nr_x0; j++) {
            rvalue[i] += distances[j]/dsum * ru0[j];
        }
    }

    UNPROTECT(4);
    return value;
}

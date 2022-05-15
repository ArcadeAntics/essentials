#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


#define give_log log_p
#define R_D__0 (log_p ? R_NegInf : 0.0)
#define R_D__1 (log_p ? 0.0 : 1.0)
#define R_D__e (log_p ? -1.0 : exp(-1.0))
#define R_DT_0 (lower_tail ? R_D__0 : R_D__1)
#define R_DT_1 (lower_tail ? R_D__1 : R_D__0)


#define test4NumericArgument(X)                                \
    switch(TYPEOF(X)) {                                        \
    case INTSXP:                                               \
    case LGLSXP:                                               \
    case NILSXP:                                               \
    case REALSXP:                                              \
        break;                                                 \
    default:                                                   \
        error("Non-numeric argument to mathematical function");\
    }


#define ML_NAN (0.0 / 0.0)


#define my_log1mexp(X) (((X) < -0.56714329040978384) ? log1p(-exp(X)) : log(-expm1(X)))


double dgev5(double x, double mu, double sigma, double xi,
    int give_log)
{
    if (ISNA(x) || ISNA(mu) || ISNA(sigma) || ISNA(xi))
        return NA_REAL;
    if (R_IsNaN(x) || R_IsNaN(mu) || R_IsNaN(sigma) || R_IsNaN(xi))
        return R_NaN;
    if (sigma < 0) return ML_NAN;
    if (!R_FINITE(sigma)) return R_D__0;
    if (!R_FINITE(x) && mu == x) return ML_NAN;
    if (sigma == 0)
        return (x == mu) ? R_PosInf : R_D__0;
    x = (x - mu) / sigma;


    if (!R_FINITE(x)) return R_D__0;
    if (!R_FINITE(xi)) return (x == 0) ? R_D__e : R_D__0;


    if (xi == 0) return (give_log ? -x - exp(-x) : exp(-x - exp(-x)))/sigma;


    double a = xi * x;
    if (a <= -1) return R_D__0;


    a++;
    double b = -1/xi;
    return (give_log ? ((-1 + b) * log(a) - pow(a, b)) :
        (pow(a, -1 + b) * exp(-pow(a, b)))) / sigma;
}


void pgev_both(double x, double xi, double *cum, double *ccum,
    int i_tail, int log_p)
{
    int lower, upper;


    if (   ISNA(x) ||    ISNA(xi)) { *cum = *ccum = NA_REAL; return; }
    if (R_IsNaN(x) || R_IsNaN(xi)) { *cum = *ccum = R_NaN  ; return; }


    lower = i_tail != 1;
    upper = i_tail != 0;


    if (!R_FINITE(xi)) {
        if (lower)  *cum = (x > 0.0) ? R_D__1 : R_D__0;
        if (upper) *ccum = (x > 0.0) ? R_D__0 : R_D__1;
    }
    else if (xi == 0) {
        if (lower)  *cum = log_p ? -exp(-x)              : exp(-exp(-x))   ;
        if (upper) *ccum = log_p ? my_log1mexp(-exp(-x)) : -expm1(-exp(-x));
    }
    else {
        double a = xi * x;
        if (a <= -1) {
            if (lower)  *cum = (xi > 0.0) ? R_D__0 : R_D__1;
            if (upper) *ccum = (xi > 0.0) ? R_D__1 : R_D__0;
        }
        else {
            a++;
            double b = -1/xi;
            if (lower)  *cum = log_p ? -pow(a, b)              : exp(-pow(a, b))   ;
            if (upper) *ccum = log_p ? my_log1mexp(-pow(a, b)) : -expm1(-pow(a, b));
        }
    }
}


double pgev6(double x, double mu, double sigma, double xi,
    int lower_tail, int log_p)
{
    double p, cp;


    if (ISNA(x) || ISNA(mu) || ISNA(sigma) || ISNA(xi))
        return NA_REAL;
    if (R_IsNaN(x) || R_IsNaN(mu) || R_IsNaN(sigma) || R_IsNaN(xi))
        return R_NaN;
    if (!R_FINITE(x) && mu == x) return ML_NAN;
    if (sigma <= 0) {
        if (sigma < 0) return ML_NAN;
        return (x < mu) ? R_DT_0 : R_DT_1;
    }
    p = (x - mu) / sigma;
    if (!R_FINITE(p))
        return (x < mu) ? R_DT_0 : R_DT_1;
    x = p;


    pgev_both(x, xi, &p, &cp, (lower_tail ? 0 : 1), log_p);


    return(lower_tail ? p : cp);
}


double qgev6(double p, double mu, double sigma, double xi,
    int lower_tail, int log_p)
{
    if (ISNA(p) || ISNA(mu) || ISNA(sigma) || ISNA(xi))
        return NA_REAL;
    if (R_IsNaN(p) || R_IsNaN(mu) || R_IsNaN(sigma) || R_IsNaN(xi))
        return R_NaN;


    if (sigma < 0) return ML_NAN;
    if (sigma == 0) return mu;
    if (!R_FINITE(xi)) return mu;


    if (!log_p) p = log(p);
    if (p > 0) return ML_NAN;


    if (xi == 0)
        return lower_tail ? (mu - sigma * log(-p)) : (1 - mu + sigma * log(-p));


    return lower_tail ? (mu + sigma/xi * (pow(-p, -xi) - 1)) :
        (1 - mu - sigma/xi * (pow(-p, -xi) - 1));
}


SEXP dgev(SEXP x, SEXP mu, SEXP sigma, SEXP xi, SEXP give_log)
{
    test4NumericArgument(x);
    test4NumericArgument(mu);
    test4NumericArgument(sigma);
    test4NumericArgument(xi);

    R_xlen_t lenx = xlength(x), lenmu = xlength(mu),
        lensigma = xlength(sigma), lenxi = xlength(xi);

    if (lenx == 0 || lenmu == 0 || lensigma == 0 || lenxi == 0)
        return allocVector(REALSXP, 0);

    x     = PROTECT(coerceVector(x    , REALSXP));
    mu    = PROTECT(coerceVector(mu   , REALSXP));
    sigma = PROTECT(coerceVector(sigma, REALSXP));
    xi    = PROTECT(coerceVector(xi   , REALSXP));

    int ilog = asLogical(give_log);

    R_xlen_t len = lenx;
    len = fmax(len, lenmu);
    len = fmax(len, lensigma);
    len = fmax(len, lenxi);

    SEXP value = PROTECT(allocVector(REALSXP, len));
    double *rx = REAL(x), *rmu = REAL(mu), *rsigma = REAL(sigma),
        *rxi = REAL(xi), *rvalue = REAL(value);

    for (R_xlen_t i = 0; i < len; i++)
        rvalue[i] = dgev5(rx[i % lenx], rmu[i % lenmu],
            rsigma[i % lensigma], rxi[i % lenxi], ilog);

    UNPROTECT(5);
    return value;
}


SEXP pgev(SEXP q, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p)
{
    test4NumericArgument(q);
    test4NumericArgument(mu);
    test4NumericArgument(sigma);
    test4NumericArgument(xi);

    R_xlen_t lenq = xlength(q), lenmu = xlength(mu),
        lensigma = xlength(sigma), lenxi = xlength(xi);

    if (lenq == 0 || lenmu == 0 || lensigma == 0 || lenxi == 0)
        return(allocVector(REALSXP, 0));

    int ilog_p = asLogical(log_p), ilower_tail = asLogical(lower_tail);

    R_xlen_t len = lenq;
    len = fmax(len, lenmu);
    len = fmax(len, lensigma);
    len = fmax(len, lenxi);

    q     = PROTECT(coerceVector(q    , REALSXP));
    mu    = PROTECT(coerceVector(mu   , REALSXP));
    sigma = PROTECT(coerceVector(sigma, REALSXP));
    xi    = PROTECT(coerceVector(xi   , REALSXP));
    SEXP value = PROTECT(allocVector(REALSXP, len));
    double *rq = REAL(q), *rmu = REAL(mu), *rsigma = REAL(sigma),
        *rxi = REAL(xi), *rvalue = REAL(value);

    for (R_xlen_t i = 0; i < len; i++)
        rvalue[i] = pgev6(rq[i % lenq], rmu[i % lenmu],
            rsigma[i % lensigma], rxi[i % lenxi], ilower_tail, ilog_p);

    UNPROTECT(5);
    return value;
}


SEXP qgev(SEXP p, SEXP mu, SEXP sigma, SEXP xi, SEXP lower_tail, SEXP log_p)
{
    test4NumericArgument(p);
    test4NumericArgument(mu);
    test4NumericArgument(sigma);
    test4NumericArgument(xi);

    R_xlen_t lenp = xlength(p), lenmu = xlength(mu),
        lensigma = xlength(sigma), lenxi = xlength(xi);

    if (lenp == 0 || lenmu == 0 || lensigma == 0 || lenxi == 0)
        return(allocVector(REALSXP, 0));

    p     = PROTECT(coerceVector(p    , REALSXP));
    mu    = PROTECT(coerceVector(mu   , REALSXP));
    sigma = PROTECT(coerceVector(sigma, REALSXP));
    xi    = PROTECT(coerceVector(xi   , REALSXP));

    int ilog_p = asLogical(log_p), ilower_tail = asLogical(lower_tail);

    R_xlen_t len = lenp;
    len = fmax(len, lenmu);
    len = fmax(len, lensigma);
    len = fmax(len, lenxi);

    SEXP value = PROTECT(allocVector(REALSXP, len));
    double *rp = REAL(p), *rmu = REAL(mu), *rsigma = REAL(sigma),
        *rxi = REAL(xi), *rvalue = REAL(value);

    for (R_xlen_t i = 0; i < len; i++)
        rvalue[i] = qgev6(rp[i % lenp], rmu[i % lenmu],
            rsigma[i % lensigma], rxi[i % lenxi], ilower_tail, ilog_p);

    UNPROTECT(5);
    return value;
}


SEXP rgev(SEXP n, SEXP mu, SEXP sigma, SEXP xi)
{
    test4NumericArgument(mu);
    test4NumericArgument(sigma);
    test4NumericArgument(xi);

    R_xlen_t lenn = xlength(n);
    if (lenn == 1) lenn = asInteger(n);
    if (lenn == NA_INTEGER || lenn < 0) error("invalid arguments");
    if (lenn == 0) return(allocVector(REALSXP, 0));

    SEXP value = PROTECT(allocVector(REALSXP, lenn));
    double *rvalue = REAL(value);

    R_xlen_t i, lenmu = xlength(mu), lensigma = xlength(sigma),
        lenxi = xlength(xi);
    if (lenmu == 0 || lensigma == 0 || lenxi == 0) {
        warning("NAs produced");
        for (i = 0; i < lenn; i++) rvalue[i] = NA_REAL;
        UNPROTECT(1);
        return value;
    }

    mu    = PROTECT(coerceVector(mu   , REALSXP));
    sigma = PROTECT(coerceVector(sigma, REALSXP));
    xi    = PROTECT(coerceVector(xi   , REALSXP));
    double *rmu = REAL(mu), *rsigma = REAL(sigma), *rxi = REAL(xi);

    for (i = 0; i < lenn; i++)
        rvalue[i] = qgev6(runif(0.0, 1.0), rmu[i % lenmu],
            rsigma[i % lensigma], rxi[i % lenxi], 1, 0);

    UNPROTECT(4);
    return value;
}

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>





SEXP do_colon(SEXP s1, SEXP s2)
{
    PROTECT(s1);
    PROTECT(s2);
    SEXP colonSymbol = PROTECT(install(":"));
    SEXP expr = PROTECT(lang3(colonSymbol, s1, s2));
    SEXP value = PROTECT(eval(expr, R_BaseEnv));
    UNPROTECT(5);
    return value;
}


SEXP do_seq(SEXP args, SEXP missings)
{
    SEXP value;
    R_xlen_t i, lout = NA_INTEGER;


    if (TYPEOF(args) != VECSXP || xlength(args) != 6)
        error("invalid 'args'");
    if (TYPEOF(missings) != LGLSXP || xlength(missings) != 5)
        error("invalid 'missings'");


    SEXP
        from       = VECTOR_ELT(args, 0),
        to         = VECTOR_ELT(args, 1),
        by         = VECTOR_ELT(args, 2),
        length_out = VECTOR_ELT(args, 3),
        along_with = VECTOR_ELT(args, 4);
    Rboolean
        endpoint   = asLogical(VECTOR_ELT(args, 5));


    if (endpoint == NA_LOGICAL)
        error("invalid 'endpoint' argument");


    Rboolean
        missing_from       = LOGICAL(missings)[0],
        missing_to         = LOGICAL(missings)[1],
        missing_by         = LOGICAL(missings)[2],
        missing_length_out = LOGICAL(missings)[3],
        missing_along_with = LOGICAL(missings)[4];


    if (missing_from       == NA_LOGICAL ||
        missing_to         == NA_LOGICAL ||
        missing_by         == NA_LOGICAL ||
        missing_length_out == NA_LOGICAL ||
        missing_along_with == NA_LOGICAL)
        error("invalid 'missings', contains NA");


    if (!missing_length_out && !missing_along_with)
        error("only one of 'length.out' and 'along.with' can be used");


    Rboolean One = ((5 - missing_from - missing_to - missing_by - missing_length_out - missing_along_with) == 1);


    if (One && !missing_from) {
        R_xlen_t lf = xlength(from);
        if (lf == 1 && (TYPEOF(from) == INTSXP || TYPEOF(from) == REALSXP)) {
            double rfrom = asReal(from);
            if (!R_FINITE(rfrom))
                error("'from' must be a finite number");
            if ((rfrom >= 1 && endpoint) || (rfrom >= 2))
                return do_colon(ScalarReal(1.0), ScalarReal(
                    rfrom
                    /*
                    endpoint ? rfrom : (rfrom - 1)
                     */
                ));
            else return allocVector(INTSXP, 0);
        }
        else if (lf >= 1)
            return do_colon(ScalarReal(1.0), ScalarReal(lf));
        else return allocVector(INTSXP, 0);
    }
    if (!missing_along_with) {
        lout = xlength(along_with);
        if (One) {
            if (lout >= 1)
                return do_colon(ScalarReal(1.0), ScalarReal(lout));
            else return allocVector(INTSXP, 0);
        }
    }
    else if (!missing_length_out) {
        double rout = asReal(length_out);
        if (ISNAN(rout))
            error("invalid 'length.out', must not be NA");
        if (xlength(length_out) != 1)
            warning("first element used of 'length.out' argument");
        lout = (length_out < 0) ? 0 : ((R_xlen_t) ceil(rout));
    }


    if (lout == NA_INTEGER) {
        double rfrom, rto;
        if (missing_from) rfrom = 1.0;
        else {
            if (xlength(from) != 1) error("'from' must be of length 1");
            rfrom = asReal(from);
            if (!R_FINITE(rfrom))
                error("'from' must be a finite number");
        }
        if (missing_to) rto = 1.0;
        else {
            if (xlength(to) != 1) error("'to' must be of length 1");
            rto = asReal(to);
            if (!R_FINITE(rto))
                error("'to' must be a finite number");
        }
        if (missing_by)
            return do_colon(ScalarReal(rfrom), ScalarReal(
                endpoint ? rto : ( (rto >= rfrom) ? (rto - 1) : (rto + 1) )
            ));
        if (xlength(by) != 1) error("'by' must be of length 1");
        double del = rto - rfrom;
        if (del == 0.0 && rto == 0.0) {
            return endpoint ? to : allocVector(INTSXP, 0);
        }
        double n, rby = asReal(by);
        Rboolean finite_del = R_FINITE(del);
        if (finite_del)
            n = del/rby;
        else n = rto/rby - rfrom/rby;
        if (!R_FINITE(n)) {
            if (del == 0.0 && rby == 0.0) {
                return endpoint ?
                    (missing_from ? ScalarReal(rfrom) : from) :
                    allocVector(INTSXP, 0);
            }
            else error("invalid '(to - from)/by)'");
        }
        if (finite_del && fabs(del)/fmax2(fabs(rto), fabs(rfrom)) < 100 * DBL_EPSILON) {
            if (!endpoint && del == 0.0)
                return allocVector(INTSXP, 0);
            return missing_from ? ScalarReal(rfrom) : from;
            /*
            return endpoint ?
                (missing_from ? ScalarReal(rfrom) : from) :
                allocVector(INTSXP, 0);
             */
        }
        R_xlen_t nn;
        double final;
        if ((missing_from || TYPEOF(from) == INTSXP) &&
            (missing_to   || TYPEOF(to)   == INTSXP) &&
            TYPEOF(by) == INTSXP) {
            int *ivalue, ifrom = missing_from ? ((int) rfrom) : asInteger(from),
                iby = asInteger(by);


            nn = (R_xlen_t) n;
            if (!endpoint) {
                final = (double)((int)(ifrom + nn * iby));
                if ((iby > 0 && final >= rto) ||
                    (iby < 0 && final <= rto))
                    nn -= 1;
            }
            if (nn < 0)
                return allocVector(INTSXP, 0);
#ifdef LONG_VECTOR_SUPPORT
            if (nn > (R_xlen_t) R_XLEN_T_MAX)
#else
            if (nn > (R_xlen_t) INT_MAX)
#endif
                error("'by' argument is much too small");
            value = allocVector(INTSXP, nn + 1);
            ivalue = INTEGER(value);
            for (i = 0; i <= nn; i++)
                ivalue[i] = (int) (ifrom + i * iby);
            return value;
        }


        nn = (R_xlen_t) (n + 1e-10);
        if (!endpoint) {
            final = finite_del ?
                (rfrom + (double)nn * rby) :
                ldexp(rfrom/4.0 + (double)nn * rby/4.0, 2);
            if ((rby > 0 && final >= rto) ||
                (rby < 0 && final <= rto))
                nn -= 1;
        }
        if (nn < 0)
            return allocVector(INTSXP, 0);
#ifdef LONG_VECTOR_SUPPORT
        if (nn > (R_xlen_t) R_XLEN_T_MAX)
#else
        if (nn > (R_xlen_t) INT_MAX)
#endif
            error("'by' argument is much too small");
        value = allocVector(REALSXP, nn + 1);
        double *rvalue = REAL(value);
        if (finite_del)
            for (i = 0; i <= nn; i++)
                rvalue[i] = rfrom + (double)i * rby;
        else {
            rfrom /= 4.0;
            rby   /= 4.0;
            for (i = 0; i <= nn; i++)
                rvalue[i] = ldexp(rfrom + (double)i * rby, 2);
        }
        if (endpoint && nn > 0) {
            if ((rby > 0 && rvalue[nn] > rto) ||
                (rby < 0 && rvalue[nn] < rto))
                rvalue[nn] = rto;
        }
        return value;
    }
    else if (lout < 1)
        return allocVector(INTSXP, 0);
    else if (One)
        return do_colon(ScalarReal(1.0), ScalarReal((double) lout));
    else if (missing_by) {
        double rfrom, rto, rby = 0, temp;
        if (missing_to) {
            rfrom = asReal(from);
            rto = rfrom + ((double)lout - 1);
            endpoint = 1;
        }
        else if (missing_from) {
            rto = asReal(to);
            rfrom = rto - (double)(endpoint ? (lout - 1) : lout);
        }
        else {
            rfrom = asReal(from);
            rto   = asReal(to);
        }
        if (!R_FINITE(rfrom)) error("'from' must be a finite number");
        if (!R_FINITE(rto))   error("'to' must be a finite number");
        Rboolean finite_del = 0;
        if (lout > 2) {
            double nint = (double)(endpoint ? (lout - 1) : lout);
            if ((finite_del = R_FINITE(rby = (rto - rfrom))))
                rby /= nint;
            else rby = (rto/nint - rfrom/nint);
        }
        temp = endpoint ? rto : (rto - rby);
        if (rfrom <= INT_MAX && rfrom >= INT_MIN &&
            temp  <= INT_MAX && temp  >= INT_MIN &&
            rfrom == (int)rfrom &&
            (lout <= 1 || temp == (int)temp) &&
            (lout <= 2 || rby == (int)rby)) {
            value = allocVector(INTSXP, lout);
            INTEGER(value)[0] = (int)rfrom;
            if (lout >= 2) INTEGER(value)[lout - 1] = (int)temp;
            if (lout >= 3)
                for (i = 1; i < lout - 1; i++) {
                    INTEGER(value)[i] = (int)(rfrom + (double)i*rby);
                }
            return value;
        }
        else {
            value = allocVector(REALSXP, lout);
            REAL(value)[0] = rfrom;
            if (lout >= 2) REAL(value)[lout - 1] = temp;
            if (lout >= 3) {
                if (finite_del)
                    for (i = 1; i < lout - 1; i++) {
                        REAL(value)[i] = rfrom + (double)i*rby;
                    }
                else {
                    rfrom /= 4.0;
                    rto   /= 4.0;
                    for (i = 1; i < lout - 1; i++) {
                        REAL(value)[i] = ldexp(rfrom + (double)i*rby, 2);
                    }
                }
            }
            return value;
        }
    }
    else if (missing_to) {
        double rfrom, rby = asReal(by), rto;
        if (missing_from) rfrom = 1.0;
        else rfrom = asReal(from);
        if (!R_FINITE(rfrom)) error("'from' must be a finite number");
        if (!R_FINITE(rby))   error("'by' must be a finite number");
        rto = rfrom + (double)(lout - 1)*rby;
        if (rfrom <= INT_MAX && rfrom >= INT_MIN &&
            rto   <= INT_MAX && rto   >= INT_MIN &&
            rby == (int)rby && rfrom == (int)rfrom) {
            value = allocVector(INTSXP, lout);
            for (i = 0; i < lout; i++) {
                INTEGER(value)[i] = (int)(rfrom + (double)i*rby);
            }
            return value;
        }
        else {
            value = allocVector(REALSXP, lout);
            for (i = 0; i < lout; i++) {
                REAL(value)[i] = rfrom + (double)i*rby;
            }
            return value;
        }
    }
    else if (missing_from) {
        double rto = asReal(to), rby = asReal(by), rfrom, temp;
        if (!R_FINITE(rto)) error("'to' must be a finite number");
        if (!R_FINITE(rby)) error("'by' must be a finite number");
        rfrom = rto - (endpoint ? (lout - 1) : lout) * rby;
        temp = endpoint ? rto : (rto - rby);
        if (rby == (int)rby && temp == (int)temp &&
            rfrom <= INT_MAX && rfrom >= INT_MIN &&
            temp  <= INT_MAX && temp  >= INT_MIN) {
            value = allocVector(INTSXP, lout);
            if (endpoint)
                for (i = 0; i < lout; i++) {
                    INTEGER(value)[i] = (int)(rto - (double)(lout - 1 - i)*rby);
                }
            else
                for (i = 0; i < lout; i++) {
                    INTEGER(value)[i] = (int)(rto - (double)(lout - i)*rby);
                }
            return value;
        }
        else {
            value = allocVector(REALSXP, lout);
            if (endpoint)
                for (i = 0; i < lout; i++) {
                    REAL(value)[i] = rto - (double)(lout - 1 - i)*rby;
                }
            else
                for (i = 0; i < lout; i++) {
                    REAL(value)[i] = rto - (double)(lout - i)*rby;
                }
            return value;
        }
    }
    error("too many arguments");
    return R_NilValue;
}

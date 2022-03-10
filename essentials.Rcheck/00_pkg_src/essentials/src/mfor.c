#include <R.h>
#include <Rinternals.h>





R_xlen_t do_length(SEXP x)
{
    if (isObject(x)) {
        SEXP expr, tmp;
        expr = PROTECT(lang2(
            install("length"),
            lang2(
                install("quote"),
                x
            )
        ));
        tmp = PROTECT(eval(expr, R_BaseEnv));
        R_xlen_t value = (R_xlen_t)
            (TYPEOF(tmp) == REALSXP ? REAL(tmp)[0] : asInteger(tmp));
        UNPROTECT(2);
        return value;
    }
    return xlength(x);
}


R_xlen_t * do_lengths(SEXP x, R_xlen_t length_x, const char *name)
{
    /* find lengths(x), and convert to a R_xlen_t array */


    SEXP expr = PROTECT(lang2(  /* lengths(x) */
        install("lengths"),
        lang2(
            install("quote"),
            x
        )
    ));
    SEXP tmp = PROTECT(eval(expr, R_BaseEnv));
    if (xlength(tmp) != length_x)
        error("'length(%s)' (%.0f) and 'length(lengths(%s))' (%.0f) are not equal",
            name, (double) length_x, name, (double) xlength(tmp));


    R_xlen_t *lengths_x;
    lengths_x = (R_xlen_t *) R_alloc(length_x, sizeof(R_xlen_t));
    switch (TYPEOF(tmp)) {
    case REALSXP:
        for (R_xlen_t i = 0; i < length_x; i++)
            lengths_x[i] = (R_xlen_t) (REAL(tmp)[i]);
        break;
    case INTSXP:
        for (R_xlen_t i = 0; i < length_x; i++)
            lengths_x[i] = (R_xlen_t) (INTEGER(tmp)[i]);
        break;
    default:
        error("invalid 'lengths(%s)' of type '%s'", name, type2char(TYPEOF(tmp)));
    }


    UNPROTECT(2);
    return lengths_x;
}


R_xlen_t get_commonLength(R_xlen_t *lengths, R_xlen_t length)
{
    R_xlen_t commonLength = 0;
    for (R_xlen_t i = 0; i < length; i++) {
        if (lengths[i] == 0)
            return lengths[i];
        else if (lengths[i] > commonLength)
            commonLength = lengths[i];
    }
    return commonLength;
}





SEXP do_mfor(SEXP rho, SEXP p, SEXP is_done)
{
    int np = 0;


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error("invalid 'rho'; should never happen, please report!");


    int n_args = ((dots == R_MissingArg) ? 0 : length(dots)),
        n_vars = n_args - 2;


    if (n_args < 3)
        error(
            (n_args == 1) ? "%d argument passed to 'mfor' which requires at least 3" :
                           "%d arguments passed to 'mfor' which requires at least 3",
            n_args
        );


    SEXP vars, seqs, expr, updaters, x, tmp, seqs_symbol, i_symbol;
    seqs_symbol = PROTECT(install("seqs"));              np++;
    i_symbol    = PROTECT(install("i"));                 np++;
    vars        = PROTECT(allocVector(VECSXP, n_vars));  np++;


    x = dots;
    for (int i = 0; i < n_vars; i++, x = CDR(x)) {
        tmp = PREXPR(CAR(x));
        if (!isSymbol(tmp) || tmp == R_MissingArg)
            error("non-symbol loop variable, argument %d", i + 1);
        SET_VECTOR_ELT(vars, i, tmp);
    }


    if (!isNull(TAG(x)))
        error("invalid 'seqs', should not be named");
    if (!isNull(TAG(CDR(x))))
        error("invalid 'expr', should not be named");


    seqs = PROTECT(eval(CAR(x), p));  np++;
    expr = PREXPR(CADR(x));


    Rboolean do_eval = 1;
    Rboolean realIndx;


    R_xlen_t commonLength;


    if (n_vars == 1) {
        commonLength = do_length(seqs);
        realIndx = commonLength > INT_MAX;
        updaters = PROTECT(allocVector(VECSXP, 1));  np++;
        SET_VECTOR_ELT(updaters, 0, lang3(
            R_Bracket2Symbol, seqs_symbol, i_symbol
        ));
    }
    else {
        R_xlen_t n_seqs = do_length(seqs);


        if (n_seqs != n_vars)
            error(
                (n_seqs < n_vars) ?
                    "not enough sequences to unpack (expected %.0f, got %.0f)" :
                      "too many sequences to unpack (expected %.0f, got %.0f)",
                                                   (double) n_vars,     (double) n_seqs
            );


        R_xlen_t *lengths_seqs = do_lengths(seqs, n_seqs, "seqs");


        /*
        Rprintf("lengths(seqs)   =");
        for (R_xlen_t i = 0; i < n_seqs; i++)
            Rprintf(" %.0f", (double) lengths_seqs[i]);
        Rprintf("\n");
         */


        commonLength = get_commonLength(lengths_seqs, n_seqs);
        do_eval = (commonLength != 0);
        if (do_eval) {


            /* fractional recycling warning */
            for (R_xlen_t j = 0; j < n_seqs; j++) {
                if (commonLength % lengths_seqs[j] != 0) {
                    warning("a sequence will be fractionally recycled");
                    break;
                }
            }


            realIndx = commonLength > INT_MAX;
            updaters = PROTECT(allocVector(VECSXP, n_seqs));  np++;


            for (R_xlen_t j = 0; j < n_seqs; j++) {
                SEXP v = PROTECT(lang3(
                    R_Bracket2Symbol,
                    seqs_symbol,
                    ScalarInteger(j + 1)
                ));
                if (lengths_seqs[j] == 1)
                    v = PROTECT(lang3(
                        R_Bracket2Symbol,
                        v,
                        ScalarInteger(1)
                    ));
                else if (lengths_seqs[j] == commonLength)
                    v = PROTECT(lang3(
                        R_Bracket2Symbol,
                        v,
                        i_symbol
                    ));
                else {
                    v = PROTECT(lang3(
                        R_Bracket2Symbol,
                        v,
                        lang3(
                            install("+"),
                            lang3(
                                install("%%"),
                                lang3(
                                    install("-"),
                                    i_symbol,
                                    ScalarInteger(1)
                                ),
                                realIndx ? ScalarReal   (lengths_seqs[j]) :
                                           ScalarInteger(lengths_seqs[j])
                            ),
                            ScalarInteger(1)
                        )
                    ));
                }
                SET_VECTOR_ELT(updaters, j, v);
                UNPROTECT(2);
            }
        }
    }


    SEXP loop_expr = R_NilValue;


    if (do_eval) {
        loop_expr = PROTECT(lang2(
            install("repeat"),
            lang3(
                R_BraceSymbol,
                lang3(
                    install("if"),
                    lang2(
                        is_done,
                        rho
                    ),
                    lang1(install("break"))
                ),
                expr
            )
        ));  np++;


        defineVar(
            i_symbol,
            realIndx ? ScalarReal(0.0) : ScalarInteger(0),
            rho
        );
        defineVar(install("seqs"), seqs, rho);
        defineVar(
            install("commonLength"),
            realIndx ? ScalarReal(commonLength) : ScalarInteger(commonLength),
            rho
        );
        defineVar(
            install("realIndx"),
            ScalarLogical(realIndx),
            rho
        );
        defineVar(
            install("n_vars"),
            ScalarInteger(n_vars),
            rho
        );
        defineVar(install("vars"), vars, rho);
        defineVar(install("updaters"), updaters, rho);
        defineVar(install("p"), p, rho);
    }


    for (int i = 0; i < n_vars; i++) {
        defineVar(
            VECTOR_ELT(vars, i),
            R_NilValue,
            p
        );
    }


    /*
    UNPROTECT(np);
    return loop_expr;
     */

    if (do_eval)
        eval(loop_expr, p);


    UNPROTECT(np);
    return R_NilValue;



    SEXP value = PROTECT(allocVector(VECSXP, 3));  np++;
    SET_VECTOR_ELT(value, 0, vars);
    SET_VECTOR_ELT(value, 1, seqs);
    SET_VECTOR_ELT(value, 2, expr);


    UNPROTECT(np);
    return value;
}





SEXP do_is_mfor_done(SEXP rho)
{
    const char *err_msg = "object '%s' not found, something is very wrong with 'mfor', please report!";


    SEXP i = findVarInFrame(rho, install("i"));
    if (i == R_UnboundValue)
        error(err_msg, "seqs");


    SEXP commonLength = findVarInFrame(rho, install("commonLength"));
    if (commonLength == R_UnboundValue)
        error(err_msg, "commonLength");


    SEXP realIndx = findVarInFrame(rho, install("realIndx"));
    if (realIndx == R_UnboundValue)
        error(err_msg, "realIndx");


    Rboolean _realIndx = LOGICAL(realIndx)[0];


    /* return TRUE to signify end of looping */
    if (_realIndx ? (REAL   (i)[0] >= REAL   (commonLength)[0]) :
                    (INTEGER(i)[0] >= INTEGER(commonLength)[0]))
        return ScalarLogical(1);


    SEXP n_vars = findVarInFrame(rho, install("n_vars"));
    if (n_vars == R_UnboundValue)
        error(err_msg, "n_vars");


    SEXP vars = findVarInFrame(rho, install("vars"));
    if (vars == R_UnboundValue)
        error(err_msg, "vars");


    SEXP updaters = findVarInFrame(rho, install("updaters"));
    if (updaters == R_UnboundValue)
        error(err_msg, "updaters");


    SEXP p = findVarInFrame(rho, install("p"));
    if (p == R_UnboundValue)
        error(err_msg, "p");


    int _n_vars = INTEGER(n_vars)[0];


    if (_realIndx)
        REAL(i)[0]++;
    else INTEGER(i)[0]++;


    for (int k = 0; k < _n_vars; k++) {
        defineVar(
            VECTOR_ELT(vars, k),
            eval(VECTOR_ELT(updaters, k), rho),
            p
        );
    }


    return ScalarLogical(0);
}

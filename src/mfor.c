#include <R.h>
#include <Rinternals.h>





// #define debug
#define SOMETHING_WRONG_WITH_MFOR(X) (error("object '%s' not found; something is very wrong with 'mfor', please report!", (X)))





#include "defines.h"





extern R_xlen_t dispatchLength(SEXP x, SEXP rho);


R_xlen_t * do_lengths(SEXP x, R_xlen_t length_x, const char *name)
{
    // x
    //
    //     object for which to find the 'lengths'
    //
    // length_x
    //
    //     the length of 'x', this is usually pre-calculated so don't wany to
    //     recalculate
    //
    // name
    //
    //     string; name of the variable at the R level. for error messages


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





/* mfor(*vars, seqs, expr) */
SEXP do_mfor(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprotect = 0;


    SEXP is_mfor_done = PROTECT(eval(      install("is.mfor.done") , rho)); nprotect++;
    SEXP p            = PROTECT(eval(lang1(install("parent.frame")), rho)); nprotect++;


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("...");


    int n_args = dotsLength(dots),
        n_vars = n_args - 2;


    if (n_args < 3)
        error(
            (n_args == 1) ? "%d argument passed to 'mfor' which requires at least 3" :
                           "%d arguments passed to 'mfor' which requires at least 3",
            n_args
        );


    SEXP vars, seqs, expr, updaters, x, tmp, seqs_symbol, i_symbol;
    seqs_symbol = install("seqs");
    i_symbol    = install("i");
    vars        = PROTECT(allocVector(VECSXP, n_vars));  nprotect++;


    x = dots;
    for (int i = 0; i < n_vars; i++, x = CDR(x)) {
        tmp = PREXPR(CAR(x));
        if (!isSymbol(tmp) || tmp == R_MissingArg)
            error("non-symbol loop variable, argument %d", i + 1);
        SET_VECTOR_ELT(vars, i, tmp);
    }
#ifdef debug
    Rprintf("\n> vars\n");
    R_print(vars);
    Rprintf("\n");
#endif


    if (!isNull(TAG(x)))
        error("invalid 'seqs', should not be named");


    seqs = PROTECT(eval(CAR(x), p));  nprotect++;
#ifdef debug
    Rprintf(n_vars == 1 ? "\n> seq\n" : "\n> seqs\n");
    R_print(seqs);
    Rprintf("\n");
#endif


    expr = PREXPR(CADR(x));
    if (!isNull(TAG(CDR(x)))) {
        expr = PROTECT(lang3(
            install("="),
            TAG(CDR(x)),
            expr
        ));  nprotect++;
    }
#ifdef debug
    Rprintf("> expr\n");
    R_print(expr);
#endif


    Rboolean do_eval = 1;
    Rboolean realIndx;


    R_xlen_t commonLength;


    if (n_vars == 1) {
        commonLength = dispatchLength(seqs, p);
#ifdef debug
        Rprintf("> length(seq)\n");
        R_print(ScalarReal((double) commonLength));
#endif
        realIndx = commonLength > INT_MAX;
        updaters = PROTECT(allocVector(VECSXP, 1));  nprotect++;
        SET_VECTOR_ELT(updaters, 0, lang3(
            R_Bracket2Symbol, seqs_symbol, i_symbol
        ));
#ifdef debug
        Rprintf("> updater\n");
        R_print(updaters);
#endif
    }
    else {
        R_xlen_t n_seqs = dispatchLength(seqs, p);
#ifdef debug
        Rprintf("\n> length(seqs)\n");
        R_print(ScalarReal((double) n_seqs));
        Rprintf("\n");
#endif


        if (n_vars != n_seqs)
            error(
                (n_vars > n_seqs) ?
                    "not enough sequences to unpack (expected %.0f, got %.0f)" :
                      "too many sequences to unpack (expected %.0f, got %.0f)",
                                                   (double) n_vars,     (double) n_seqs
            );


        R_xlen_t *lengths_seqs = do_lengths(seqs, n_seqs, "seqs");
#ifdef debug
        SEXP print_this = PROTECT(allocVector(REALSXP, n_seqs));
        for (R_xlen_t i = 0; i < n_seqs; i++)
            REAL(print_this)[i] = (double) lengths_seqs[i];
        Rprintf("\n> lengths(seqs)\n");
        R_print(print_this);
        Rprintf("\n");
        UNPROTECT(1);
#endif


        commonLength = get_commonLength(lengths_seqs, n_seqs);
#ifdef debug
        Rprintf("\n> commonLength(seqs)\n");
        R_print(ScalarReal(commonLength));
        Rprintf("\n");
#endif
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
            updaters = PROTECT(allocVector(VECSXP, n_seqs));  nprotect++;


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
#ifdef debug
            Rprintf("\n> updaters\n");
            R_print(updaters);
            Rprintf("\n");
#endif
        }
    }


    SEXP loop_expr = R_NilValue;


    if (do_eval) {
        loop_expr = PROTECT(lang2(
            findVarInFrame(R_BaseEnv, install("repeat")),
            lang3(
                findVarInFrame(R_BaseEnv, R_BraceSymbol),
                lang3(
                    findVarInFrame(R_BaseEnv, install("if")),
                    lang2(
                        is_mfor_done,
                        rho
                    ),
                    lang1(findVarInFrame(R_BaseEnv, install("break")))
                ),
                expr
            )
        ));  nprotect++;
#ifdef debug
        Rprintf("\n> loop_expr\n");
        R_print(loop_expr);
        Rprintf("\n");
#endif


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


    /*
    SEXP value = PROTECT(allocVector(LISTSXP, 4));  nprotect++;
    tmp = value;


    SETCAR(tmp, vars     ); SET_TAG(tmp, install("vars"     )); tmp = CDR(tmp);
    SETCAR(tmp, seqs     ); SET_TAG(tmp, install("seqs"     )); tmp = CDR(tmp);
    SETCAR(tmp, expr     ); SET_TAG(tmp, install("expr"     )); tmp = CDR(tmp);
    SETCAR(tmp, loop_expr); SET_TAG(tmp, install("loop_expr"));


    UNPROTECT(nprotect);
    return value;
     */


    for (int i = 0; i < n_vars; i++) {
        defineVar(
            VECTOR_ELT(vars, i),
            R_NilValue,
            p
        );
    }


    if (do_eval)
        eval(loop_expr, p);


    set_R_Visible(0);
    UNPROTECT(nprotect);
    return R_NilValue;
}





SEXP do_ismfordone(SEXP rho)
{
    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    SEXP i = findVarInFrame(rho, install("i"));
    if (i == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("i");


    SEXP commonLength = findVarInFrame(rho, install("commonLength"));
    if (commonLength == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("commonLength");


    SEXP realIndx = findVarInFrame(rho, install("realIndx"));
    if (realIndx == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("realIndx");


    Rboolean _realIndx = LOGICAL(realIndx)[0];


    /* return TRUE to signify end of looping */
    if (_realIndx ? (REAL   (i)[0] >= REAL   (commonLength)[0]) :
                    (INTEGER(i)[0] >= INTEGER(commonLength)[0]))
        return ScalarLogical(1);


    SEXP n_vars = findVarInFrame(rho, install("n_vars"));
    if (n_vars == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("n_vars");


    SEXP vars = findVarInFrame(rho, install("vars"));
    if (vars == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("vars");


    SEXP updaters = findVarInFrame(rho, install("updaters"));
    if (updaters == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("updaters");


    SEXP p = findVarInFrame(rho, install("p"));
    if (p == R_UnboundValue)
        SOMETHING_WRONG_WITH_MFOR("p");


    int _n_vars = INTEGER_ELT(n_vars, 0);


    if (_realIndx)
        REAL(i)[0]++;
    else INTEGER(i)[0]++;


    for (int indx = 0; indx < _n_vars; indx++) {
        defineVar(
            VECTOR_ELT(vars, indx),
            eval(VECTOR_ELT(updaters, indx), rho),
            p
        );
    }


    return ScalarLogical(0);
}

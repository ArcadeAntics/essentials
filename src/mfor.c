#include <R.h>
#include <Rinternals.h>





// #define debug
#define SOMETHING_WRONG_WITH_MFOR(X) (error("object '%s' not found; something is very wrong with 'mfor', please report!", (X)))





#include "defines.h"
#include "translations.h"





extern R_xlen_t dispatchLength(SEXP x, SEXP rho);


R_xlen_t * get_lengths(SEXP x, R_xlen_t length_x, const char *name)
{
    // x
    //
    //     object for which to find the 'lengths'
    //
    // length_x
    //
    //     the length of 'x', this is usually pre-calculated so don't want to
    //     re-calculate
    //
    // name
    //
    //     name of R level variable, for error messages


    /* find lengths(x), and convert to a R_xlen_t array */


    static SEXP lengthsSymbol = NULL;
    if (lengthsSymbol == NULL) {
        lengthsSymbol = install("lengths");
    }


    /* lengths(x) */
    SEXP expr = PROTECT(lang2(lengthsSymbol, lang2(R_QuoteSymbol, x)));
    SEXP tmp = PROTECT(eval(expr, R_BaseEnv));
    if (xlength(tmp) != length_x)
        error("'length(%s)' (%.0f) and 'length(lengths(%s))' (%.0f) are not equal",
            name, (double) length_x, name, (double) xlength(tmp));


    R_xlen_t *lengths_x;
    lengths_x = (R_xlen_t *) R_alloc(length_x, sizeof(R_xlen_t));
    switch (TYPEOF(tmp)) {
    case REALSXP:
    {
        double *rtmp = REAL(tmp);
        for (R_xlen_t i = 0; i < length_x; i++)
            lengths_x[i] = (R_xlen_t) (rtmp[i]);
    }
        break;
    case INTSXP:
    {
        int *itmp = INTEGER(tmp);
        for (R_xlen_t i = 0; i < length_x; i++)
            lengths_x[i] = (R_xlen_t) (itmp[i]);
    }
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





typedef struct mfor_info {
    Rboolean realIndx;
    R_xlen_t commonLength;
    SEXP i;
    SEXP updaters;
    SEXP mfor_rho;
    SEXP p;
} MFOR_INFO, *ptrMFOR_INFO;


static void finalizer(SEXP ptr)
{
    ptrMFOR_INFO info = R_ExternalPtrAddr(ptr);
    if (!info) return;
    /* Rprintf("\nfinalizing %p\n", R_ExternalPtrAddr(ptr)); */
    R_Free(info);
    R_ClearExternalPtr(ptr);
}


SEXP getInFrame(SEXP sym, SEXP env, int unbound_ok)
{
    SEXP value = findVarInFrame(env, sym);
    if (!unbound_ok && value == R_UnboundValue)
        error(_("object '%s' not found"), CHAR(PRINTNAME(sym)));
    if (TYPEOF(value) == PROMSXP) {
        if (PRVALUE(value) == R_UnboundValue)
            return eval(value, R_EmptyEnv);
        else
            return PRVALUE(value);
    }
    else return value;
}


/* mfor(*vars, seqs, expr) */
SEXP do_mfor(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP _is_mfor_doneSymbol = NULL,
                parent_frameSymbol = NULL,
                seqsSymbol = NULL,
                iSymbol = NULL,
                equalsSymbol = NULL,
                plusSymbol = NULL,
                modSymbol = NULL,
                minusSymbol = NULL,
                repeatSymbol = NULL,
                ifSymbol = NULL,
                breakSymbol = NULL;
    if (_is_mfor_doneSymbol == NULL) {
        _is_mfor_doneSymbol = install(".is_mfor_done");
        parent_frameSymbol = install("parent.frame");
        seqsSymbol = install("seqs");
        iSymbol = install("i");
        equalsSymbol = install("=");
        plusSymbol = install("+");
        modSymbol = install("%%");
        minusSymbol = install("-");
        repeatSymbol = install("repeat");
        ifSymbol = install("if");
        breakSymbol = install("break");
    }


    int nprotect = 0;


    SEXP p = PROTECT(eval(lang1(parent_frameSymbol), rho)); nprotect++;


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


    SEXP seqs, expr, updaters, tmp;
    updaters = PROTECT(allocVector(LISTSXP, n_vars)); nprotect++;


    SEXP d = dots, u = updaters;
    for (int i = 0; i < n_vars; i++, d = CDR(d), u = CDR(u)) {
        if (TAG(d) != R_NilValue)
            error("invalid 'vars', should not be named");
        tmp = PREXPR(CAR(d));
        if (TYPEOF(tmp) != SYMSXP || tmp == R_MissingArg)
            error("non-symbol loop variable, argument %d", i + 1);
        SET_TAG(u, tmp);
    }


    if (!isNull(TAG(d)))
        error("invalid 'seqs', should not be named");


    seqs = PROTECT(eval(CAR(d), p)); nprotect++; d = CDR(d);
#ifdef debug
    Rprintf(n_vars == 1 ? "\n> seq\n" : "\n> seqs\n");
    R_print(seqs);
#endif


    expr = PREXPR(CAR(d));
    if (!isNull(TAG(d))) {
        expr = PROTECT(lang3(equalsSymbol, TAG(d), expr)); nprotect++;
    }
#ifdef debug
    Rprintf("\n> expr\n");
    R_print(expr);
#endif


    Rboolean do_eval = 1;
    Rboolean realIndx = FALSE;


    R_xlen_t commonLength;


    if (n_vars == 1) {
        commonLength = dispatchLength(seqs, p);
#ifdef debug
        Rprintf("\n> length(seq)\n");
        R_print(ScalarReal((double) commonLength));
#endif
        realIndx = commonLength > INT_MAX;
        SETCAR(updaters, lang3(R_Bracket2Symbol, seqsSymbol, iSymbol));
#ifdef debug
        Rprintf("\n> updater\n");
        R_print(updaters);
#endif
    }
    else {
        R_xlen_t n_seqs = dispatchLength(seqs, p);
#ifdef debug
        Rprintf("\n> length(seqs)\n");
        R_print(ScalarReal((double) n_seqs));
#endif


        if (n_vars != n_seqs)
            error(
                (n_vars > n_seqs) ?
                    "not enough sequences to unpack (expected %.0f, got %.0f)" :
                      "too many sequences to unpack (expected %.0f, got %.0f)",
                                                   (double) n_vars,     (double) n_seqs
            );


        R_xlen_t *lengths_seqs = get_lengths(seqs, n_seqs, "seqs");
#ifdef debug
        SEXP print_this = PROTECT(allocVector(REALSXP, n_seqs));
        for (R_xlen_t i = 0; i < n_seqs; i++)
            REAL(print_this)[i] = (double) lengths_seqs[i];
        Rprintf("\n> lengths(seqs)\n");
        R_print(print_this);
        UNPROTECT(1);
#endif


        commonLength = get_commonLength(lengths_seqs, n_seqs);
#ifdef debug
        Rprintf("\n> commonLength(seqs)\n");
        R_print(ScalarReal(commonLength));
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


            u = updaters;
            for (R_xlen_t j = 0; j < n_seqs; j++, u = CDR(u)) {
                /* seqs[[j]] */
                SEXP v = PROTECT(lang3(
                    R_Bracket2Symbol,
                    seqsSymbol,
                    ScalarInteger(j + 1)
                ));
                if (lengths_seqs[j] == 1)
                    /* seqs[[j]][[1L]] */
                    v = PROTECT(lang3(R_Bracket2Symbol, v, ScalarInteger(1)));
                else if (lengths_seqs[j] == commonLength)
                    /* seqs[[j]][[i]] */
                    v = PROTECT(lang3(R_Bracket2Symbol, v, iSymbol));
                else {
                    /* seqs[[j]][[(i - 1) %% length(seqs[[j]]) + 1]] */
                    v = PROTECT(lang3(
                        R_Bracket2Symbol,
                        v,
                        lang3(
                            plusSymbol,
                            lang3(
                                modSymbol,
                                lang3(
                                    minusSymbol,
                                    iSymbol,
                                    realIndx ? ScalarReal(1.0) : ScalarInteger(1)
                                ),
                                realIndx ? ScalarReal   (lengths_seqs[j]) :
                                           ScalarInteger(lengths_seqs[j])
                            ),
                            realIndx ? ScalarReal(1.0) : ScalarInteger(1)
                        )
                    ));
                }
                SETCAR(u, v);
                UNPROTECT(2);
            }
#ifdef debug
            Rprintf("\n> updaters\n");
            R_print(updaters);
#endif
        }
    }


    SEXP loop_expr = R_NilValue;


    ptrMFOR_INFO info = R_Calloc(1, MFOR_INFO);
    SEXP ptr = R_MakeExternalPtr(info, R_NilValue, R_NilValue);
    PROTECT(ptr); nprotect++;
    R_RegisterCFinalizerEx(ptr, finalizer, TRUE);
    if (do_eval) {


        info->realIndx = realIndx;
        info->commonLength = commonLength;
        defineVar(iSymbol, info->i = realIndx ? ScalarReal(0.0) : ScalarInteger(0), rho);
        info->updaters = updaters;
        info->mfor_rho = rho;
        info->p = p;


        /* repeat if (.is_mfor_done(ptr)) break else expr */
        loop_expr = PROTECT(lang2(
            getInFrame(repeatSymbol, R_BaseEnv, FALSE),
            lang4(
                getInFrame(ifSymbol, R_BaseEnv, FALSE),
                lang2(eval(_is_mfor_doneSymbol, rho), ptr),
                lang1(getInFrame(breakSymbol, R_BaseEnv, FALSE)),
                expr
            )
        )); nprotect++;
#ifdef debug
        Rprintf("\n> loop_expr\n");
        R_print(PROTECT(lang2(
            repeatSymbol,
            lang4(
                ifSymbol,
                lang2(_is_mfor_doneSymbol, ptr),
                lang1(breakSymbol),
                expr
            )
        ))); UNPROTECT(1);
#endif

        defineVar(seqsSymbol, seqs, rho);
    }


    for (SEXP u = updaters; u != R_NilValue; u = CDR(u))
        defineVar(TAG(u), R_NilValue, p);


    if (do_eval)
        eval(loop_expr, p);


    UNPROTECT(nprotect);
    set_R_Visible(0);
    return R_NilValue;
}





SEXP do_ismfordone(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ptr = CADR(args);
    if (TYPEOF(ptr) != EXTPTRSXP)
        error("not an external pointer");
    ptrMFOR_INFO info = R_ExternalPtrAddr(ptr);
    if (info == NULL)
        error("external pointer is NULL");


    /* return TRUE to signify end of looping */
    if (info->realIndx) {
        if (REAL(info->i)[0] >= info->commonLength)
            return ScalarLogical(TRUE);
        ++REAL(info->i)[0];
    } else {
        if (INTEGER(info->i)[0] >= info->commonLength)
            return ScalarLogical(TRUE);
        ++INTEGER(info->i)[0];
    }


    SEXP updaters = info->updaters,
         mfor_rho = info->mfor_rho,
         p = info->p;


    for (SEXP u = updaters; u != R_NilValue; u = CDR(u))
        defineVar(TAG(u), eval(CAR(u), mfor_rho), p);



    return ScalarLogical(FALSE);
}

#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include "defines.h"
#include "translations.h"


#if !defined(R_VERSION) || R_VERSION < R_Version(4, 1, 0)
SEXP R_NewEnv(SEXP enclos, int hash, int size)
{
    static SEXP new_envSymbol = NULL;
    if (new_envSymbol == NULL) {
        new_envSymbol = install("new.env");
    }


    if (TYPEOF(enclos) != ENVSXP) error("invalid 'enclos'");
    SEXP expr = lang4(
        new_envSymbol,
        ScalarInteger(hash),
        enclos,
        ScalarInteger(size)
    );
    PROTECT(expr);
    SEXP value = eval(expr, R_BaseEnv);
    UNPROTECT(1);
    return value;
}
#endif


void delayedAssign(SEXP value, SEXP eval_env, SEXP assign_env)
{
    static SEXP delayedAssignSymbol = NULL;
    if (delayedAssignSymbol == NULL) {
        delayedAssignSymbol = install("delayedAssign");
    }


    SEXP expr = PROTECT(lang5(
        delayedAssignSymbol,
        mkString("x"),
        value,
        eval_env,
        assign_env
    ));
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}


extern void SET_PRCODE(SEXP x, SEXP v);
extern void SET_PRENV(SEXP x, SEXP v);


SEXP mkPROMISE(SEXP prcode, SEXP prenv)
{
    static SEXP delayedAssignSymbol = NULL,
                xSymbol = NULL;
    if (delayedAssignSymbol == NULL) {
        delayedAssignSymbol = install("delayedAssign");
        xSymbol = install("x");
    }



    SEXP env = R_NewEnv(R_BaseEnv, TRUE, 1);
    PROTECT(env);
    SEXP expr = lang3(
        delayedAssignSymbol,
        ScalarString(PRINTNAME(xSymbol)),
        R_NilValue
    );
    PROTECT(expr);
    eval(expr, env);
    SEXP promise = findVarInFrame(env, xSymbol);
    SET_PRCODE(promise, prcode);
    SET_PRENV(promise, prenv);
    UNPROTECT(2);
    return promise;
}


SEXP do_mkpromise(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    if (TYPEOF(CADDR(args)) != ENVSXP)
        error("invalid 'prenv', must be an environment");
    return mkPROMISE(CADR(args), CADDR(args));
}


SEXP do_delayedassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP delayedAssignSymbol = NULL;
    if (delayedAssignSymbol == NULL) {
        delayedAssignSymbol = install("delayedAssign");
    }


    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP value = CAR(args); args = CDR(args);
    SEXP eval_env = CAR(args); args = CDR(args);
    SEXP assign_env = CAR(args); args = CDR(args);


    if (!isString(x) || LENGTH(x) == 0)
        error(_("invalid first argument"));
	SEXP sym = installTrChar(STRING_ELT(x, 0));


	if (!isEnvironment(eval_env))
	    error(_("invalid '%s' argument"), "eval.env");


	if (!isEnvironment(assign_env))
	    error(_("invalid '%s' argument"), "assign.env");


    SEXP expr = PROTECT(lang5(delayedAssignSymbol, x, R_NilValue, eval_env, assign_env));
    eval(expr, R_BaseEnv);
    UNPROTECT(1);


    SEXP promise = findVarInFrame(assign_env, sym);
    SET_PRCODE(promise, value);
    set_R_Visible(FALSE);
    return R_NilValue;
}


SEXP R_getS4DataSlot(SEXP x, SEXPTYPE type)
{
    //    Rprintf("IS_S4_OBJECT(x) && (TYPEOF(x) == S4SXP) = %d\n", IS_S4_OBJECT(x) && (TYPEOF(x) == S4SXP));
    if (IS_S4_OBJECT(x) && (TYPEOF(x) == S4SXP)) {
        SEXP data = PROTECT(getAttrib(x, install(".Data")));
        if (data == R_NilValue) {
            UNPROTECT(1);
            data = PROTECT(getAttrib(x, install(".xData")));
        }
        if (data == R_NilValue) {
            UNPROTECT(1);
            return R_NilValue;
        }
        if (type == ANYSXP || TYPEOF(data) == type) {
            UNPROTECT(1);
            return data;
        }
        UNPROTECT(1);
        return R_NilValue;
    }
    else {
        return R_NilValue;
    }
}


#define simple_as_environment(arg) (R_getS4DataSlot(arg, ENVSXP))


SEXP list2env(SEXP x, SEXP parent)
{
    SEXP tmp, value, xnms;
    int i, n;
    n = length(x);
    value = PROTECT(R_NewEnv(parent, TRUE, n));
    switch(TYPEOF(x)) {
    case LISTSXP:
        for (i = n - 1; i >= 0; i--) {
            tmp = nthcdr(x, i);
            defineVar(
                TAG(tmp),
                lazy_duplicate(CAR(tmp)),
                value
            );
        }
        break;
    case VECSXP:
        xnms = PROTECT(getAttrib(x, R_NamesSymbol));
        if (n && (TYPEOF(xnms) != STRSXP || length(xnms) != n))
            error("names(x) must be a character vector of the same length as x");
        for (i = n - 1; i >= 0; i--) {
            defineVar(
                installTrChar(STRING_ELT(xnms, i)),
                lazy_duplicate(VECTOR_ELT(x, i)),
                value
            );
        }
        UNPROTECT(1);
        break;
    default:
        error("invalid '%s' of type '%s'",
              "x", type2char(TYPEOF(x)));
    }
    UNPROTECT(1);
    return value;
}


SEXP asenv(SEXP envir, SEXP enclos, SEXP rho)
{
    static SEXP parent_frameSymbol = NULL,
        sys_frameSymbol = NULL;
    if (parent_frameSymbol == NULL) {
        parent_frameSymbol = install("parent.frame");
        sys_frameSymbol = install("sys.frame");
    }


    SEXP value = envir;


    SEXPTYPE tEnclos = TYPEOF(enclos);
    if (isNull(enclos)) {
        enclos = R_BaseEnv;
    } else if (!isEnvironment(enclos) &&
        !isEnvironment((enclos = simple_as_environment(enclos)))) {
        error(_("invalid '%s' argument of type '%s'"),
              "enclos", type2char(tEnclos));
    }


    if (IS_S4_OBJECT(value) && (TYPEOF(value) == S4SXP))
        value = R_getS4DataSlot(value, ANYSXP);
    switch (TYPEOF(value)) {
    case NILSXP:
        return enclos;
        break;
    case ENVSXP:
        return value;
        break;
    case LISTSXP:
        return list2env(envir, enclos);
        break;
    case VECSXP:
        return list2env(envir, enclos);
        break;
    case INTSXP:
    case REALSXP:
    {
        if (length(value) != 1)
            error(_("numeric 'envir' arg not of length one"));
        int frame = asInteger(value);
        if (frame == NA_INTEGER)
            error(_("invalid '%s' argument of type '%s'"),
                  "envir", type2char(TYPEOF(value)));
        SEXP expr = lang1(parent_frameSymbol);
        PROTECT(expr);
        SEXP context = eval(expr, rho);
        UNPROTECT(1);  /* expr */
        PROTECT(context);
        expr = lang2(sys_frameSymbol, ScalarInteger(frame));
        PROTECT(expr);
        value = eval(expr, context);
        UNPROTECT(2);  /* context & expr */
        return value;
    }
        break;
    default:
        error(_("invalid '%s' argument of type '%s'"),
              "envir", type2char(TYPEOF(value)));
    }


    return R_NilValue;  /* -Wall */
}


SEXP do_asenv(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP parent_frameSymbol = NULL,
        sys_frameSymbol = NULL;
    if (parent_frameSymbol == NULL) {
        parent_frameSymbol = install("parent.frame");
        sys_frameSymbol = install("sys.frame");
    }


    args = CDR(args);
    SEXP envir = CAR(args); args = CDR(args);
    SEXP enclos = CAR(args); args = CDR(args);


    int nprotect = 0;
    SEXP value = envir;


    SEXPTYPE tEnclos = TYPEOF(enclos);
    if (isNull(enclos)) {
        enclos = R_BaseEnv;
    } else if (!isEnvironment(enclos) &&
        !isEnvironment((enclos = simple_as_environment(enclos)))) {
        error(_("invalid '%s' argument of type '%s'"),
              "enclos", type2char(tEnclos));
    }


    if (IS_S4_OBJECT(value) && (TYPEOF(value) == S4SXP))
        value = R_getS4DataSlot(value, ANYSXP);
    switch (TYPEOF(value)) {
    case NILSXP:
        value = enclos;
    case ENVSXP:
        PROTECT(value); nprotect++;
        break;
    case LISTSXP:
        value = list2env(envir, enclos);
        PROTECT(value); nprotect++;
        break;
    case VECSXP:
        value = list2env(envir, enclos);
        PROTECT(value); nprotect++;
        break;
    case INTSXP:
    case REALSXP:
    {
        if (length(value) != 1)
            error(_("numeric 'envir' arg not of length one"));
        int frame = asInteger(value);
        if (frame == NA_INTEGER)
            error(_("invalid '%s' argument of type '%s'"),
                  "envir", type2char(TYPEOF(value)));
        SEXP expr = lang2(parent_frameSymbol, ScalarInteger(2));
        PROTECT(expr);
        SEXP context = eval(expr, rho);
        UNPROTECT(1);  /* expr */
        PROTECT(context); nprotect++;
        expr = lang2(sys_frameSymbol, ScalarInteger(frame));
        PROTECT(expr);
        value = eval(expr, context);
        UNPROTECT(1);  /* expr */
    }
        break;
    default:
        error(_("invalid '%s' argument of type '%s'"),
              "envir", type2char(TYPEOF(value)));
    }


    UNPROTECT(nprotect);
    return value;
}


SEXP do_fstrold(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP sprintf = CAR(args); args = CDR(args);
    SEXP fmt     = CAR(args); args = CDR(args);
    SEXP exprs   = CAR(args); args = CDR(args);
    SEXP envir   = CAR(args); args = CDR(args);

    PROTECT_INDEX indx;
    SEXP expr = R_NilValue;
    PROTECT_WITH_INDEX(expr, &indx);
    for (R_xlen_t j = xlength(exprs) - 1; j >= 0; j--)
        REPROTECT(expr = LCONS(VECTOR_ELT(exprs, j), expr), indx);
    REPROTECT(expr = LCONS(fmt    , expr), indx);
    REPROTECT(expr = LCONS(sprintf, expr), indx);
    SEXP value = eval(expr, envir);
    UNPROTECT(1);
    return value;
}





#define _f_str                                                                        \
do {                                                                                  \
    REPROTECT(expr = R_NilValue, indx);                                               \
    yy = VECTOR_ELT(y, i);                                                            \
    if (length(yy) > 0) {                                                             \
        mm = VECTOR_ELT(m, i);                                                        \
        imm = INTEGER(mm);                                                            \
        for (mm_indx = length(mm) - 2; mm_indx >= 0; mm_indx -= 7) {                  \
            if ( (!imm[mm_indx]) + (!imm[mm_indx - 1]) + (!imm[mm_indx - 2]) != 2)    \
                error("invalid 'm'; should never happen, please report!");            \
            if (imm[yy_indx = mm_indx]) {}                                            \
            else if (imm[--yy_indx]) {}                                               \
            else if (imm[--yy_indx]) {}                                               \
            else error("invalid 'm'; should never happen, please report!");           \
            tmp2 = PROTECT(ScalarString(STRING_ELT(yy, yy_indx)));                    \
            tmp2 = PROTECT(lang2(str2expressionSymbol, tmp2));                        \
            tmp = PROTECT(eval(tmp2, R_BaseEnv));                                     \
            switch (length(tmp)) {                                                    \
            case 1:                                                                   \
                REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 0), expr), indx);              \
                break;                                                                \
            case 2:                                                                   \
                REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 1), expr), indx);              \
                REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 0), expr), indx);              \
                break;                                                                \
            default:                                                                  \
                error("parsing result not of length one or two, but %d", length(tmp));\
            }                                                                         \
            UNPROTECT(3);                                                             \
        }                                                                             \
    }                                                                                 \
    REPROTECT(expr = LCONS(ScalarString(STRING_ELT(fmt, i)), expr), indx);            \
    REPROTECT(expr = LCONS(sprintf, expr), indx);                                     \
    vvalue = eval(expr, env);                                                         \
} while (0)





SEXP do_fstr(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP perlSymbol = NULL,
        gregexecSymbol = NULL,
        regmatchesSymbol = NULL,
        gsubSymbol = NULL,
        sprintfSymbol = NULL,
        str2expressionSymbol = NULL;
    if (perlSymbol == NULL) {
        perlSymbol = install("perl");
        gregexecSymbol = install("gregexec");
        regmatchesSymbol = install("regmatches");
        gsubSymbol = install("gsub");
        sprintfSymbol = install("sprintf");
        str2expressionSymbol = install("str2expression");
    }


    int nprotect = 0;
    SEXP tmp, tmp2;


    args = CDR(args);
    SEXP x = CAR(args); args = CDR(args);
    SEXP envir = CAR(args); args = CDR(args);
    SEXP enclos = CAR(args); args = CDR(args);
    Rboolean simplify = asLogical(CAR(args)); args = CDR(args);
    if (simplify == NA_LOGICAL)
        error(_("invalid '%s' argument"), "simplify");


    int len;
    if (TYPEOF(x) != STRSXP) {
        if (isObject(x)) {
            tmp = PROTECT(lang2(R_AsCharacterSymbol, lang2(R_QuoteSymbol, x))); nprotect++;
            x = PROTECT(eval(tmp, R_BaseEnv)); nprotect++;
            if (TYPEOF(x) != STRSXP)
                error("invalid 'x'");
            len = length(x);
            if (len <= 0) {
                UNPROTECT(nprotect);
                return allocVector(simplify ? STRSXP : VECSXP, 0);
            }
        }
        else {
            len = length(x);
            if (len <= 0) return allocVector(simplify ? STRSXP : VECSXP, 0);
            x = coerceVector(x, STRSXP);
            PROTECT(x); nprotect++;
        }
    }
    else {
        len = length(x);
        if (len <= 0) return allocVector(simplify ? STRSXP : VECSXP, 0);
    }


    SEXP env = asenv(envir, enclos, rho);
    PROTECT(env); nprotect++;


    PROTECT_INDEX indx;
    SEXP expr = R_NilValue;
    PROTECT_WITH_INDEX(expr, &indx);


    SEXP pattern, m, y, fmt, sprintf, value, mm, yy, vvalue;
    pattern = PROTECT(mkString(
        "((?:^|[^%])(?:%%)*%)(-*)(?:\\(([\\S\\s]*?)\\)|\\[([\\S\\s]*?)\\]|\\{([\\S\\s]*?)\\})\\2([^%]*?[aAdifeEgGosxX])"
    )); nprotect++;


    /* gregexec(pattern, x, perl = TRUE) */
    REPROTECT(expr = lang4(gregexecSymbol, pattern, x, ScalarLogical(TRUE)), indx);
    SET_TAG(CDDDR(expr), perlSymbol);
    m = eval(expr, R_BaseEnv);
    PROTECT(m); nprotect++;


    /* regmatches(x, m) */
    REPROTECT(expr = lang3(regmatchesSymbol, x, m), indx);
    y = eval(expr, R_BaseEnv);
    PROTECT(y); nprotect++;


    /* gsub(pattern, "\\1\\6", x, perl = TRUE) */
    REPROTECT(expr = lang5(gsubSymbol, pattern, mkString("\\1\\6"), x, ScalarLogical(TRUE)), indx);
    SET_TAG(nthcdr(expr, 4), perlSymbol);
    fmt = eval(expr, R_BaseEnv);
    PROTECT(fmt); nprotect++;


    sprintf = PROTECT(eval(sprintfSymbol, R_BaseEnv)); nprotect++;


    int i = 0, mm_indx, yy_indx, *imm;


    if (simplify && len == 1) {
        _f_str;
        UNPROTECT(nprotect);
        return vvalue;
    }


    value = PROTECT(allocVector(VECSXP, len)); nprotect++;
    for (i = 0; i < len; i++) {
        _f_str;
        SET_VECTOR_ELT(value, i, vvalue);
    }


    if (simplify) {
        Rboolean all_length_1 = TRUE;
        for (i = 0; i < len; i++) {
            if (length(VECTOR_ELT(value, i)) != 1) {
                all_length_1 = FALSE;
                break;
            }
        }
        if (all_length_1) {
            SEXP return_this = PROTECT(allocVector(STRSXP, len)); nprotect++;
            for (i = 0; i < len; i++)
                SET_STRING_ELT(return_this, i, STRING_ELT(VECTOR_ELT(value, i), 0));
            UNPROTECT(nprotect);
            return return_this;
        }
    }


    UNPROTECT(nprotect);
    return value;
}

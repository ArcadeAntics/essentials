#include <R.h>
#include <Rinternals.h>


void delayedAssign(SEXP value, SEXP eval_env, SEXP assign_env)
{
    SEXP expr = PROTECT(lang5(
        install("delayedAssign"),
        mkString("x"),
        value,
        eval_env,
        assign_env
    ));
    eval(expr, R_BaseEnv);
    UNPROTECT(1);
}


SEXP do_mkPROMISE(SEXP value, SEXP env)
{
    if (TYPEOF(env) != ENVSXP)
        error("invalid 'env', must be an environment");
    SEXP assign_env = PROTECT(R_NewEnv(R_EmptyEnv, TRUE, 1));
    delayedAssign(value, env, assign_env);
    SEXP val = findVarInFrame(assign_env, install("x"));
    UNPROTECT(1);
    return val;
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


SEXP do_as_env(SEXP envir, SEXP enclos, SEXP context)
{
    int np = 0, frame;
    SEXP value = envir;


    SEXPTYPE tEnclos = TYPEOF(enclos);
    if (isNull(enclos)) {
        enclos = R_BaseEnv;
    } else if (!isEnvironment(enclos) &&
               !isEnvironment((enclos = simple_as_environment(enclos)))) {
        error("invalid '%s' argument of type '%s'",
            "enclos", type2char(tEnclos));
    }


    if (IS_S4_OBJECT(value) && (TYPEOF(value) == S4SXP))
        value = R_getS4DataSlot(value, ANYSXP);
    switch (TYPEOF(value)) {
    case NILSXP:
        value = enclos;
    case ENVSXP:
        PROTECT(value); np++;
        break;
    case LISTSXP:
        value = list2env(envir, enclos);
        PROTECT(value); np++;
        break;
    case VECSXP:
        value = list2env(envir, enclos);
        PROTECT(value); np++;
        break;
    case INTSXP:
    case REALSXP:
        if (length(value) != 1)
            error("numeric 'envir' arg not of length one");
        frame = asInteger(value);
        if (frame == NA_INTEGER)
            error("invalid '%s' argument of type '%s'",
                "envir", type2char(TYPEOF(value)));
        if (TYPEOF(context) != ENVSXP)
            error("invalid '%s' argument of type '%s'",
                "context", type2char(TYPEOF(context)));
        PROTECT(value = eval(lang2(install("sys.frame"), ScalarInteger(frame)), context)); np++;
        break;
    default:
        error("invalid '%s' argument of type '%s'",
            "envir", type2char(TYPEOF(value)));
    }


    UNPROTECT(np);
    return value;
}


SEXP do_f_str_old(SEXP sprintf, SEXP fmt, SEXP exprs, SEXP envir)
{
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





#define _f_str                                                         \
    {                                                                  \
        REPROTECT(expr = R_NilValue, indx);                            \
        yy = VECTOR_ELT(y, i);                                         \
        if (length(yy) > 0) {                                          \
            mm = VECTOR_ELT(m, i);                                     \
            imm = INTEGER(mm);                                         \
            for (mm_indx = length(mm) - 2; mm_indx >= 0; mm_indx -= 7) {\
                if ( (!imm[mm_indx]) + (!imm[mm_indx - 1]) + (!imm[mm_indx - 2]) != 2)\
                    error("invalid 'm'; should never happen, please report!");\
                if (imm[yy_indx = mm_indx]) {}                         \
                else if (imm[--yy_indx]) {}                            \
                else if (imm[--yy_indx]) {}                            \
                else error("invalid 'm'; should never happen, please report!");\
                tmp2 = PROTECT(ScalarString(STRING_ELT(yy, yy_indx))); \
                tmp2 = PROTECT(lang2(install("str2expression"), tmp2));\
                tmp = PROTECT(eval(tmp2, R_BaseEnv));                  \
                switch (length(tmp)) {                                 \
                case 1:                                                \
                    REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 0), expr), indx);\
                    break;                                             \
                case 2:                                                \
                    REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 1), expr), indx);\
                    REPROTECT(expr = LCONS(VECTOR_ELT(tmp, 0), expr), indx);\
                    break;                                             \
                default:                                               \
                    error("parsing result not of length one or two, but %d", length(tmp));\
                }                                                      \
                UNPROTECT(3);                                          \
            }                                                          \
        }                                                              \
        REPROTECT(expr = LCONS(ScalarString(STRING_ELT(fmt, i)), expr), indx);\
        REPROTECT(expr = LCONS(sprintf, expr), indx);                  \
        vvalue = eval(expr, rho);                                      \
    }





SEXP do_f_str(SEXP x, SEXP rho, SEXP simplify)
{
    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    Rboolean lsimplify = asLogical(simplify);
    if (lsimplify == NA_LOGICAL)
        error("invalid 'simplify' argument");


    int np = 0;
    SEXP tmp, tmp2;


    if (TYPEOF(x) != STRSXP) {
        if (isObject(x)) {
            tmp = PROTECT(lang2(install("as.character"), lang2(install("quote"), x))); np++;
            x = PROTECT(eval(tmp, R_BaseEnv)); np++;
            if (TYPEOF(x) != STRSXP)
                error("invalid 'x'");
        }
        else { x = PROTECT(coerceVector(x, STRSXP)); np++; }
    }


    int len = length(x);
    if (lsimplify && len <= 0) {
        UNPROTECT(np);
        return allocVector(STRSXP, 0);
    }


    PROTECT_INDEX indx;
    SEXP expr = R_NilValue;
    PROTECT_WITH_INDEX(expr, &indx); np++;


    SEXP pattern, m, y, fmt, sprintf, value, mm, yy, vvalue;
    pattern = PROTECT(mkString(
        "((?:^|[^%])(?:%%)*%)(-*)(?:\\(([\\S\\s]*?)\\)|\\[([\\S\\s]*?)\\]|\\{([\\S\\s]*?)\\})\\2([^%]*?[aAdifeEgGosxX])"
    )); np++;


    /* gregexec(pattern, x, perl = TRUE) */
    REPROTECT(expr = LCONS(ScalarLogical(1), expr), indx);
    SET_TAG(expr, install("perl"));
    REPROTECT(expr = LCONS(x, expr), indx);
    REPROTECT(expr = LCONS(pattern, expr), indx);
    REPROTECT(expr = LCONS(install("gregexec"), expr), indx);
    m = PROTECT(eval(expr, R_BaseEnv)); np++;


    REPROTECT(expr = R_NilValue, indx);


    /* regmatches(x, m) */
    REPROTECT(expr = LCONS(m, expr), indx);
    REPROTECT(expr = LCONS(x, expr), indx);
    REPROTECT(expr = LCONS(install("regmatches"), expr), indx);
    y = PROTECT(eval(expr, R_BaseEnv)); np++;


    REPROTECT(expr = R_NilValue, indx);


    /* gsub(pattern, "\\1\\6", x, perl = TRUE) */
    REPROTECT(expr = LCONS(ScalarLogical(1), expr), indx);
    SET_TAG(expr, install("perl"));
    REPROTECT(expr = LCONS(x, expr), indx);
    REPROTECT(expr = LCONS(mkString("\\1\\6"), expr), indx);
    REPROTECT(expr = LCONS(pattern, expr), indx);
    REPROTECT(expr = LCONS(install("gsub"), expr), indx);
    fmt = PROTECT(eval(expr, R_BaseEnv)); np++;


    sprintf = PROTECT(eval(install("sprintf"), R_BaseEnv)); np++;


    int i = 0, mm_indx, yy_indx, *imm;


    if (lsimplify && len == 1) {
        _f_str
        UNPROTECT(np);
        return vvalue;
    }


    value = PROTECT(allocVector(VECSXP, len)); np++;
    for (i = 0; i < len; i++) {
        _f_str
        PROTECT(vvalue);
        SET_VECTOR_ELT(value, i, vvalue);
        UNPROTECT(1);
    }


    if (lsimplify) {
        Rboolean all1 = 1;
        for (i = 0; i < len; i++) {
            if (length(VECTOR_ELT(value, i)) != 1) {
                all1 = 0;
                break;
            }
        }
        if (all1) {
            SEXP return_this = PROTECT(allocVector(STRSXP, len)); np++;
            for (i = 0; i < len; i++)
                SET_STRING_ELT(return_this, i, STRING_ELT(VECTOR_ELT(value, i), 0));
            UNPROTECT(np);
            return return_this;
        }
    }


    UNPROTECT(np);
    return value;
}

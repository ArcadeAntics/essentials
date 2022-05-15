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


SEXP do_f_str(SEXP sprintf, SEXP fmt, SEXP exprs, SEXP envir)
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

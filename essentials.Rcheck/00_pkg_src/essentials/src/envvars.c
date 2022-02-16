#include <R.h>
#include <Rinternals.h>


#define UNIMPLEMENTED_TYPE(X, Y) (error("unimplemented type '%s' in '%s'", type2char(TYPEOF(Y)), X))


#define Rprint(X) (eval(lang2(install("print"), lang2(install("quote"), X)), R_BaseEnv))


// #define debug


SEXP do_envvars(SEXP args, SEXP visible)
{
    SEXP value, names;


    if (TYPEOF(visible) != LGLSXP || LENGTH(visible) != 1)
        error("invalid '%s'", "visible");


    /* if zero arguments were provided */
    if (args == R_NilValue) {


#ifdef debug
        Rprintf("zero arguments were provided to '%s'", "envvars");
#endif


        SEXP expr = PROTECT(lang1(install("Sys.getenv")));
        value = PROTECT(eval(expr, R_BaseEnv));
        value = PROTECT(coerceVector(value, VECSXP));
        setAttrib(value, R_ClassSymbol, R_NilValue);
        LOGICAL(visible)[0] = 1;
        UNPROTECT(3);
        return value;
    }
    else if (TYPEOF(args) != LISTSXP)
        error("invalid '%s'", "args");


    int n    = length(args),
        vsbl = 0,
        np   = 0;
    if (n == 1 &&
        (isPairList(CAR(args)) || isVectorList(CAR(args))) &&
        TAG(args) == R_NilValue) {


#ifdef debug
        Rprintf("one argument, a list or a pairlist, without a tag was provided to '%s'", "envvars");
#endif


        args = CAR(args);
        n = length(args);
    }


    SEXP argnames = R_NilValue;
    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
        break;
    case VECSXP:
        if (n > 0) {
            argnames = getAttrib(args, R_NamesSymbol); np++;
        }
        break;
    default:
        UNIMPLEMENTED_TYPE("envvars", args);
    }


    if (n <= 0) {
        value = PROTECT(allocVector(VECSXP, 0)); np++;
        names = PROTECT(allocVector(STRSXP, 0)); np++;
        setAttrib(value, R_NamesSymbol, names);
        LOGICAL(visible)[0] = vsbl;
        UNPROTECT(np);
        return value;
    }


    SEXP temp_args;


    int nset = 0, nunset = 0;
    PROTECT(names = allocVector(STRSXP, n)); np++;
    switch (TYPEOF(args)) {
    case LISTSXP:
        temp_args = args;
        for (int i = 0; i < n; i++, temp_args = CDR(temp_args)) {
            if (TAG(temp_args) != R_NilValue) {
                SET_STRING_ELT(names, i, asChar(TAG(temp_args)));
                if (asChar(CAR(temp_args)) == NA_STRING)
                    nunset++;
                else nset++;
            }
            else {
                vsbl = 1;
                SET_STRING_ELT(names, i, asChar(CAR(temp_args)));
            }
        }
        break;
    case VECSXP:
        if (argnames == R_NilValue) {
            for (int i = 0; i < n; i++) {
                vsbl = 1;
                SET_STRING_ELT(names, i, asChar(VECTOR_ELT(args, i)));
            }
        }
        else for (int i = 0; i < n; i++) {
            if (*CHAR(STRING_ELT(argnames, i))) {
                SET_STRING_ELT(names, i, STRING_ELT(argnames, i));
                if (asChar(VECTOR_ELT(args, i)) == NA_STRING)
                    nunset++;
                else nset++;
            }
            else {
                vsbl = 1;
                SET_STRING_ELT(names, i, asChar(VECTOR_ELT(args, i)));
            }
        }
        break;
    default:
        UNIMPLEMENTED_TYPE("envvars", args);
    }


#ifdef debug
    Rprintf("the names of the environment variables are:\n");
    Rprint(names);
    Rprintf("\n");
#endif


    SEXP expr;
    PROTECT(expr = lang4(
        install("Sys.getenv"),
        names,
        ScalarString(NA_STRING),
        ScalarLogical(1)
    ));                                               np++;


#ifdef debug
    Rprintf("the expression to get the environment variables is:\n");
    Rprint(expr);
    Rprintf("\n");
#endif


    PROTECT(value = eval(expr, R_BaseEnv));           np++;


#ifdef debug
    Rprintf("the expression evaluated to:\n");
    Rprint(value);
    Rprintf("\n");
#endif


    PROTECT(value = coerceVector(value, VECSXP));     np++;
    setAttrib(value, R_ClassSymbol, R_NilValue);


#ifdef debug
    Rprintf("the object was coerced to:\n");
    Rprint(value);
    Rprintf("\n");
#endif


    if (nset || nunset) {
        SEXP expr2;
        PROTECT(expr  = allocVector(LANGSXP, nset + 1)); SEXP temp_setenv = CDR(expr);             np++;
        PROTECT(expr2 = allocVector(LANGSXP, 2));                                                  np++;
        SETCAR(expr , install("Sys.setenv"));
        SETCAR(expr2, install("Sys.unsetenv"));
        SEXP unsetenv_arg;
        PROTECT(unsetenv_arg = allocVector(STRSXP, nunset));                                       np++;
        int j = 0;
        switch (TYPEOF(args)) {
        case LISTSXP:
            for (SEXP temp_args = args; temp_args != R_NilValue; temp_args = CDR(temp_args)) {
                if (TAG(temp_args) == R_NilValue) {}
                else if (asChar(CAR(temp_args)) == NA_STRING) {
                    SET_STRING_ELT(unsetenv_arg, j, asChar(TAG(temp_args)));
                    j++;
                }
                else {
                    SET_TAG(temp_setenv, TAG(temp_args));
                    SETCAR(temp_setenv, ScalarString(asChar(CAR(temp_args))));
                    temp_setenv = CDR(temp_setenv);
                }
            }
            break;
        case VECSXP:
            for (int i = 0; i < n; i++) {
                if (!(*CHAR(STRING_ELT(argnames, i)))) {}
                else if (asChar(VECTOR_ELT(args, i)) == NA_STRING) {
                    SET_STRING_ELT(unsetenv_arg, j, asChar(STRING_ELT(argnames, i)));
                    j++;
                }
                else {
                    SET_TAG(temp_setenv, installTrChar(STRING_ELT(argnames, i)));
                    SETCAR(temp_setenv, ScalarString(asChar(VECTOR_ELT(args, i))));
                    temp_setenv = CDR(temp_setenv);
                }
            }
            break;
        default:
            UNIMPLEMENTED_TYPE("envvars", args);
        }
        SETCADR(expr2, unsetenv_arg);


        if (nset) {


#ifdef debug
            Rprintf("the expression to set the environment variables is:\n");
            Rprint(expr);
            Rprintf("\n");
#endif


            eval(expr, R_BaseEnv);
        }
        if (nunset) {


#ifdef debug
            Rprintf("the expression to unset the environment variables is:\n");
            Rprint(expr2);
            Rprintf("\n");
#endif


            eval(expr2, R_BaseEnv);
        }
    }


    LOGICAL(visible)[0] = vsbl;
    UNPROTECT(np);
    return value;
}


SEXP do_getEnvvar(SEXP x, SEXP default_)
{
    SEXP
        Sys_getenv = PROTECT(install("Sys.getenv")),
        xx         = PROTECT(ScalarString(asChar(x))),
        unset      = PROTECT(ScalarString(asChar(default_))),
        names      = PROTECT(ScalarLogical(0));
    SEXP value = eval(
        lang4(Sys_getenv, xx, unset, names),
        R_BaseEnv
    );
    UNPROTECT(4);
    return value;
}

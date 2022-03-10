#include <R.h>
#include <Rinternals.h>


#define UNIMPLEMENTED_TYPE(X, Y) (error("unimplemented type '%s' in '%s'", type2char(TYPEOF(Y)), X))


#define Rprint(X) (eval(lang2(install("print"), lang2(install("quote"), X)), R_BaseEnv))


// #define debug





SEXP do_envvars(SEXP args, SEXP visible)
{
    SEXP value, names;


    /* visible is TRUE of FALSE depending on whether the result 'value' should
       be returned visibly or invisibly from the R function */
    if (TYPEOF(visible) != LGLSXP || LENGTH(visible) != 1)
        error("invalid '%s'", "visible");


    /* if (zero arguments were provided) */
    if (args == R_NilValue) {


#ifdef debug
        Rprintf("zero arguments were provided to '%s'", "envvars");
#endif


        /*
            if zero arguments were provided, the user is requesting all the
            environment variables. do to this, build and evaluate the following:

            Sys.getenv()

            then remove its class attribute, and change 'visible' to TRUE
         */
        SEXP expr = PROTECT(lang1(install("Sys.getenv")));
        value = PROTECT(eval(expr, R_BaseEnv));
        value = PROTECT(coerceVector(value, VECSXP));
        setAttrib(value, R_ClassSymbol, R_NilValue);
        LOGICAL(visible)[0] = 1;
        UNPROTECT(3);
        return value;
    }


    /* if the user provided arguments, they should form a pairlist */
    else if (TYPEOF(args) != LISTSXP)
        error("invalid '%s'", "args");


    int n    = length(args),  // number of arguments in 'args'
        vsbl = 0,             // should the result be returned visibly?
        np   = 0;             // number of SEXP to UNPROTECT at the end


    /*
        if (the user provided 1 (unnamed) argument
            which is either a list or pairlist)
     */
    if (n == 1 &&
        (isPairList(CAR(args)) || isVectorList(CAR(args))) &&
        TAG(args) == R_NilValue) {


#ifdef debug
        Rprintf("one argument, a list or a pairlist, without a tag was provided to '%s'", "envvars");
#endif


        /*
            then use that argument as 'args'
            don't forget to re-calculate the number of arguments in 'args'
         */
        args = CAR(args);
        n = length(args);
    }


    /* if 'args' is a list, grab the attribute 'names' */
    SEXP argnames = R_NilValue;
    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
        break;
    case VECSXP:
        if (n > 0) {
            argnames = PROTECT(getAttrib(args, R_NamesSymbol)); np++;
        }
        break;
    default:
        UNIMPLEMENTED_TYPE("envvars", args);
    }


    /* if 'args' has no arguments */
    if (n <= 0) {


        /* return an empty named list, invisibly */
        value = PROTECT(allocVector(VECSXP, 0)); np++;
        names = PROTECT(allocVector(STRSXP, 0)); np++;
        setAttrib(value, R_NamesSymbol, names);
        LOGICAL(visible)[0] = vsbl;
        UNPROTECT(np);
        return value;
    }


    SEXP temp_args;


    /*
        go through 'args', figuring out how many environment variables
        will be set and how many will be unset
    */
    int nset = 0, nunset = 0;
    PROTECT(names = allocVector(STRSXP, n)); np++;
    switch (TYPEOF(args)) {
    case LISTSXP:
        temp_args = args;


        /* loop through each element of 'args' */
        for (int i = 0; i < n; i++, temp_args = CDR(temp_args)) {


            /*
                if (current element is named)
                (setting or unsetting an environment variable)
            */
            if (TAG(temp_args) != R_NilValue) {


                /*
                    we are either setting or unsetting a variable. in either
                    scenario, the name of the environment variable being set or
                    unset is the current element's name (tag). we need this
                    since we have to return the previous values of the
                    environment variables (so the user has an easy time
                    reverting these changes, if desired)
                */
                SET_STRING_ELT(names, i, asChar(TAG(temp_args)));


                /*
                    we are unsetting an environment variable
                    if the current element is NA_character_
                 */
                if (asChar(CAR(temp_args)) == NA_STRING)
                    nunset++;
                else nset++;
            }


            /* getting an environment variable */
            else {


                /*
                    in this scenario, the name of the environment variable is
                    the current element, NOT its name
                */
                vsbl = 1;
                SET_STRING_ELT(names, i, asChar(CAR(temp_args)));
            }
        }
        break;


    /* same ideas but for a list this time, above was a pairlist */
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


    /*
        get the values of all the requested environment variables using:

        Sys.getenv(names, NA_character_, TRUE)

        coerce to a list and remove its class
     */
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


    /* (possibly) set or unset the requested environment variables */
    if (nset || nunset) {


        /*
            we need to build and execute calls to Sys.setenv and Sys.unsetenv

            for Sys.setenv, we need to build something like:

            Sys.setenv(
                "environment variable name 1" = "environment variable value 1",
                "environment variable name 2" = "environment variable value 2",
                ...
            )

            to do this, we need a "language" vector of length 'nset + 1', the
            extra one for the name of the function, Sys.setenv

            for Sys.unsetenv, we need to build something like:

            Sys.unsetenv(
                c(
                    "environment variable name 1",
                    "environment variable name 2",
                    ...
                )
            )

            to do this, we need a "lanuage" vector of length 2, one for the
            function name Sys.unsetenv, and one for the character vector of
            arguments. additionally, allocate a character vector of length 'nunset'
         */


        PROTECT(expr = allocVector(LANGSXP, nset + 1));  np++;
        SETCAR(expr, install("Sys.setenv"));  // set the first element to the function name Sys.setenv
        SEXP temp_setenv = CDR(expr);  // the next element of 'expr'


        SEXP expr2 = PROTECT(allocVector(LANGSXP, 2));  np++;
        SETCAR(expr2, install("Sys.unsetenv"));
        SEXP unsetenv_arg = PROTECT(allocVector(STRSXP, nunset));  np++;
        int j = 0;  /*
            we don't have a way to keep track of the next element of
            'unsetenv_arg', so we keep track of which element is next to set
         */
        switch (TYPEOF(args)) {
        case LISTSXP:


            /* loop through each argument in 'args' */
            for (SEXP temp_args = args; temp_args != R_NilValue; temp_args = CDR(temp_args)) {


                /* if the current element is unnamed, do nothing*/
                if (TAG(temp_args) == R_NilValue) {}


                /*
                    if the current element is NA_character_,
                    add its name to 'unsetenv_args'
                */
                else if (asChar(CAR(temp_args)) == NA_STRING) {
                    SET_STRING_ELT(unsetenv_arg, j, asChar(TAG(temp_args)));
                    j++;
                }


                /*
                    otherwise, add the name and value to the current element,
                    and skip to the next element
                */
                else {
                    SET_TAG(temp_setenv, TAG(temp_args));
                    SETCAR(temp_setenv, ScalarString(asChar(CAR(temp_args))));
                    temp_setenv = CDR(temp_setenv);
                }
            }
            break;


        /* same thing, but it 'args' is a list instead of a pairlist */
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


        /* set and unset the environment variables */
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


    /*
        set the visibility of the return value
        so the R function knows what to do
    */
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

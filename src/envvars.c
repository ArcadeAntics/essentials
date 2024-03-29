#include <R.h>
#include <Rinternals.h>
#include "defines.h"
#include "translations.h"


// #define debug





SEXP do_envvars(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP Sys_getenvSymbol = NULL,
                Sys_setenvSymbol = NULL,
                Sys_unsetenvSymbol = NULL;
    if (Sys_getenvSymbol == NULL) {
        Sys_getenvSymbol = install("Sys.getenv");
        Sys_setenvSymbol = install("Sys.setenv");
        Sys_unsetenvSymbol = install("Sys.unsetenv");
    }


    SEXP value, names;
    int nprotect = 0;


    SEXP dots = findVarInFrame(rho, R_DotsSymbol);
    if (dots == R_UnboundValue)
        error("could not find the ... list; should never happen, please report!");


    int dots_length = dotsLength(dots);


    /* zero arguments were provided */
    if (dots_length == 0) {


#ifdef debug
        Rprintf("zero arguments were provided to '%s'\n", "envvars");
#endif


        /*
            if zero arguments were provided, the user is requesting all the
            environment variables. do to this, build and evaluate the following:

            Sys.getenv()

            then remove its class attribute, and change 'visible' to TRUE
         */
        SEXP expr = PROTECT(lang1(Sys_getenvSymbol)); nprotect++;
        value = PROTECT(eval(expr, R_BaseEnv)); nprotect++;
        value = PROTECT(coerceVector(value, VECSXP)); nprotect++;
        setAttrib(value, R_ClassSymbol, R_NilValue);
        /* we want to return visibily, and since we didn't eval anything from the ... list, we know it will return visibly */
        UNPROTECT(nprotect);
        return value;
    }


    /* eval all arguments in ... list, copy to separate pairlist */
    args = PROTECT(allocVector(LISTSXP, dots_length)); nprotect++;
    SEXP a = args,
         d = dots,
         x;
    for (; d != R_NilValue; a = CDR(a), d = CDR(d)) {
        SET_TAG(a, TAG(d));
        x = CAR(d);
        x = eval(x, rho);
        SETCAR(a, x);
    }


    int n = length(args),  // number of arguments in 'args'
        visible = 0;       // should the result be returned visibly?


    /*
     * the user provided 1 (unnamed) argument
     * which is either a list or pairlist
     */
    if (n == 1 &&
        (isPairList(CAR(args)) || isVectorList(CAR(args))) &&
        TAG(args) == R_NilValue) {


#ifdef debug
        Rprintf("one argument, a list or a pairlist, without a tag was provided to '%s'\n", "envvars");
#endif


        /*
            then use that argument as 'args'
            don't forget to re-calculate the number of arguments in 'args'
         */
        args = CAR(args);
        n = length(args);
    }


    /* if 'args' has no arguments */
    if (n <= 0) {
        /* return an empty named list, invisibly */
        value = PROTECT(allocVector(VECSXP, 0)); nprotect++;
        names = PROTECT(allocVector(STRSXP, 0)); nprotect++;
        setAttrib(value, R_NamesSymbol, names);
        set_R_Visible(visible);
        UNPROTECT(nprotect);
        return value;
    }


    /* if 'args' is a list, grab the attribute 'names' */
    SEXP argnames = R_NilValue;
    if (isPairList(args));
    else if (isVectorList(args)) {
        if (n > 0) {
            argnames = PROTECT(getAttrib(args, R_NamesSymbol)); nprotect++;
        }
    }
    else UNIMPLEMENTED_TYPE("envvars", args);


    SEXP temp_args;


    /*
        go through 'args', figuring out how many environment variables
        will be set and how many will be unset
    */
    int nset = 0, nunset = 0;
    PROTECT(names = allocVector(STRSXP, n)); nprotect++;
    if (isPairList(args)) {
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
                visible = 1;
                SET_STRING_ELT(names, i, asChar(CAR(temp_args)));
            }
        }
    }
    else if (isVectorList(args)) {
        if (argnames == R_NilValue) {
            for (int i = 0; i < n; i++) {
                visible = 1;
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
                visible = 1;
                SET_STRING_ELT(names, i, asChar(VECTOR_ELT(args, i)));
            }
        }
    }
    else UNIMPLEMENTED_TYPE("envvars", args);


#ifdef debug
    Rprintf("the names of the environment variables are:\n");
    R_print(names);
    Rprintf("\n");
#endif


    /*
        get the values of all the requested environment variables using:

        Sys.getenv(names, NA_character_, TRUE)

        coerce to a list and remove its class
     */
    SEXP expr;
    PROTECT(expr = lang4(
        Sys_getenvSymbol,
        names,
        ScalarString(NA_STRING),
        ScalarLogical(1)
    ));                                               nprotect++;


#ifdef debug
    Rprintf("the expression to get the environment variables is:\n");
    R_print(expr);
    Rprintf("\n");
#endif


    PROTECT(value = eval(expr, R_BaseEnv));           nprotect++;


#ifdef debug
    Rprintf("the expression evaluated to:\n");
    R_print(value);
    Rprintf("\n");
#endif


    PROTECT(value = coerceVector(value, VECSXP));     nprotect++;
    setAttrib(value, R_ClassSymbol, R_NilValue);


#ifdef debug
    Rprintf("the object was coerced to:\n");
    R_print(value);
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

            to do this, we need a "language" vector of length 2, one for the
            function name Sys.unsetenv, and one for the character vector of
            arguments. additionally, allocate a character vector of length 'nunset'
         */


        PROTECT(expr = allocVector(LANGSXP, nset + 1));  nprotect++;
        SETCAR(expr, Sys_setenvSymbol);  // set the first element to the function name Sys.setenv
        SEXP temp_setenv = CDR(expr);  // the next element of 'expr'


        SEXP expr2 = PROTECT(allocVector(LANGSXP, 2));  nprotect++;
        SETCAR(expr2, Sys_unsetenvSymbol);
        SEXP unsetenv_arg = PROTECT(allocVector(STRSXP, nunset));  nprotect++;
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


        /* same thing, but if 'args' is a list instead of a pairlist */
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
            R_print(expr);
            Rprintf("\n");
#endif


            eval(expr, R_BaseEnv);
        }
        if (nunset) {


#ifdef debug
            Rprintf("the expression to unset the environment variables is:\n");
            R_print(expr2);
            Rprintf("\n");
#endif


            eval(expr2, R_BaseEnv);
        }
    }


    /*
        set the visibility of the return value
        so the R function knows what to do
    */
    set_R_Visible(visible);
    UNPROTECT(nprotect);
    return value;
}


SEXP do_getenvvar(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    static SEXP Sys_getenvSymbol = NULL,
                defaultSymbol = NULL;
    if (Sys_getenvSymbol == NULL) {
        Sys_getenvSymbol = install("Sys.getenv");
        defaultSymbol = install("default");
    }


    SEXP x = CADR(args);
    if (TYPEOF(x) != STRSXP || LENGTH(x) != 1)
        error(_("'%s' must be a character string"), "x");
    SEXP expr = PROTECT(lang4(
        Sys_getenvSymbol,
        isObject(x) ? ScalarString(STRING_ELT(x, 0)) : x,
        ScalarString(NA_STRING),
        ScalarLogical(FALSE)
    ));
    SEXP value = PROTECT(eval(expr, R_BaseEnv));
    if (STRING_ELT(value, 0) == NA_STRING) {
        UNPROTECT(2);
        return eval(defaultSymbol, rho);
    }
    else {
        UNPROTECT(2);
        return value;
    }
}

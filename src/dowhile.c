#include <R.h>
#include <Rinternals.h>


// #define debug


#include "getFromBase.h"


#include "defines.h"  // includes R_print() and set_R_Visible()





SEXP dowhile(SEXP call, SEXP op, SEXP args, SEXP rho, int until)
{
    int nprotect = 0;


    /* expr is an (unevaluated) expression, the body of the do while/until loop
       cond is an (unevaluated) expression, the condition of the do while/until loop */


    SEXP expr = findVarInFrame(rho, install("expr"));
    if (expr == R_UnboundValue)
        error("something is wrong with 'dowhile'");
    expr = PREXPR(expr);


    SEXP cond = findVarInFrame(rho, install("cond"));
    if (cond == R_UnboundValue)
        error("something is wrong with 'dowhile'");
    cond = PREXPR(cond);


    SEXP assign_in_place = PROTECT(eval(install("assign.in.place"), rho)); nprotect++;


    SEXP parent_frame;
    parent_frame = PROTECT(lang1(install("parent.frame"))); nprotect++;
    parent_frame = PROTECT(eval(parent_frame, rho)); nprotect++;


    const char *fun = until ? "%until%" : "%while%";


    /* the user will have no access to this variable, not even using
     * sys.status(), so we can safely update it in place
     *
     * we MUST use allocVector here, can't use ScalarLogical(TRUE), that would
     * return the already allocated TRUE R value, and we can't go changing that
     *
     * we must allocate our own memory to update in place
     */
    SEXP skip = PROTECT(allocVector(LGLSXP, 1)); nprotect++;
    LOGICAL(skip)[0] = TRUE;


    /* we're looking for an expression of the form
       do ( expr )
       do ( { expr1 ; expr2 } ) */
    if (TYPEOF(expr) != LANGSXP || CAR(expr) != install("do"))
        error("invalid 'expr', must be wrapped with do()\n  instead of: expr %s (cond)\n  try:        do(expr) %s (cond)",
            fun, fun);
    else if (xlength(expr) != 2) {
        if (xlength(expr) == 1)
            error("invalid 'expr', 0 expressions within do() which requires 1");
        else error("invalid 'expr', %d expressions within do() which requires 1\n  instead of: do(expr1, expr2) %s (cond)\n  try:        do({\n                  expr1\n                  expr2\n              }) %s (cond)",
            xlength(expr) - 1, fun, fun);
    }


    /* if there is a tag on the second item (the expression within do()),
       that means the parser has interpreted

       do(var = expr)

       as tag 'var' with value 'expr' to function 'do', but really we want
       'var = expr' as the argument to 'do'

       so we artificially create what the user wanted to do, instead of what
       they actually typed. we could insist that they use '<-' instead or that
       they must wrap calls to '=' with parenthesis, but we know what they
       meant, so might as well fix it quietly */
    else if (!isNull(TAG(CDR(expr)))) {
        expr = PROTECT(lang3(
            install("="),
            TAG(CDR(expr)),
            CADR(expr)
        )); nprotect++;
        /*
        error("invalid 'expr', do not name the expression within do()\n  if you intended to use '=' within do() like:\n\n  do(var = expr) %s (cond)\n\n  use '<-' instead or wrap with parenthesis like:\n\n  do(var <- expr) %s (cond)\n  do((var = expr)) %s (cond)",
            fun, fun, fun);
         */
    }


    /* select the expression within do() */
    else {
        expr = PROTECT(CADR(expr)); nprotect++;
    }


    /* we're looking for an expression of the form
       ( expr )
       ( { expr1 ; expr2 } ) */
    if (TYPEOF(cond) != LANGSXP || CAR(cond) != install("("))
        error("invalid 'cond', must be wrapped with parenthesis\n  instead of: do(expr) %s cond\n  try:        do(expr) %s (cond)",
            fun, fun);
    else if (xlength(cond) != 2)
        error("invalid 'cond', %d expressions within '()' which requires 1",
            xlength(cond) - 1);


    /* select the expression within '()' */
    cond = CADR(cond);


    /* if, for WHATEVER reason, the expression or condition are the missing
       argument, signal an error */
    if (expr == R_MissingArg)
        error("invalid 'expr', missing with no default");
    if (cond == R_MissingArg)
        error("invalid 'cond', missing with no default");


    /* we signal an error for the following:

       do ({
           expr1
           expr2
       }) %while% (var = expr)
                       ^

       in 'if' and 'while' conditions, you cannot use '=' at the top level:

       if (var = expr) expr1
       while (var = expr) expr1

       both signal an error with message "unexpected '=' in ...". you can
       use '<-' (the most preferable) or wrap with '()' like:

       if (var <- expr) expr1
       while ((var = expr)) expr1

       even though:

       do ({
           expr1
           expr2
       }) %while% (var = expr)

       works perfectly fine, i want it to parse identically */
    if (TYPEOF(cond) == LANGSXP && CAR(cond) == install("="))
        error("invalid 'cond', unexpected '='\n  if you intended to use '=' within 'cond' like:\n\n  do(expr) %s (var = cond)\n\n  use '<-' instead or wrap with parenthesis like:\n\n  do(expr) %s (var <- cond)\n  do(expr) %s ((var = cond))",
            fun, fun, fun);


    /* we make a repeat loop that will emulate a do while/until loop
       i transferred this to C because i needed to not fuck up the call stack
       by using eval at the R level. for example:


       fun <- function (verbose = FALSE, implementation = c("C", "R"))
       {
           `%until%` <- switch(match.arg(implementation), C = {
               essentials::`%until%`
           }, R = {
               essentials:::`%ountil%`
           })
           do ({
               if (verbose)
                   print(sys.calls())
               return(TRUE)
           }) %until% (TRUE)
           return(FALSE)
       }


       # returns TRUE, as expected
       fun(implementation = "C")


       # returns FALSE, counter-intuitively
       fun(implementation = "R")


       # you can try re-running
       fun(verbose = TRUE, implementation = "C")
       fun(verbose = TRUE, implementation = "R")
       # and you should see that the call stack is different between the two.
       # this is why 'return' does not behave as expected. 'return' is evaluated
       # in the context of 'eval' instead of in the context of 'R_implementation' */


    /* we are attempting to build the following call (with 'aenv', 'name',
       'cond', and 'expr' substituted appropriately):


       # do until loop
       repeat {
           if (aenv[[name]])
               assign(name, FALSE, aenv)
           else if (cond)
               break
           expr
       }


       # do while loop
       repeat {
           if (aenv[[name]])
               assign(name, FALSE, aenv)
           else if (cond) {
           }
           else break
           expr
       }
     */
    PROTECT(expr = lang2(
        getFromBase(install("repeat")),
        lang3(
            getFromBase(R_BraceSymbol),
            lang4(
 /* if   */     getFromBase(install("if")),
 /* cond */     skip,
 /* expr */     lang3(
                    assign_in_place,
                    skip,
                    ScalarLogical(FALSE)
                ),
 /* alt.expr */ until ? lang3(
                    getFromBase(install("if")),
                    cond,
                    lang1(getFromBase(install("break")))) :


                /* we could use

                   lang3(install("if"), lang2(install("!"), cond),
                       lang1(install("break")))


                   but we don't want to do that in cases where 'cond' evaluates to:
                   * an object that is not length one
                   * a string ("T", "TRUE", "True", "true", "F", "FALSE", "False", "false")
                   * a raw byte (since '!' is defined differently for that class)
                   * any other classed objects (might have a method for '!') */
                lang4(
                    getFromBase(install("if")),
                    cond,
                    lang1(getFromBase(R_BraceSymbol)),
         /* else */ lang1(getFromBase(install("break")))
                )
            ),
            expr
        )
    )); nprotect++;
#ifdef debug
    Rprintf("\n> loop_expr\n");
    R_print(expr);
    error("stopped for debugging purposes");
    return R_NilValue;
#endif


    eval(expr, parent_frame);
    set_R_Visible(0);
    UNPROTECT(nprotect);
    return R_NilValue;
}


/* do(expr) %while% (cond) */
SEXP do_dowhile(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return dowhile(call, op, args, rho, 0);
}


/* do(expr) %until% (cond) */
SEXP do_dountil(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return dowhile(call, op, args, rho, 1);
}

#include <R.h>
#include <Rinternals.h>


/* do(expr) %while% (cond) */
/* do(expr) %until% (cond) */
SEXP do_dowhile(SEXP expr, SEXP cond, SEXP until, SEXP rho)
{
    /* expr is an (unevaluated) expression, the body of the do while/until loop
       cond is an (unevaluated) expression, the condition of the do while/until loop
       until is TRUE/FALSE, indicating if this is a do while/until loop
       rho is an environment in which to evaluate the do while/until loop body and condition */


    int u = asLogical(until);
    if (u == NA_LOGICAL)
        error("invalid 'until'");
    const char *fun = u ? "%until%" : "%while%";


    if (TYPEOF(rho) != ENVSXP)
        error("invalid 'rho'");


    /* a temporary environment, holds a value indicating if
       the do while/until loop has been evaluated once or not */
    SEXP aenv = PROTECT(R_NewEnv(R_EmptyEnv, TRUE, 1));


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
    else if (!isNull(TAG(CDR(expr))))
        expr = PROTECT(lang3(
            install("="),
            TAG(CDR(expr)),
            CADR(expr)
        ));
        /*
        error("invalid 'expr', do not name the expression within do()\n  if you intended to use '=' within do() like:\n\n  do(var = expr) %s (cond)\n\n  use '<-' instead or wrap with parenthesis like:\n\n  do(var <- expr) %s (cond)\n  do((var = expr)) %s (cond)",
            fun, fun, fun);
         */


    /* select the expression within do() */
    else expr = PROTECT(CADR(expr));


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
    if (TYPEOF(expr) == SYMSXP && expr == R_MissingArg)
        error("invalid 'expr', missing with no default");
    if (TYPEOF(cond) == SYMSXP && cond == R_MissingArg)
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


    /* we need to define a temporary variable in 'aenv' that will keep track
       of whether its the first time through the loop */
    const char *name = "skip";
    defineVar(install(name), ScalarLogical(TRUE), aenv);


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
        install("repeat"),
        lang3(
            R_BraceSymbol,
            lang4(
 /* if   */     install("if"),
 /* cond */         lang3(
                    R_Bracket2Symbol,
                    aenv,
                    mkString(name)
                ),
 /* expr */     lang4(
                    install("assign"),
                    mkString(name),
                    ScalarLogical(FALSE),
                    aenv
                ),
 /* alt.expr */ u ? lang3(
                    install("if"),
                    cond,
                    lang1(install("break"))) :


                /* we could use

                   lang3(install("if"), lang2(install("!"), cond),
                       lang1(install("break")))


                   but we don't want to do that in cases where 'cond' evaluates to:
                   * an object that is not length one
                   * a string ("T", "TRUE", "True", "true", "F", "FALSE", "False", "false")
                   * a raw byte (since '!' is defined differently for that class)
                   * any other classed objects (might have a method for '!') */
                lang4(
                    install("if"),
                    cond,
                    lang1(R_BraceSymbol),
         /* else */ lang1(install("break"))
                )
            ),
            expr
        )
    ));


    // UNPROTECT(3); return expr;


    eval(expr, rho);
    UNPROTECT(3);
    return R_NilValue;
}

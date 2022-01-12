utils::globalVariables("do")


# old implementation of do while and do until loops ----


`%owhile%` <- `%while%` <- function (expr, cond)
do.while(substitute(expr), substitute(cond), FALSE, parent.frame())


`%ountil%` <- `%until%` <- function (expr, cond)
do.while(substitute(expr), substitute(cond), TRUE, parent.frame())


do.while <- function (expr, cond, until = FALSE, envir = parent.frame())
{
    if (!is.call(expr) || expr[[1L]] != quote(do))
        stop(if (until)
            "do until loop must begin with 'do'"
        else "do while loop must begin with 'do'")
    else if (length(expr) != 2L)
        stop("invalid 'expr'")


    if (!is.call(cond) || cond[[1L]] != quote(`(`))
        stop("'cond' must be wrapped with parenthesis")
    else if (length(cond) != 2L)
        stop("invalid 'cond'")


    expr <- expr[[2L]]
    cond <- cond[[2L]]


    if (is.call(expr) && expr[[1L]] == quote(`{`)) {}
    else expr <- call("{", expr)


    eval(call("repeat", as.call(c(
        as.list(expr),
        if (until)
            call("if", cond, call("break"))


        # we could use `call("if", call("!", cond), call("break"))`,
        # but we don't want to do that in cases where `cond` evaluates to:
        # * an object that is not length one
        # * a string ("T", "TRUE", "True", "true", "F", "FALSE", "False", "false")
        # * a raw byte (since `!` is defined differently for that class)
        else call("if", cond, call("{"), call("break"))
    ))), envir = envir)
}


# new implementation of do while and do until loops ----


`%while%` <- function (expr, cond)
invisible(.Call(C_do.while, substitute(expr), substitute(cond),
    FALSE, parent.frame()))


`%until%` <- function (expr, cond)
invisible(.Call(C_do.while, substitute(expr), substitute(cond),
    TRUE, parent.frame()))

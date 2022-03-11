utils::globalVariables("do")


# old implementation of do while and do until loops ----


do.while <- function (until)
{
    fun <- evalq(function(expr, cond) NULL, parent.frame())
    body(fun) <- bquote({
        expr <- substitute(expr)
        cond <- substitute(cond)
        if (!is.call(expr) || expr[[1L]] != quote(do))
            stop(.(if (until)
                "do until loop must begin with 'do'"
                else "do while loop must begin with 'do'"))
        else if (length(expr) != 2L)
            stop("invalid 'expr'")


        if (!is.call(cond) || cond[[1L]] != quote(`(`))
            stop("'cond' must be wrapped with parenthesis")
        else if (length(cond) != 2L)
            stop("invalid 'cond'")


        expr <- expr[[2L]]
        cond <- cond[[2L]]


        assign.env <- new.env(parent = emptyenv(), size = 1)
        assign.env[["skip"]] <- TRUE
        eval(bquote(repeat {
            if (.(quote(.(assign.env)))[["skip"]])
                assign("skip", FALSE, .(quote(.(assign.env))))
            else .({
                if (until)
                    quote(if (.(cond)) break)
                else quote(if (.(cond)) {} else break)
            })
            .(quote(.(expr)))
        }), parent.frame())
    })
    fun
}


`%owhile%` <- do.while(until = FALSE)


`%ountil%` <- do.while(until = TRUE)


rm(do.while)


# new implementation of do while and do until loops ----


`%while%` <- function (expr, cond)
invisible(.Call(C_do.while, substitute(expr), substitute(cond),
    FALSE, parent.frame()))


`%until%` <- function (expr, cond)
invisible(.Call(C_do.while, substitute(expr), substitute(cond),
    TRUE, parent.frame()))

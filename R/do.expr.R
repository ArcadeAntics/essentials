quoteLang <- function (cl)
{
    switch(typeof(cl), symbol = {
        as.call(list(quote, cl))
    }, language = {
        if (inherits(cl, "formula"))
            cl
        else as.call(list(quote, cl))
    }, cl)
}


do.expr <- function (expr)
{
    sexpr <- substitute(expr)
    if (!is.call(sexpr) || length(sexpr) <= 1)
        return(expr)
    what <- sexpr[[1L]]
    sargs <- as.list(sexpr)[-1L]
    has.tag <- nzchar(names(sargs))
    args <- vector("list", length(sargs))
    names(args) <- names(sargs)
    envir <- parent.frame()
    for (i in seq_along(sargs)) {
        if (identical(sarg <- sargs[[i]], quote(expr = ))) {
            args[[i]] <- list(quote(expr = ))
            next
        }
        else if (is.call(sarg) && identical(sarg[[1L]], quote(`*`)) && length(sarg) == 2L) {
            if (has.tag[[i]])
                stop("do not name arguments which are being unpacked")
            arg <- eval(sarg[[2L]], envir)
            tmp <- vector("list", length(arg))
            for (j in seq_along(tmp))
                tmp[j] <- list(quoteLang(arg[[j]]))
            args[[i]] <- tmp
        }
        else if (is.call(sarg) && identical(sarg[[1L]], quote(`**`)) && length(sarg) == 2L) {
            if (has.tag[[i]])
                stop("do not name arguments which are being unpacked")
            arg <- eval(sarg[[2L]], envir)
            tmp <- vector("list", length(arg))
            for (j in seq_along(tmp))
                tmp[j] <- list(quoteLang(arg[[j]]))
            names(tmp) <- names(arg)
            args[[i]] <- tmp
        }
        else args[[i]] <- list(sarg)
    }
    expr <- as.call(c(list(what), unlist(args, recursive = FALSE)))
    eval(expr, envir)
}


x <- 1:3
names(x) <- letters[1:3]
do.expr( base::list(k = 1:4, `*`(x), `**`(x), what = "testing", quote(y)) )

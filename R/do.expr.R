quoteLang <- function (cl, quote.MissingArg = FALSE)
{
    if (isMissingArg(cl)) {
        if (quote.MissingArg)
            as.call(list(quote, quote(expr = )))
        else quote(expr = )
    }
    else switch(typeof(cl), symbol = {
        as.call(list(quote, cl))
    }, language = {
        if (inherits(cl, "formula"))
            cl
        else as.call(list(quote, cl))
    }, cl)
}


# do.expr <- function (expr)
# {
#     sexpr <- substitute(expr)
#     if (!is.call(sexpr) || length(sexpr) <= 1)
#         return(expr)
#     what <- sexpr[[1L]]
#     sargs <- as.list(sexpr)[-1L]
#     has.tag <- if (!is.null(nms <- names(sargs)))
#         nzchar(nms)
#     else logical(length(sargs))
#     args <- vector("list", length(sargs))
#     names(args) <- names(sargs)
#     envir <- parent.frame()
#     for (i in seq_along(sargs)) {
#         args[[i]] <- if (isMissingArg(sarg <- sargs[[i]])) {
#             list(quote(expr = ))
#         }
#         else if (is.call(sarg) && identical(sarg[[1L]], quote(`*`)) && length(sarg) == 2L) {
#             if (has.tag[[i]])
#                 stop("do not name arguments which are being unpacked")
#             arg <- eval(sarg[[2L]], envir)
#             if (!is.object(arg) && ((atom <- is.atomic(arg)) || is.vector(arg) || is.pairlist(arg))) {
#                 if (!is.null(names(arg)))
#                     names(arg) <- NULL
#                 if (atom)
#                     as.list(arg)
#                 else lapply(as.list(arg), quoteLang)
#             }
#             else {
#                 tmp <- vector("list", length(arg))
#                 for (j in seq_along(tmp))
#                     tmp[j] <- list(quoteLang(arg[[j]]))
#                 tmp
#             }
#         }
#         else if (is.call(sarg) && identical(sarg[[1L]], quote(`**`)) && length(sarg) == 2L) {
#             if (has.tag[[i]])
#                 stop("do not name arguments which are being unpacked")
#             arg <- eval(sarg[[2L]], envir)
#             if (!is.object(arg) && ((atom <- is.atomic(arg)) || is.vector(arg) || is.pairlist(arg))) {
#                 if (atom)
#                     as.list(arg)
#                 else lapply(as.list(arg), quoteLang)
#             }
#             else {
#                 tmp <- vector("list", length(arg))
#                 for (j in seq_along(tmp))
#                     tmp[j] <- list(quoteLang(arg[[j]]))
#                 if (!is.null(names(arg)))
#                     names(tmp) <- names(arg)
#                 tmp
#             }
#         }
#         else list(sarg)
#     }
#     expr <- as.call(c(list(what), unlist(args, recursive = FALSE)))
#     eval(expr, envir)
# }


do.expr <- function (expr)
{
    sexpr <- substitute(expr)
    if (!is.call(sexpr))
        expr
    else .Call(C_do.expr, sexpr, parent.frame())
}


##x <- 1:3
##names(x) <- letters[1:3]
##do.expr( base::list(k = 1:4, `*`(x), `**`(x), what = "testing", quote(y)) )

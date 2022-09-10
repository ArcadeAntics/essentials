`%<-%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .Call(C_unpackset, sys.call(sys.nframe()), `<-`, substitute(x), value, parent.frame())
    invisible(value)
}


`%<<-%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .Call(C_unpackset, sys.call(sys.nframe()), `<<-`, substitute(x), value, parent.frame())
    invisible(value)
}


`%->%` <- `%<-%`
formals(`%->%`) <- rev(formals(`%->%`))


`%->>%` <- `%<<-%`
formals(`%->>%`) <- rev(formals(`%->>%`))


`%=%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .Call(C_unpackset, sys.call(sys.nframe()), `=`, substitute(x), value, parent.frame())
    invisible(value)
}


unpack.assign <- function (x, value, evaluated = TRUE)
{
    value  # we need to force 'value' immediately
    .Call(C_unpackset, sys.call(sys.nframe()), `<-`, if (evaluated) x else substitute(x), value, parent.frame())
    invisible(value)
}


unpack.super.assign <- function (x, value, evaluated = TRUE)
{
    value  # we need to force 'value' immediately
    .Call(C_unpackset, sys.call(sys.nframe()), `<<-`, if (evaluated) x else substitute(x), value, parent.frame())
    invisible(value)
}


# .fun <- function (call, op, name, e, value, rho)
# {
#     if (identical(e, quote(expr = ))) {
#     }
#     else if (is.call(e) && is.symbol(e[[1L]])) {
#         if (e[[1L]] == "list") {
#             e <- as.list(e)[-1L]
#             starred_indx <- which(vapply(e, function(ee) is.call(ee) && is.symbol(ee[[1L]]) && ee[[1L]] == "*" && length(ee) <= 2L, NA))
#             if (length(starred_indx) > 1L)
#                 stop(errorCondition("multiple starred expressions in assignment", call = call))
#             else if (length(starred_indx) == 1L) {
#                 nstarred <- length(value) - length(e) + 1L
#                 if (nstarred < 0L)
#                     stop(errorCondition(sprintf("not enough values to unpack (expected at least %.0f, got %.0f)",
#                         length(e) - 1L, length(value)), call = call))
#
#
#                 # expressions before starred expression
#                 for (indx in seq_len(starred_indx - 1L))
#                     .fun(base::call(name, e[[indx]], value[[indx]]), op, name, e[[indx]], value[[indx]], rho)
#
#
#                 # starred expression
#                 ee <- e[[starred_indx]]
#                 if (length(ee) < 2L) {
#                 } else {
#                     ee <- ee[[2L]]
#                     indxs <- seq.int(starred_indx, length.out = nstarred)
#                     .fun(base::call(name, ee, value[indxs]), op, name, ee, value[indxs], rho)
#                 }
#
#
#                 # expressions after starred expression
#                 indxs <- seq.int(to = length(e), length.out = length(e) - starred_indx)
#                 shift <- nstarred - 1L
#                 for (indx in indxs)
#                     .fun(base::call(name, e[[indx]], value[[indx + shift]]), op, name, e[[indx]], value[[indx + shift]], rho)
#             }
#             else {
#                 if (length(e) != length(value))
#                     stop(errorCondition(sprintf("%s values to unpack (expected %.0f, got %.0f)",
#                         if (length(value) < length(e)) "not enough" else "too many", length(e), length(value)), call = call))
#                 for (indx in seq_along(e))
#                     .fun(base::call(name, e[[indx]], value[[indx]]), op, name, e[[indx]], value[[indx]], rho)
#             }
#         } else if (e[[1L]] == "*" && length(e) == 2L) {
#             stop("can only use starred expression inside list()", call. = FALSE)
#         } else eval(as.call(list(op, e, as.call(list(quote, value)))), rho)
#     }
#     else eval(as.call(list(op, e, as.call(list(quote, value)))), rho)
# }


# test.fun <- function (expr)
# {
#     expr <- substitute(expr)
#     local({
#         eval(expr, environment())
#         as.list(environment(), all.names = TRUE, sorted = TRUE)
#     })
# }
#
#
# test.fun( list(NULL, `*`(x)) %<-% 1 )
#
#
# test.fun( list(NULL, `*`(x)) %<-% 1:200 )
#
#
# test.fun( list(NULL) %<-% 1 )
#
#
# test.fun( list(a, b, NULL, `*`(NULL), w, x, y, z) %<-% 1:100 )
#
#
# test.fun( list(`*`(a), z) %<-% 1:6 )
#
#
# test.fun( list(`*`(x)) %<-% 1:6 )
#
#
# test.fun( list(a, b, c, d, NULL, z) %<-% 1:6 )
#
#
# test.fun( list(a, b, c, d, e, f) %<-% 1:6 )
#
#
# test.fun( list(a, b, NULL, `*`(m), w, x, y, z) %<-% 1:100 )
#
#
# test.fun({
#     m <- list(NULL)
#     list(a, b, NULL, `*`(m[[1]]), w, x, y, z) %<-% 1:100
# })
#
#
# test.fun({
#     m <- list(NULL)
#     list(a, b, NULL, `*`(m[[1]]), w, x, y, z) %<-% 1:10
# })
#
#
# test.fun({
#     m <- list(NULL)
#     list(a, b, NULL, `*`(m[[1]]), w, x, y, z) %<-% 1:7
# })
#
#
# test.fun({
#     m <- list(NULL)
#     list(a, b, NULL, `*`(m[[1]]), z) %<-% 1:5
# })
#
#
# test.fun({
#     z <- list(NULL)
#     list(a, b, NULL, `*`(z[[1]])) %<-% 1:5
# })
#
#
# test.fun({
#     m <- list(NULL, NULL)
#     list(a, b, `*`(m[[1]]), z) %<-% 1:5
# })
#
#
# test.fun( list(a, b, `*`(m), z) %<-% 1:5 )
#
#
# test.fun( list(a, b, `*`(m), z) %<-% list(1, 2, 3, 4, 5) )
#
#
# test.fun( list(x, y, `*`(m), x, y, z) %<-% list(1, 2, 3, 4, 5) )
#
#
# test.fun( list(`*`(a), x, y, z) %<-% list(1, 2, 3, 4, 5) )
#
#
# test.fun( list(`*`(a), v, w, x, y, z) %<-% list(1, 2, 3, 4, 5) )
#
#
# stopifnot(identical(
#     test.fun( list(`*`(a), x, y, z) %<-% list(1, 2, 3, 4, 5, 6) ),
#     list(a = list(1, 2, 3), x = 4, y = 5, z = 6)
# ))

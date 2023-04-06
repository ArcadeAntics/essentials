`%<-%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .External2(C_unpackset, substitute(x), `<-`, value)
}


`%<<-%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .External2(C_unpackset, substitute(x), `<<-`, value)
}


`%->%` <- `%<-%`
formals(`%->%`) <- rev(formals(`%->%`))


`%->>%` <- `%<<-%`
formals(`%->>%`) <- rev(formals(`%->>%`))


`%=%` <- function (x, value)
{
    value  # we need to force 'value' immediately
    .External2(C_unpackset, substitute(x), `=`, value)
}


unpack.assign <- function (x, value, evaluated = TRUE)
{
    value  # we need to force 'value' immediately
    .External2(C_unpackset, if (evaluated) x else substitute(x), `<-`, value)
}


unpack.super.assign <- function (x, value, evaluated = TRUE)
{
    value  # we need to force 'value' immediately
    .External2(C_unpackset, if (evaluated) x else substitute(x), `<<-`, value)
}





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

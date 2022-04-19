.duplicated.data.frame_transform <- function (x)
{
    i <- seq_len(nrow(x))
    attributes(x) <- NULL
    fun <- function(xx) {
        if (is.object(xx)) {
            if (inherits(xx, "factor")) xx <- `dim<-`(as.integer(xx), dim(xx))
            else if (inherits(xx, "POSIXlt"))
                xx <- as.character(xx)
        }
        if (length(dim(xx)) == 2L)
            as.character(lapply(i, function(ii) xx[ii, ]))
        else xx
    }
    if (length(x) != 1L)
        as.character(.mapply(list, lapply(x, "fun"), NULL))
    else fun(x[[1L]])
}


rowmatch <- function (x, table, nomatch = NA_integer_, incomparables = NULL)
{
    if (is.null(x)) return(integer())


    match_transform <- function(xx) {
        if (is.object(xx)) {
            if (inherits(xx, "factor")) xx <- `dim<-`(as.character.factor(xx), dim(xx))
            else if (inherits(xx, "POSIXlt")) {
                xx <- as.character(xx)
            }
        }
        if (length(dim(xx)) <= 2L)
            as.matrix(xx)
        else as.array(xx)
    }


    x <- match_transform(x)


    if (!nrow(x)) return(integer())


    nomatch <- as.scalar.integer(nomatch)


    if (is.null(table)) return(rep(nomatch, nrow(x)))


    table <- match_transform(table)


    # if 'table' has no rows
    #
    # or
    #
    # 'x' and 'table' are not conformable
    #
    # then return 'nomatch'
    if (!nrow(table) || length(dim(x)) != length(dim(table)) ||
        any(dim(x)[-1L] != dim(table)[-1L]))
        return(rep(nomatch, nrow(x)))


    type <- .Call(C_match.type, x, table)


    # format for use in 'match'
    fun <- function(xx) {
        asplit(`dim<-`(as.vector(xx, type), dim(xx)), 1L)
    }


    if (!is.null(incomparables) && !isFALSE(incomparables)) {
        incomparables <- match_transform(incomparables)

        # if 'incomparables' has no rows
        # or 'x' and 'incomparables' are not conformable
        # set 'incomparables' to NULL
        if (!nrow(incomparables) || length(dim(x)) != length(dim(incomparables)) ||
            any(dim(x)[-1L] != dim(incomparables)[-1L]))
            incomparables <- NULL
        else incomparables <- fun(incomparables)
    }
    else incomparables <- NULL
    x <- fun(x)
    table <- fun(table)
    match(x = x, table = table, nomatch = nomatch, incomparables = incomparables)
}


methods::setGeneric(
    name      = "row.match",
    signature = c("x", "table"),
    def       = as.function(c(formals(rowmatch), local({
        nf <- names(formals(rowmatch))
        x <- lapply(nf, base::as.symbol)
        names(x) <- nf
        as.call(c(list(quote(rowmatch)), x))
    })))
)


# row.match.data.frame <- function (x, table, nomatch = NA_integer_, incomparables = NULL)
# {
#     if (is.null(x))
#         return(integer())
#
#     x <- as.data.frame(x)
#
#     if (!nrow(x))
#         return(integer())
#
#     nomatch <- as.scalar.integer(nomatch)
#
#     if (is.null(table))
#         return(rep(nomatch, nrow(x)))
#
#     table <- as.data.frame(table)
#
#     # if 'table' has no rows
#     # or 'x' and 'table' are not conformable
#     # return 'nomatch'
#     if (!nrow(table) || ncol(x) != ncol(table))
#         return(rep(nomatch, nrow(x)))
#
#     fun <- function(x) {
#         matrixlike <- lengths(lapply(x, "dim")) == 2L
#         if (any(matrixlike)) {
#             i <- seq_len(nrow(x))
#             attributes(x) <- NULL
#             x[matrixlike] <- lapply(x[matrixlike], function(xx) {
#                 rownames(xx) <- NULL
#                 lapply(i, function(ii) xx[ii, , drop = FALSE])
#             })
#         }
#         else attributes(x) <- NULL
#         .mapply(list, x, NULL)
#     }
#
#     if (!is.null(incomparables) && !isFALSE(incomparables)) {
#         incomparables <- as.data.frame(incomparables)
#
#         # if 'incomparables' has no rows
#         # or 'x' and 'incomparables' are not conformable
#         # set 'incomparables' to NULL
#         if (!nrow(incomparables) || ncol(x) != ncol(incomparables))
#             incomparables <- NULL
#         else incomparables <- fun(incomparables)
#     }
#     x <- fun(x)
#     table <- fun(table)
#     match(x = x, table = table, nomatch = nomatch, incomparables = incomparables)
# }


row.match.data.frame <- function (x, table, nomatch = NA_integer_, incomparables = NULL)
.Call(C_row.match.data.frame, x, table, nomatch, incomparables, environment())


methods::setMethod(
    f          = "row.match",
    signature  = c(
        x     = "data.frame",
        table = "data.frame"),
    definition = as.function(c(formals(row.match.data.frame), local({
        nf <- names(formals(row.match.data.frame))
        x <- lapply(nf, base::as.symbol)
        names(x) <- nf
        as.call(c(list(quote(row.match.data.frame)), x))
    })))
)


`%rowin%` <- function (x, table)
rowmatch(x, table, nomatch = 0L) > 0L


`%row.in%` <- function (x, table)
row.match(x, table, nomatch = 0L) > 0L

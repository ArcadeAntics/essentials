.plapply <- function (X, FUN, dots = NULL)
{
    FUN <- match.fun(FUN)
    .Call(C_plapply, X, FUN, FALSE, environment())
}


plapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    .Call(C_plapply, X, FUN, TRUE, environment())
}


.psapply <- function (X, FUN, dots = NULL, simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    value <- .plapply(X = X, FUN = FUN, dots = dots)
    if (USE.NAMES && is.null(names(value))) {
        for (i in which(lengths(X) == length(value))) {
            if (is.character(X[[i]])) {
                names(value) <- X[[i]]
                break
            }
        }
    }
    if (!isFALSE(simplify))
        simplify2array(value, higher = (simplify == "array"))
    else value
}


psapply <- function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    value <- plapply(X = X, FUN = FUN, ...)
    if (USE.NAMES && is.null(names(value))) {
        for (i in which(lengths(X) == length(value))) {
            if (is.character(X[[i]])) {
                names(value) <- X[[i]]
                break
            }
        }
    }
    if (!isFALSE(simplify))
        simplify2array(value, higher = (simplify == "array"))
    else value
}


.pvapply <- function (X, FUN, FUN.VALUE, dots = NULL, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    .Call(C_pvapply, X, FUN, FUN.VALUE, USE.NAMES, FALSE, environment())
}


pvapply <- function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    .Call(C_pvapply, X, FUN, FUN.VALUE, USE.NAMES, TRUE, environment())
}


# plapply <- function (X, FUN, DOTS = NULL)
# {
#     FUN <- match.fun(FUN)
#     if (typeof(X) != "list")
#         stop("invalid 'X', must be a list")
#     if (!is.null(DOTS) && typeof(DOTS) != "list")
#         stop("invalid 'DOTS', must be a list")
#     if (!length(X))
#         list()
#     else if (!all(lengths(X))) {
#         value <- list()
#         if (any(vapply(X, function(xx) !is.null(names(xx)), NA)))
#             names(value) <- character()
#         value
#     }
#     else {
#         value <- .plapply(FUN, X, DOTS)
#         i <- lengths(X) == length(value) &
#             vapply(X, function(xx) !is.null(names(xx)), NA)
#         if (i <- match(TRUE, i, nomatch = 0L))
#             names(value) <- names(X[[i]])
#         value
#     }
# }
#
#
# psapply <- function (X, FUN, DOTS = NULL, simplify = TRUE, USE.NAMES = TRUE)
# {
#     value <- plapply(X, FUN, DOTS)
#     if (USE.NAMES && is.null(names(value))) {
#         i <- lengths(X) == length(value) & vapply(X, is.character, NA)
#         if (i <- match(TRUE, i, nomatch = 0L))
#             names(value) <- X[[i]]
#     }
#     if (!isFALSE(simplify))
#         simplify2array(value, higher = (simplify == "array"))
#     else value
# }
#
#
# pvapply <- function (X, FUN, FUN.VALUE, DOTS = NULL, USE.NAMES = TRUE)
# {
#     USE.NAMES <- if (USE.NAMES) TRUE else FALSE
#     if (!is.vector(FUN.VALUE))
#         stop("'FUN.VALUE' must be a vector")
#     commonLen <- length(FUN.VALUE)
#     commonType <- typeof(FUN.VALUE)
#     if (!commonType %in% c("complex", "double", "integer",
#         "logical", "raw", "character", "list"))
#         stop(gettextf("type '%s' is not supported", commonType))
#     dim_v <- dim(FUN.VALUE)
#     array_value <- length(dim_v) >= 1L
#     value <- plapply(X, FUN, DOTS)
#     n <- length(value)
#
#
#     lens <- lengths(value)
#     types <- vapply(value, typeof, "")
#     bad <- rbind(
#         length = lens != commonLen,
#         type   = {
#             ok <- types == commonType
#             switch(commonType, complex = {
#                 ok <- ok | types %in% c("double", "integer", "logical")
#             }, double = {
#                 ok <- ok | types %in% c("integer", "logical")
#             }, integer = {
#                 ok <- ok | types == "logical"
#             })
#             !ok
#         }
#     )
#     if (i <- match(TRUE, bad, nomatch = 0L)) {
#         i <- arrayInd(i, dim(bad))
#         j <- i[[2L]]
#         i <- i[[1L]]
#         s1 <- rownames(bad)[[i]]
#         s2 <- switch(s1, length = commonLen, type = commonType)
#         s4 <- switch(s1, length = lens[[j]], type = types[[j]])
#         stop(gettextf("values must be %s %s,\n but plapply(X, FUN, DOTS)[[%d]] result is %s %s",
#             s1, s2, j, s1, s4))
#     }
#     if (USE.NAMES) {
#         i <- lengths(X) == n
#         j <- i & vapply(X, function(xx) !is.null(names(xx)), NA)
#         Names <- if (j <- match(TRUE, j, nomatch = 0L))
#             names(X[[j]])
#         else {
#             j <- i & vapply(X, is.character, NA)
#             if (j <- match(TRUE, j, nomatch = 0L))
#                 X[[j]]
#         }
#         if (commonLen != 1L) {
#             if (array_value) {
#                 rowNames <- dimnames(FUN.VALUE)
#                 if (is.null(rowNames)) {
#                     i <- vapply(value, function(xx) {
#                         !is.null(dimnames(xx)) &&
#                             length(dim(xx)) == length(dim_v) &&
#                             all(dim(xx) == dim_v)
#                     }, NA)
#                     if (i <- match(TRUE, i, nomatch = 0L))
#                         rowNames <- dimnames(value[[i]])
#                 }
#             }
#             else {
#                 rowNames <- names(FUN.VALUE)
#                 if (is.null(rowNames)) {
#                     i <- vapply(value, function(xx) !is.null(names(xx)), NA)
#                     if (i <- match(TRUE, i, nomatch = 0L))
#                         rowNames <- names(value[[i]])
#                 }
#             }
#         }
#     }
#     value <- unlist(value, recursive = FALSE, use.names = FALSE)
#     if (is.null(value))
#         value <- as.vector(value, commonType)
#     if (length(value) != n * commonLen)
#         stop(gettext("'unlist' returned a vector of the wrong length; should never happen, please report!"))
#     if (typeof(value) != commonType)
#         stop(gettext("'unlist' returned a vector of the wrong type; should never happen, please report!"))
#
#
#     if (commonLen != 1L)
#         dim(value) <- c(if (array_value)
#             dim_v else
#         commonLen, n)
#
#
#     if (USE.NAMES) {
#         if (commonLen == 1L) {
#             if (!is.null(Names)) names(value) <- Names
#         } else {
#             if (!is.null(Names) || !is.null(rowNames)) {
#                 dimNames <- vector("list", i <- length(dim(value)))
#                 if (array_value && !is.null(rowNames))
#                     dimNames[-i] <- rowNames
#                 else dimNames[[1L]] <- rowNames
#                 dimNames[[i]] <- Names
#                 dimnames(value) <- dimNames
#             }
#         }
#     }
#     value
# }

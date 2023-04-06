.plapply <- function (X, FUN, dots = NULL)
{
    FUN <- match.fun(FUN)
    .External2(C_plapply, X, FUN, dots)
}


plapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    .External2(C_plapply, X, FUN)
}


.psapply <- function (X, FUN, dots = NULL, simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    value <- .External2(C_plapply, X, FUN, dots)
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
    value <- .External2(C_plapply, X, FUN)
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
    .External2(C_pvapply, X, FUN, FUN.VALUE, dots, USE.NAMES)
}


pvapply <- function (X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    .External2(C_pvapply, X, FUN, FUN.VALUE, USE.NAMES)
}

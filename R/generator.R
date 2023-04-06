# `if` <- function(...) {
#     cat("arguments to `if`\n")
#     print(substitute(list(...)))
# }


generator <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    lockEnvironment(environment(), bindings = TRUE)
    structure(function(i) FUN(X[[i]], ...), class = c("generator", "function"))
}


`[.generator` <- function (x, ...)
{
    x.envir <- environment(x)
    FUN <- X <- NULL
    value <- structure(function(i) FUN(X[[i]], ...), class = c("generator", "function"))
    environment(value) <- list2env(
        list(
            X = get("X", envir = x.envir, inherits = FALSE)[...],
            FUN = get("FUN", envir = x.envir, inherits = FALSE),
            ... = if (identical(dots <- x.envir[["..."]], quote(expr = )))
                quote(expr = )
            else if (typeof(dots) == "...")
                dots
            else stop("invalid ... list; should never happen, please report!")
        ),
        parent = parent.env(x.envir)
    )
    lockEnvironment(environment(value), bindings = TRUE)
    as.list.generator(value)
}


`[[.generator` <- function (x, ...)
x(..1)


as.list.generator <- function (x, ...)
lapply(seq_along(x), x)


length.generator <- function (x)
length(get("X", envir = environment(x), inherits = FALSE))


names.generator <- function (x)
names(get("X", envir = environment(x), inherits = FALSE))


# x <- generator(1:10, function(xx) if (xx >= 5) stop("test"))
# x[1:4]
# mfor(xx, x, print(xx))
#
#
# evalq(mfor(xx, 1:5, print(xx)), list(mfor = mfor, `:` = `:`, print = print), emptyenv())

as.scalar.logical <- function (x)
.Call(C_as.scalar.logical, x)


as.scalar.integer <- function (x)
.Call(C_as.scalar.integer, x)


as.scalar.real <- as.scalar.double <- as.scalar.numeric <- function (x)
.Call(C_as.scalar.real, x)


as.scalar.complex <- function (x)
.Call(C_as.scalar.complex, x)


as.scalar.number <- function (x, strict = TRUE)
.Call(C_as.scalar.number, x, strict)


as.scalar.string <- as.scalar.character <- function (x)
.Call(C_as.scalar.string, x)


as.scalar.raw <- function (x)
.Call(C_as.scalar.raw, x)


as.scalar <- function (x, mode = "any")
.Call(C_as.scalar, x, mode)


is.scalar <- function (x, mode = "any")
.Call(C_is.scalar, x, mode)


aslength1 <- function (x)
{
    if (!is.vector(x))
        x <- as.vector(x)
    len <- length(x)
    if (len == 1L) {
        x
    }
    else if (len > 1L) {
        warning(gettextf("first element used of '%s' argument",
            deparse(substitute(x), nlines = 1L)[1L], domain = NA))
        x[1L]
    }
    else stop(gettextf("'%s' must be of length 1", domain = NA,
        deparse(substitute(x), nlines = 1L)[1L]))
}

as.scalar.logical <- function (x)
.External2(C_asscalarlogical, x)


as.scalar.integer <- function (x)
.External2(C_asscalarinteger, x)


as.scalar.double <- as.scalar.numeric <- function (x)
.External2(C_asscalardouble, x)


as.scalar.complex <- function (x)
.External2(C_asscalarcomplex, x)


as.scalar.character <- as.scalar.string <- function (x)
.External2(C_asscalarcharacter, x)


as.scalar.raw <- function (x)
.External2(C_asscalarraw, x)


as.scalar.number <- function (x, strict = TRUE)
.External2(C_asscalarnumber, x, strict)





as.scalar <- function (x, mode = "any")
.External2(C_asscalar, x, mode)


is.scalar <- function (x, mode = "any")
.External2(C_isscalar, x, mode)


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
            deparse(substitute(x), nlines = 1L)[1L], domain = "R"))
        x[1L]
    }
    else stop(gettextf("'%s' must be of length 1", domain = "R",
        deparse(substitute(x), nlines = 1L)[1L]))
}

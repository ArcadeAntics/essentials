methods::setClassUnion(
    name = "numbers",
    members = c("numeric", "complex")
)


methods::setMethod(
    f = methods::coerce,
    signature = c(from = "ANY", to = "numbers"),
    definition = function (from, to, strict = TRUE)
{
    value <- as.numbers(from)
    if (strict)
        attributes(value) <- NULL
    value
})





numbers <- function (length = 0L)
numeric(length = length)


as.numbers <- function (x, ...)
UseMethod("as.numbers")


as.numbers.default <- function (x, strict = TRUE, ...)
.Call(C_as.numbers, if (missing(x)) NULL else x, strict)


is.numbers <- function (x)
UseMethod("is.numbers")


is.numbers.default <- function (x)
is.numeric(x) || is.complex(x)

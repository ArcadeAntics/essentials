library(methods)


methods::setClassUnion(
    name = "numbers",
    members = list(`attr<-`("numeric", "package", "methods"),
                   `attr<-`("complex", "package", "methods"))
)


coerce <- methods::coerce
methods::setMethod(
    f = coerce,
    signature = c(from = `attr<-`("ANY", "package", "methods"), to = "numbers"),
    definition = function (from, to, strict = TRUE)
{
    value <- as.numbers(from)
    if (strict)
        attributes(value) <- NULL
    value
})
rm(coerce)





numbers <- function (length = 0L)
numeric(length = length)


as.numbers <- function (x, ...)
UseMethod("as.numbers")


as.numbers.default <- function (x, strict = TRUE, ...)
.External2(C_asnumbers, if (missing(x)) NULL else x, strict)


is.numbers <- function (x)
UseMethod("is.numbers")


is.numbers.default <- function (x)
is.numeric(x) || is.complex(x)

methods::setClassUnion(
    name = "numbers",
    members = c("numeric", "complex")
)


methods::setMethod(
    f = "coerce",
    signature = c(from = "ANY", to = "numbers"),
    definition = function (from, to, strict = TRUE)
{
    value <- as.numbers(from)
    if (strict)
        attributes(value) <- NULL
    value
})


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





flat.list <- function (...)
{
    if (getOption("check.bounds")) {
        on.exit(options(check.bounds = TRUE))
        options(check.bounds = FALSE)
    }
    value <- list()
    i <- 0L
    names(value) <- names(rapply(list(...), f = function(X) {
        i <<- i + 1L
        value[[i]] <<- X
        NA
    }))
    return(value)
}


# hypot <- function (x, y)
# {
#     if (missing(y))
#         .Call(C_hypot, Re(x), Im(x))
#     else .Call(C_hypot, x, y)
# }


hypot <- function (..., na.rm = FALSE)
.External(C_hypot, na.rm, ...)


phypot <- function (..., na.rm = FALSE)
.External(C_phypot, na.rm, ...)


listify <- function (x)
if (inherits(x, "list")) x else list(x)


strip <- function (x)
gsub("^\\s+|\\s+$", "", x)


strsplit2 <- function (x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE,
    max = -1L, rm.last.empty.string = FALSE)
{
    # x <- c(NA_character_, "", "testing this out", "testing this ", " ")
    # split <- "\\s+"
    # strsplit (x, split)
    # strsplit2(x, split)
    # strsplit2(x, split, rm.last.empty.string = TRUE)


    if (!is.character(x))
        stop("non-character argument")
    if (is.na(fixed <- as.scalar.logical(fixed)) || !fixed) {
        fixed <- FALSE
        if (missing(split))
            split <- "\\s+"
    }
    split <- as.character(split)


    m <- gregexpr(split, x, perl = perl, fixed = fixed, useBytes = useBytes)
    if (!missing(max)) {
        max <- rep(as.integer(max), length.out = length(m))
        if (any(i <- max >= 0L))
            m[i] <- Map(function(so, maxi) {
                if ((n <- length(so)) == 1L) {
                    if (is.na(so) || so == -1L)
                        return(so)
                }
                if (n > maxi) {
                    structure(`length<-`(so, maxi),
                        match.length = `length<-`(attr(so, "match.length"), maxi),
                        index.type = attr(so, "index.type"))
                }
                else so
            }, m[i], max[i])
    }
    value <- regmatches(x, m, invert = TRUE)


    # we don't need to copy the names explicitly,
    # it is already done for us within 'regmatches'


    if (missing(rm.last.empty.string))
        return(value)
    rm.last.empty.string <- rep(as.logical(rm.last.empty.string),
        length.out = length(value))
    if (any(rm.last.empty.string)) {
        rm.last.empty.string <- rm.last.empty.string & lengths(value)
        if (any(rm.last.empty.string))
            value[rm.last.empty.string] <- lapply(value[rm.last.empty.string],
                function(valuei) {


                    # can't use 'valuei[[length(valuei)]] == ""'
                    # when valuei is NA_character_
                    # use '!nzchar(valuei[[length(valuei)]])' instead
                    if (!nzchar(valuei[[length(valuei)]]))
                        valuei[-length(valuei)]
                    else valuei
                })
    }
    value
}





IDW <- function (x0, u0, x, p = 2, na.rm = FALSE)
{
    if (!is.null(x0))
        x0 <- as.matrix(x0)
    u0 <- rep_len(u0, nrow(x0))
    if (!is.null(x))
        x <- as.matrix(x)
    if (na.rm) {
        isna <- is.na(u0) | apply(x0, 1L, "anyNA")
        x0 <- x0[!isna, , drop = FALSE]
        u0 <- u0[!isna]
    }
    if (is.complex(u0))
        .Call(C_IDW, x0, Re(u0), x, p) + 1i *
            .Call(C_IDW, x0, Im(u0), x, p)
    else .Call(C_IDW, x0, u0, x, p)
}


# RK4 <- function (independent, initialConditions, fun)
# .Call(C_RK4, independent, initialConditions, fun, environment())





isMissingArg <- function (x)
.Call(C_isMissingArg, substitute(x), parent.frame())

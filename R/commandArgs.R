.tag_pattern <- "([[:alpha:]][-.[:alnum:]_]*|\\.(?:[-.[:alpha:]_][-.[:alnum:]_]*)?)"
.value_pattern <- "(?:=(.*))"


.name_pattern <- paste0("^", .tag_pattern, "$")
.name_or_flag_pattern <- paste0("^-{0,2}", .tag_pattern, .value_pattern, "?$")
.name_or_flag_with_value_pattern <- paste0("^-{0,2}", .tag_pattern, .value_pattern, "$")
.flag_pattern <- paste0("^--?", .tag_pattern, .value_pattern, "?$")


.is_name <- function (x)
{
    grepl(.name_pattern, x)
}


.is_name_or_flag <- function (x)
{
    grepl(.name_or_flag_pattern, x)
}


.is_flag <- function (x)
{
    grepl(.flag_pattern, x)
}


.is_short_flag <- function (x)
{
    startsWith(x, "-")
}


.is_long_flag <- function (x)
{
    startsWith(x, "--")
}


.get_tag <- function (x)
{
    sub(.name_or_flag_pattern, "\\1", x)
}


.has_value <- function (x)
{
    grepl(.name_or_flag_with_value_pattern, x)
}


.get_value <- function (x)
{
    sub(.name_or_flag_pattern, "\\2", x)
}





.format_help <- function (x)
{
    x <- as.character(x)
    x[is.na(x)] <- ""
    x <- paste(x, collapse = "\n")
    gsub("^\\s*\n|\\s+$", "", x)
}


match.action <- function (action)
{
    action <- match.arg(action, c(
        "store"         ,
        "store_constant", "store.constant",
        "store_true"    , "store.true"    , "T", "TRUE" , "True" , "true" ,
        "store_false"   , "store.false"   , "F", "FALSE", "False", "false",
        "append"        ,
        "count"         ,
        "help"          ,
        "exit"          ,
        "skip"          )
    )
    if (action %in% c("store_constant", "store.constant"))
        "store_const"
    else if (action %in% c("store.true", "T", "TRUE", "True", "true"))
        "store_true"
    else if (action %in% c("store.false", "F", "FALSE", "False", "false"))
        "store_false"
    else action
}


parse.nargs <- function (nargs)
{
    if (is.null(nargs))
        return(NULL)
    validate <- function(x) {
        x <- floor(as.numeric(x))
        if (length(x) == 1L) {
            if (is.na(x))
                NULL
            else if (x <= 0)
                stop("invalid 'nargs' argument of length 1, must be positive")
            else if (x > .Machine$double.base^.Machine$double.digits - 1)
                stop(gettextf("invalid 'nargs' argument of length 1, must be less than %d^%d",
                    .Machine$double.base, .Machine$double.digits))
            else x
        }
        else if (length(x) == 2L) {
            if (is.na(x[1L]))
                x[1L] <- 0
            else if (x[1L] < 0)
                stop("invalid 'nargs' argument of length 2, first element must be positive")
            else if (x[1L] > .Machine$double.base^.Machine$double.digits - 1)
                stop(gettextf("invalid 'nargs' argument of length 2, first element must be less than %d^%d",
                    .Machine$double.base, .Machine$double.digits))
            if (is.na(x[2L]))
                x[2L] <- Inf
            else if (x[2L] == 0 || x[2L] < x[1L])
                stop("invalid 'nargs' argument of length 2, second element must be non-zero and not less than the first element")
            if (x[1L] == x[2L])
                x[1L]
            else x
        }
        else stop(sprintf("invalid 'nargs' argument of length %d, must be length 1 or 2",
            length(nargs)))
    }
    if (is.integer(args) || is.double(nargs) || is.logical(nargs) || is.complex(nargs))
        return(validate(nargs))
    nargs <- as.character(nargs)[1L]
    if (is.na(nargs))
        stop("invalid 'nargs' argument")
    if (nargs == "?")              return(c(0,   1))
    if (nargs %in% c("*", "{,}"))  return(c(0, Inf))
    if (nargs == "+")              return(c(1, Inf))


    if (grepl(pattern <- "^\\{([0123456789]+)}$", nargs))                  # {n}
        return(validate(sub(pattern, "\\1", nargs)))


    if (grepl(pattern <- "^\\{([0123456789]+),}$", nargs))                 # {n,}
        return(validate(c(sub(pattern, "\\1", nargs), Inf)))


    if (grepl(pattern <- "^\\{,([0123456789]+)}$", nargs))                 # {,m}
        return(validate(c(0, sub(pattern, "\\1", nargs))))


    if (grepl(pattern <- "^\\{([0123456789]+),([0123456789]+)}$", nargs))  # {n,m}
        return(validate(c(sub(pattern, "\\1", nargs), sub(pattern, "\\2", nargs))))


    stop(gettextf("invalid 'nargs' argument %s", encodeString(nargs, quote = "\"")))
}


nargs.description <- function (nargs, singular = "time", plural = NA)
{
    ngettextf <- function(n, fmt1, fmt2, ..., domain = NULL) {
        suppressWarnings(sprintf(ngettext(n == 1, fmt1, fmt2, domain = domain), ...))
    }
    if (!is.na(singular) && nzchar(singular))
        singular <- paste0(" ", singular)
    else singular <- ""
    if (!is.na(plural) && nzchar(plural))
        plural <- paste0(" ", plural)
    else if (nzchar(singular))
        plural <- paste0(singular, "s")
    else plural <- ""
    nargs <- parse.nargs(nargs)
    if (length(nargs) == 1)
        return(ngettextf(nargs, "exactly %.0f%2$s", "exactly %.0f%3$s", nargs, singular, plural))
    if (nargs[1L] == 0) {
        if (nargs[2L] == Inf)
            gettextf("0 or more%s", plural)
        else ngettextf(nargs[2L], "at most %.0f%2$s", "at most %.0f%3$s", nargs[2L], singular, plural)
    }
    else if (nargs[2L] == Inf) {
        ngettextf(nargs[1L], "at least %.0f%2$s", "at least %.0f%3$s", nargs[1L], singular, plural)
    }
    else paste(ngettextf(nargs[1L], "at least %.0f%2$s", "at least %.0f%3$s", nargs[1L], singular, plural),
        ngettextf(nargs[2L], "at most %.0f%2$s", "at most %.0f%3$s", nargs[2L], singular, plural), sep = ", ")
}


print.formalCommandArg <- function (x, ..., quote = FALSE)
{
    cat("A command-line argument with")
    y <- x[c("tags", "short.flags", "long.flags")]
    y <- y[lengths(y) > 0L]
    y <- lapply(y, function(yy) {
        paste0("\"", yy, "\"", collapse = ", ")
    })
    y <- c(y, x[c("action", "nargs", "type")])
    y$nargs <- nargs.description(y$nargs, singular = "")
    if (!is.null(x$choices))
        y <- c(y, list(choices = deparse1(x$choices, collapse = "")))
    if (!is.na(x$destination))
        y <- c(y, x["destination"])
    y <- c(y, x["metavariable"])
    storage.mode(y) <- "character"
    print.default(cbind(` ` = y), ..., quote = quote)
    if (length(x$group)) {
        cat("in group\n")
        print(vapply(x$group, `[[`, 0L, "value"))
    }
    if ("constant" %in% names(x)) {
        cat("with constant\n")
        print(x$constant)
    }
    if (!identical(x$default, quote(expr = ))) {
        cat("with default\n")
        print(x$default)
    }
    else cat("with no default\n")
    if (!is.null(x$help) && nzchar(x$help)) {
        cat("with help message:\n")
        cat(x$help, "\n")
    }
    else cat("with no help message\n")
    invisible(x)
}

##options(keep.source = TRUE)


as.env <- function (envir = parent.frame(2),
          enclos = if (is.list(envir) || is.pairlist(envir))
                       parent.frame(2) else baseenv())
.Call(C_as.env, envir, enclos, parent.frame(2))


f.str.old <- function (x, envir = parent.frame(),
             enclos = if (is.list(envir) || is.pairlist(envir))
                          parent.frame() else baseenv(),
    simplify = TRUE)
{
    # f.str                                                       R Docmentation
    #
    # Use C-Style String Formatting Commands with Interpolation
    #
    #
    #
    # Description:
    #
    # A variant of 'sprintf' where values are specified WITHIN the format
    # strings.
    #
    #
    #
    # Usage:
    #
    # f.str(x, envir = parent.frame(),
    #          enclos = if (is.list(envir) || is.pairlist(envir))
    #                       parent.frame() else baseenv(),
    #       simplify = TRUE)
    #
    #
    #
    # Arguments:
    #
    # x
    #
    #     a character vector of format strings.
    #
    # envir
    #
    #     the environment in which the values are to be evaluated. May also be
    #     NULL, a list, a data frame, a pairlist, or an integer as specified to
    #     'sys.call'.
    #
    # enclos
    #
    #     Relevant when envir is a (pair)list or a data frame. Specifies the
    #     enclosure, i.e., where R looks for objects not found in envir. This
    #     can be NULL (interpreted as the base package environment, 'baseenv()')
    #     or an environment.
    #
    # simplify
    #
    #     should the result be simplified to a character vector if possible?
    #
    #
    #
    #
    #
    # Details:
    #
    # Values in the format strings are specified in a similar manner to that of
    # raw character constants (see ?Quotes): (...) with ... any character
    # sequence, except that it must not contain the closing delimiter ')'. The
    # delimiter pairs [] and {} can also be used. For additional flexibility, a
    # number of dashes can be placed immediately before the opening delimiter,
    # as long as the same number of dashes appear immediately after the closing
    # delimiter.
    #
    # A field width or precision (but not both) may be indicated by an asterisk *:
    # see 'Examples' for the two methods of providing such an argument.
    #
    # These values should appear immediately before the % in the allowed
    # conversion specifications with the formatting specifications and type
    # immediately after.
    #
    #
    #
    #
    #
    # Value:
    #
    # For 'f.str(simplify = FALSE)', a list of the same length as 'x'.
    #
    # For 'f.str(simplify = TRUE)': if 'x' has length zero, 'character(0)'.
    # Otherwise, if 'x' is length one, the first element of the list.
    # Otherwise, if each element of the list is length one, the list is unlisted.
    # Otherwise, the list is returned as is.
    #
    #
    #
    # See Also:
    #
    # 'sprintf' for all of the allowed conversion specifications.
    #
    # 'glue::glue' for another method of string interpolation.
    #
    #
    #
    # Examples:
    #
    # name <- "Fred"
    # age <- 50
    # anniversary <- as.Date("1991-10-12")
    #
    # ## all 3 delimiter pairs:
    # f.str(x <- "My name is %(name)s, my age next year is %[age + 1]i, my anniversary is %{format(anniversary, '%A, %B %d, %Y')}s.")
    #
    # ## evaluate in a different environment:
    # f.str(x, list(name = "Joe", age = 40, anniversary = as.Date("2001-10-12")))
    #
    # ## use a literal % :
    #
    # f.str("%(66.666).0f%% said yes (out of a sample of size %(3).0f)")
    #
    # ## re-use one argument three times, show difference between %x and %X
    # xx <- f.str("%(0:15)d %1$x %1$X")
    # xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
    # noquote(format(xx, justify = "right"))
    #
    # ## Platform-dependent bad example from qdapTools 1.0.0:
    # ## may pad with spaces or zeroes.
    # f.str("%(month.name)09s")
    #
    # n <- 1:18
    # f.str(paste0("e with %(", n, ")2d digits = %[exp(1)].", n, "g"))
    #
    # ## Using asterisk for width or precision
    # f.str("precision %(3;pi).*f, width '%(8;pi)*.3f'")
    #
    # ## Asterisk and argument re-use, 'e' example reiterated:
    # f.str("e with %(n)2d digits = %[exp(1)].*1$g")
    #
    # ## re-cycle arguments
    # f.str("%('test')s %(1:3)d")
    #
    # ## binary output showing rounding/representation errors
    # x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
    # cbind(x, f.str("%(x)a"), f.str("%(y)a"))


    x <- as.character(x)
    envir <- as.env(envir, enclos)
    simplify <- if (simplify) TRUE else FALSE


    pattern <- "((?:^|[^%])(?:%%)*%)(-*)(?:\\(([\\S\\s]*?)\\)|\\[([\\S\\s]*?)\\]|\\{([\\S\\s]*?)\\})\\2([^%]*?[aAdifeEgGosxX])"
    #           ^^^^^^^^^^^^^^^^^^^^                                                                                            odd number of consecutive %
    #                               ^^^^                                                                                        a number of dashes
    #                                      ^^^                ^^^                ^^^                                            opening delimiters
    #                                         ^^^^^^^^^^^^       ^^^^^^^^^^^^       ^^^^^^^^^^^^                                any character sequence (non-greedy)
    #                                                     ^^^                ^^^                ^^^                             closing delimiters
    #                                                                                               ^^^                         same number of dashes
    #                                                                                                  ^^^^^^^^^^^^^^^^^^^^^^^  end of conversion specifications


    # locations of parenthesized subexpressions of 'pattern'
    m <- gregexec(pattern, x, perl = TRUE)


    # substrings of parenthesized subexpressions
    y <- regmatches(x, m)


    # remove the dashes, delimiters, and expressions
    fmts <- gsub(pattern, "\\1\\6", x, perl = TRUE)


    value <- lapply(seq_along(fmts), function(i) {
        fmt <- fmts[[i]]
        if (length(y[[i]]) > 0) {


            # select 'any character sequence' subexpressions (corresponding to which delimiters were used)
            exprs <- lapply(y[[i]][4:6, ][m[[i]][4:6, ] != 0], function(yy) {


                # parse to expression, must be length 1 or 2:
                # length 1 is the value to be passed into 'fmt'
                # length 2 is the field width or precision to be used in 'fmt' and the value to be passed into 'fmt'
                exprs <- str2expression(yy)
                if (!(length(exprs) %in% 1:2))
                    stop("parsing result not of length one or two, but ", length(exprs))
                exprs
            })


            # turn a list of a expressions into an expression
            exprs <- unlist(exprs, recursive = FALSE, use.names = FALSE)
            .Call(C_f.str.old, sprintf, fmt, exprs, envir)  # evaluate where requested
        } else sprintf(fmt)
    })


    if (simplify) {
        n <- length(value)
        if (!n)
            character(0)
        else if (n == 1L)
            value[[1L]]
        else if (all(lengths(value) == 1L)) {
            r <- unlist(value, recursive = FALSE)
            if (length(r) == n)
                r
            else value
        }
        else value
    } else value
}


f.str <- function (x, envir = parent.frame(),
             enclos = if (is.list(envir) || is.pairlist(envir))
                          parent.frame() else baseenv(),
    simplify = TRUE)
.Call(C_f.str, x, as.env(envir, enclos), simplify)


consecutiveTRUE <- function (x)
{
    if (storage.mode(x) != "logical")
        stop("argument to 'consecutiveTRUE' is not logical")
    gregexpr("1+", paste(as.integer(x & !is.na(x)), collapse = ""))[[1L]]
}

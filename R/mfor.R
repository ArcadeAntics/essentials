# common.length <- function (...)
# {
#     if (...length() <= 0L)
#         0L
#     else if (!is.numeric(x <- c(list(...), recursive = TRUE, use.names = FALSE)))
#         stop("invalid arguments")
#     else if (anyNA(x))
#         as.vector(NA, typeof(x))
#     else if (min(x) <= 0L)
#         0L
#     else max(x)
# }


is.mfor.done <- function (rho)
.Call(C_is.mfor.done, rho)


omfor <- function (...)
{
    if ((Nargs <- nargs()) < 3L)
        stop(
            sprintf(
                ngettext(
                    Nargs,
                    "%s argument passed to 'mfor' which requires at least 3",
                    "%s arguments passed to 'mfor' which requires at least 3"
                ),
                Nargs
            )
        )


    # all of the (unevaluated) arguments to 'mfor'
    vars <- as.list(substitute(list(...)))[-1L]


    # the (unevaluated) expression to evaluate in the loop
    expr <- vars[[Nargs]]


    # the syntactical names for variables in the loop
    vars <- vars[seq_len(Nargs - 2L)]
    if (!all(vapply(vars, is.symbol, FUN.VALUE = NA, USE.NAMES = FALSE)))
        stop("non-symbol loop variables")


    # this is the collection of sequences we will be iterating over
    seqs <- ...elt(Nargs - 1L)


    n_vars <- length(vars)


    # if we have one variable, the syntax is more like 'for'
    if (n_vars == 1L) {
        skip.eval <- FALSE
        commonLength <- length(seqs)
        updaters <- list(quote(seqs[[i]]))
    }
    else {
        n_seqs <- length(seqs)


        if (!identical(n_seqs, n_vars))
            stop(gettextf(
                if (isTRUE(n_seqs < n_vars))
                    "not enough sequences to unpack (expected %s, got %s)"
                else  "too many sequences to unpack (expected %s, got %s)",
                                                          n_vars, n_seqs
            ))


        lengths_seqs <- lengths(seqs)
        if (length(lengths_seqs) != n_seqs)
            stop(gettextf(
                "'length(seqs)' (%s) and 'length(lengths(seqs))' (%s) are not equal",
                             n_seqs,            length(lengths_seqs)
            ))


        if (!is.numeric(lengths_seqs))
            stop(gettextf(
                "invalid 'lengths(seqs)' of type '%s'",
                                typeof(lengths_seqs)
            ))


        if (skip.eval <- any(lengths_seqs == 0L, na.rm = TRUE))
            commonLength <- 0L
        else {
            commonLength <- max(lengths_seqs, na.rm = TRUE)
            if (any(commonLength %% lengths_seqs != 0L, na.rm = TRUE))
                warning("a sequence will be fractionally recycled")


            updaters <- lapply(seq_len(n_seqs), function(j) {
                len <- lengths_seqs[[j]]
                if (len == 1L)
                    bquote(seqs[[.(j)]][[1L]])
                else if (len == commonLength)
                    bquote(seqs[[.(j)]][[i]])
                else bquote(seqs[[.(j)]][[(i - 1L) %% .(len) + 1L]])
            })
        }
    }
    if (!skip.eval) {
        loop_expr <- bquote(repeat {
            if (.(is.mfor.done)(.(environment())))
                break
            .(expr)
        })


        i <- if (realIndx <- commonLength > .Machine$integer.max)
            0
        else 0L
    }


    # this is where we will be evaluating the for loop
    p <- parent.frame()


    lapply(as.character(vars), assign, value = NULL, envir = p, inherits = FALSE)


    # return(pairlist(
    #     vars = vars,
    #     seqs = seqs,
    #     expr = expr,
    #     loop_expr = loop_expr
    # ))


    if (skip.eval)
        invisible()
    else eval(loop_expr, envir = p)
}


mfor <- function (...)
invisible(.Call(C_mfor, environment(), parent.frame()))

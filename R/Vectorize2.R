Vectorize2 <- function (FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE, FUN.VALUE = NULL)
{
    arg.names <- as.list(formals(FUN))
    arg.names[["..."]] <- NULL
    arg.names <- names(arg.names)
    vectorize.args <- as.character(vectorize.args)
    if (!length(vectorize.args))
        return(FUN)
    if (!all(vectorize.args %in% arg.names))
        stop("must specify names of formal arguments for 'vectorize'")
    rm(arg.names)
    if (is.null(FUN.VALUE)) {
        rm(FUN.VALUE)
        lockEnvironment(environment(), bindings = TRUE)
        function(...) {
            args <- list(...)
            names(args) <- names(match.call(FUN))[-1L]
            names <- if (is.null(names(args)))
                character(length(args))
            else names(args)
            dovec <- names %in% vectorize.args
            .psapply(args[dovec], FUN, args[!dovec], SIMPLIFY, USE.NAMES)
        }
    }
    else {
        rm(SIMPLIFY)
        lockEnvironment(environment(), bindings = TRUE)
        function(...) {
            args <- list(...)
            names(args) <- names(match.call(FUN))[-1L]
            names <- if (is.null(names(args)))
                character(length(args))
            else names(args)
            dovec <- names %in% vectorize.args
            .pvapply(args[dovec], FUN, FUN.VALUE, args[!dovec], USE.NAMES)
        }
    }
}

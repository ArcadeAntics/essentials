R_DoubleColonSymbol <- quote(`::`)
R_TripleColonSymbol <- quote(`:::`)
R_EmptyEnv <- emptyenv()
R_BaseEnv <- baseenv()


wrapper <- function (fun, defaults = NULL, with.pkg = TRUE)
{
    with.pkg <- as.scalar.logical(with.pkg)


    ns.accessor <- NULL
    pkg <- NULL
    fun <- substitute(fun)
    while (is.call(fun)) {
        if (fun[[1L]] == R_DoubleColonSymbol || fun[[1L]] == R_TripleColonSymbol) {
            ns.accessor <- fun[[1L]]
            pkg <- fun[[2L]]
            fun <- fun[[3L]]
        }
        else fun <- fun[[1L]]
    }
    if (is.character(ns.accessor))
        ns.accessor <- as.symbol(ns.accessor)
    if (is.character(pkg))
        pkg <- as.symbol(pkg)
    if (is.character(fun))
        fun <- as.symbol(fun)
    if (is.null(ns.accessor)) {
        envir <- parent.frame()
        while (!identical(envir, R_EmptyEnv)) {
            if (exists(fun, envir = envir, mode = "function", inherits = FALSE)) {
                x <- get(fun, envir = envir, mode = "function", inherits = FALSE)
                if (identical(envir, R_BaseEnv)) {
                    ns.accessor <- R_DoubleColonSymbol
                    pkg <- as.symbol("base")
                }
                else {
                    name <- attr(envir, "name")
                    if (!is.null(name) && grepl("^package:", name) && !is.null(attr(envir, "path"))) {
                        ns.accessor <- R_DoubleColonSymbol
                        pkg <- .rmpkg(name)
                    }
                }
                break
            }
            envir <- parent.env(envir)
        }
        if (identical(envir, R_EmptyEnv))
            stop(gettextf("object '%s' of mode '%s' was not found", domain = "R", as.character(fun), "function"))
    }
    else if (ns.accessor == R_DoubleColonSymbol) {
        x <- getExportedValue(pkg, fun)
        if (!is.function(x))
            stop(gettextf("'%s' from 'namespace:%s' is not a function",
                as.character(fun), as.character(pkg)), domain = NA)
    }
    else if (ns.accessor == R_TripleColonSymbol) {
        x <- get(fun, envir = getNamespace(pkg),
            mode = "function", inherits = FALSE)
    }
    else stop("invalid 'ns.accessor'; should never happen, please report!")
    if (is.primitive(x) && is.null(x <- args(x)))
        stop("unable to retrieve formal arguments of primitive function")
    x <- names(formals(x))
    y <- sapply(x, as.symbol, simplify = FALSE)
    x[x == "..."] <- ""
    if (is.list(defaults)) {
        if (!is.null(names(defaults))) {
            i <- names(defaults) %in% names(y)
            y[names(defaults)[i]] <- defaults[i]
        }
        else {
            i <- seq_len(min(length(y), length(defaults)))
            y[i] <- defaults[i]
        }
    }
    if (with.pkg && !is.null(pkg))
        fun <- call(as.character(ns.accessor), pkg, fun)
    as.call(c(list(fun), y))
}

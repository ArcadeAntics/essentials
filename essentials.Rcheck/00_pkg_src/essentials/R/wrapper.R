wrapper <- function (fun, defaults = NULL, with.pkg = TRUE)
{
    with.pkg <- as.scalar.logical(with.pkg)


    ns.accessor <- NULL
    pkg <- NULL
    fun <- substitute(fun)
    while (is.call(fun)) {
        if (fun[[1L]] == quote(`::`) || fun[[1L]] == quote(`:::`)) {
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
        while (!identical(envir, emptyenv())) {
            if (exists(fun, mode = "function", envir, inherits = FALSE)) {
                x <- get(fun, mode = "function", envir, inherits = FALSE)
                if (identical(envir, baseenv())) {
                    ns.accessor <- quote(`::`)
                    pkg <- as.symbol("base")
                }
                else {
                    name <- attr(envir, "name")
                    if (!is.null(name) && grepl("^package:", name) && !is.null(attr(envir, "path"))) {
                        ns.accessor <- quote(`::`)
                        pkg <- .rmpkg(name)
                    }
                }
                break
            }
            envir <- parent.env(envir)
        }
        if (identical(envir, emptyenv()))
            stop(gettextf("object '%s' of mode 'function' was not found", as.character(fun)))
    }
    else if (ns.accessor == quote(`::`)) {
        x <- getExportedValue(pkg, fun)
        if (!is.function(x))
            stop(gettextf("'%s' from 'namespace:%s' is not a function",
                as.character(fun), as.character(pkg)), domain = NA)
    }
    else if (ns.accessor == quote(`:::`)) {
        x <- get(fun, mode = "function",
            envir = getNamespace(pkg), inherits = FALSE)
    }
    else stop("invalid 'ns.accessor'; should never happen, please report!")
    if (is.primitive(x) && is.null(x <- args(x)))
        stop("unable to retrieve formal arguments of primitive function")
    x <- names(formals(x))
    y <- lapply(x, "as.symbol")
    x[x == "..."] <- ""
    names(y) <- x
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

essentials:::check_this(  # essentials
    check = FALSE, as.cran = TRUE,

    chdir = TRUE, special = TRUE
)





.import <- function (name, env = new.env(parent = emptyenv()), last = TRUE)
{
    env
    ns <- getNamespace(name)
    exports <- getNamespaceExports(ns)
    importIntoEnv(env, exports, ns, exports)
    dimpenv <- getNamespaceInfo(ns, "lazydata")
    dnames <- names(dimpenv)
    eval(str2lang(".Internal(importIntoEnv(env, dnames, dimpenv, dnames))"))
    if (last) {
        attr(env, "name") <- paste0("package:", name)
        lockEnvironment(env, TRUE)
    }
    env
}


import <- function (...)
{
    dots <- as.list(substitute(list(...)))[-1L]
    envir <- parent.frame()
    for (dot in dots) {
        if (is.symbol(dot)) {
            x <- as.character(dot)
            value <- .import(dot)
        }
        else if ( is.call(dot) &&
                  length(dot) == 3L &&
                  identical(dot[[1L]], quote(`%as%`)) &&
                  is.symbol(dot[[2L]]) &&
                  is.symbol(dot[[3L]]) ) {
            x <- as.character(dot[[3L]])
            value <- .import(dot[[2L]])
        }
        else stop(sprintf("invalid argument '%s'", paste(deparse(dot), collapse = "")))
        assign(x, value, envir = envir)
    }
    invisible()
}


importFrom <- function (...)
{
    dots <- as.list(substitute(list(...)))[-1L]
    if (length(dots) < 1)
        stop(sprintf("incorrect number of arguments (%d), expecting %s in the ... list",
            length(dots), "at least 1"))
    name <- dots[[1L]]
    if (!is.symbol(name))
        stop("invalid first argument")
    if (length(dots) <= 1) {
        .import(name, parent.frame(), last = FALSE)
        return(invisible())
    }
    ns <- getNamespace(name)
    envir <- parent.frame()
    for (dot in dots[-1L]) {
        if (is.symbol(dot)) {
            x <- as.character(dot)
            name <- dot
        }
        else if ( is.call(dot) &&
                  length(dot) == 3L &&
                  identical(dot[[1L]], quote(`%as%`)) &&
                  is.symbol(dot[[2L]]) &&
                  is.symbol(dot[[3L]]) ) {
            x <- as.character(dot[[3L]])
            name <- as.character(dot[[2L]])
        }
        else stop(sprintf("invalid argument '%s'", paste(deparse(dot), collapse = "")))
        assign(x, getExportedValue(ns, name), envir = envir)
    }
    invisible()
}


import(this.path %as% t, essentials %as% e)
essentials |> importFrom(`%while%`)
this.path |> importFrom(this.path %as% t)

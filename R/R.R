.system <- function (command, intern = FALSE, ..., dry.run = FALSE, mustWork = NA,
    quiet = Sys.getenv("R_ESSENTIALS_QUIET", intern))
{
    if (dry.run)
        return(command)


    intern <- if (intern) TRUE else FALSE
    mustWork
    mustWork <- tryCatch(if (mustWork) TRUE else FALSE, error = function(c) NA)
    quiet <- if (quiet) TRUE else FALSE


    .quiet <- Sys.getenv("R_ESSENTIALS_QUIET", NA_character_, FALSE)
    if (is.na(.quiet)) {
        on.exit(Sys.unsetenv("R_ESSENTIALS_QUIET"))
        Sys.setenv(R_ESSENTIALS_QUIET = quiet)
    }
    else if (.quiet != quiet) {
        on.exit(Sys.setenv(R_ESSENTIALS_QUIET = .quiet))
        Sys.setenv(R_ESSENTIALS_QUIET = quiet)
    }
    if (!quiet) cat(shPrompt(), command, "\n", sep = "")
    value <- system(command, intern = intern, ...)
    if (intern) value
    else {
        if (!value || isFALSE(mustWork)) {
            if (!quiet)
                cat("\nProcess finished with exit code ",
                    value, "\n", sep = "")
        }
        else if (isTRUE(mustWork)) {
            if (value == -1L)
                stop(gettextf("'%s' could not be run",
                    command), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d",
                command, value), domain = NA)
        }
        else if (value == -1L)
            warning(gettextf("'%s' could not be run", command),
                domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d",
            command, value), domain = NA)
        invisible(value)
    }
}





python <- function (options = NULL, command = NULL, module = NULL, file = NULL,
    args = NULL, chdir = FALSE, ...)
{
    name <- "python"
    if (is.character(file) || is.null(file)) {
        if (length(file) == 0) file <- NULL
        else {
            if (length(file) > 1) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            if (grepl("^(ftp|http|https|file)://", file)) {
                if (chdir) warning("'chdir = TRUE' makes no sense for a URL")
            }
            else if (chdir && (path <- dirname(file)) != ".") {
                file <- basename(file)
                owd <- getwd()
                if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                on.exit(setwd(owd))
                setwd(path)
            }
            file <- shEncode(file, windows.type = name)
        }
    }
    else stop("invalid 'file' argument")


    if (is.character(command) || is.null(command)) {
        if (length(command) == 0) command <- NULL
        else {
            command <- paste(command, collapse = "\n")
            command <- paste("-c", shEncode(command, windows.type = name))
        }
    }
    else stop("invalid 'command' argument")


    if (is.character(module) || is.null(module)) {
        if (length(module) == 0) module <- NULL
        else {
            if (length(module) > 1) {
                warning("first element used of 'module' argument")
                module <- module[[1L]]
            }
            module <- paste("-m", shEncode(module, windows.type = name))
        }
    }
    else stop("invalid 'module' argument")


    if (sum(!is.null(command), !is.null(module), !is.null(file)) > 1)
        stop("cannot use more than one of 'command', 'module', and 'file'")


    args <- asArgs(args)
    args <- shEncode(args, windows.type = name)


    options <- asArgs(options)
    options <- shEncode(options, windows.type = name)
    options <- c(name, options, command, module, file, args)
    command <- paste(options, collapse = " ")
    .system(command = command, ...)
}





.R <- function (options, file, exprs, args, chdir, ..., name, extra)
{
    if (is.character(file) || is.null(file)) {
        if (length(file) == 0) file <- NULL
        else {
            if (length(file) > 1) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            if (grepl("^(ftp|ftps|http|https|file)://", file)) {
                if (chdir) warning("'chdir = TRUE' makes no sense for a URL")
            }
            else if (chdir && (path <- dirname(file)) != ".") {
                # if 'file' is a relative path, we change it to only the
                # basename. hopefully no one has a problem with this decision,
                # though we could use 'normalizePath' if this becomes an issue.
                # it seems to work well for both windows drive, windows unc,
                # unix drive, and unix unc (from what i can tell)
                #
                #
                # for local files, it's only 12 times faster, but for network
                # files, it's 5000 times faster


                # file <- normalizePath(file)
                file <- basename(file)


                owd <- getwd()
                if (is.null(owd))
                    stop("cannot 'chdir' as current directory is unknown")
                on.exit(setwd(owd))
                setwd(path)
            }
            file <- shEncode(file, windows.type = name)
            if (extra) file <- paste0("--file=", file)
        }
    }
    else stop("invalid 'file' argument")


    if (is.character(exprs) || is.null(exprs)) {
        if (length(exprs) == 0) exprs <- NULL
        else exprs <- paste("-e", shEncode(exprs, windows.type = name))
    }
    else stop("invalid 'exprs' argument")


    if (!is.null(file) && !is.null(exprs))
        stop("cannot use 'exprs' with 'file'")


    args <- asArgs(args)
    if (length(args)) {
        args <- shEncode(args, windows.type = name)
        if (extra)
            args <- c("--args", args)
    }


    options <- asArgs(options)
    options <- shEncode(options, windows.type = name)
    options <- c(name, options, file, exprs, args)
    command <- paste(options, collapse = " ")
    .system(command = command, ...)
}


R <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ...)
{
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = "R", extra = TRUE)
}


Rcmd <- function (options = NULL, command = "", args = NULL, ...)
{
    options <- asArgs(options)
    options <- shEncode(options, windows.type = "R")
    command <- asArgs(command)[[1L]]
    command <- shEncode(command, windows.type = "Rterm")
    args <- asArgs(args)
    args <- shEncode(args, windows.type = "R CMD")
    command <- paste(c("R", options, "CMD", command, args), collapse = " ")
    .system(command = command, ...)
}


Rgui <- function (options = NULL, args = NULL, ...)
{
    .R(options = options, file = NULL, exprs = NULL, args = args,
        ..., name = "Rgui", extra = TRUE)
}


Rscript <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ...)
{
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = "Rscript", extra = FALSE)
}


Rterm <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ...)
{
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = if (.Platform$OS.type == "windows")
            "Rterm"
        else "R", extra = TRUE)
}

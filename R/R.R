# get.shell.and.flag <- function (shell, flag = "/c", missing.shell = missing(shell), missing.flag = missing(flag))
# {
#     if (missing.shell) {
#         if (os.windows) {
#             shell <- Sys.getenv("R_SHELL")
#             if (!nzchar(shell))
#                 shell <- Sys.getenv("COMSPEC")
#         }
#         else shell <- NULL
#     }
#     if (!is.null(shell)) {
#         if (missing.flag && any(!is.na(pmatch(c("bash", "tcsh", "sh"), basename(shell)))))
#             flag <- "-c"
#         paste(shell, flag)
#     }
#     else NULL
# }


.system <- function (command, intern = FALSE, ..., dry.run = FALSE, mustWork = NA,
    quiet = Sys.getenv("R_ESSENTIALS_QUIET", intern))
{
    ocommand <- command
    # if (os.windows) {
    #     command <- gsub("([()%!^\"<>&|])", "^\\1", command)
    #     command <- paste(Sys.getenv("COMSPEC"), "/c", command)
    # }


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
    if (!quiet) cat(shPrompt(), ocommand, "\n", sep = "")
    value <- system(command = command, intern = intern, ...)
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
                    ocommand), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d",
                ocommand, value), domain = NA)
        }
        else if (value == -1L)
            warning(gettextf("'%s' could not be run", ocommand),
                domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d",
            ocommand, value), domain = NA)
        invisible(value)
    }
}





python <- function (options = NULL, command = NULL, module = NULL, file = NULL,
    args = NULL, chdir = FALSE, ..., name = windows.type, dir)
{
    windows.type <- if (os.windows) "python.exe" else "python3"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    if (is.character(file) || is.null(file)) {
        if (length(file) == 0) file <- NULL
        else {
            if (length(file) > 1) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            file <- path.expand(file)
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
            file <- shEncode(file, windows.type = windows.type)
        }
    }
    else stop("invalid 'file' argument")


    if (is.character(command) || is.null(command)) {
        if (length(command) == 0) command <- NULL
        else {
            command <- paste(command, collapse = "\n")
            command <- paste("-c", shEncode(command, windows.type = windows.type))
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
            module <- paste("-m", shEncode(module, windows.type = windows.type))
        }
    }
    else stop("invalid 'module' argument")


    if (sum(!is.null(command), !is.null(module), !is.null(file)) > 1)
        stop("cannot use more than one of 'command', 'module', and 'file'")


    args <- asArgs(args)
    args <- shEncode(args, windows.type = windows.type)


    options <- asArgs(options)
    options <- shEncode(options, windows.type = windows.type)
    options <- c(name, options, command, module, file, args)
    command <- paste(options, collapse = " ")
    .system(command = command, ...)
}





.R <- function (options, file, exprs, args, chdir, ..., name, windows.type, extra,
    width.cutoff = 60L, deparseCtrl = c("keepInteger", "showAttributes", "useSource", "keepNA", "digits17"))
{
    if (is.character(file) || is.null(file)) {
        if (length(file) <= 0L)
            file <- NULL
        else {
            if (length(file) > 1L) {
                warning("first element used of 'file' argument")
                file <- file[[1L]]
            }
            file <- path.expand(file)
            if (grepl("^(ftp|ftps|http|https|file)://", file)) {
                if (chdir)
                    warning("'chdir = TRUE' makes no sense for a URL")
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
            file <- shEncode(file, windows.type = windows.type)
            if (extra) file <- paste0("--file=", file)
        }
    }
    else stop("invalid 'file' argument")


    if (is.null(exprs) || is.character(exprs)) {
        if (length(exprs) <= 0L)
            exprs <- NULL
        else exprs <- paste("-e", shEncode(exprs, windows.type = windows.type))
    }
    else if (is.language(exprs)) {
        exprs <- this.path:::code2character(exprs, width.cutoff, deparseCtrl)
        if (length(exprs) <= 0L)
            exprs <- NULL
        else exprs <- paste("-e", shEncode(exprs, windows.type = windows.type))
    }
    else stop("invalid 'exprs' argument")


    if (!is.null(file) && !is.null(exprs))
        stop("cannot use 'exprs' with 'file'")


    args <- asArgs(args)
    if (length(args)) {
        args <- shEncode(args, windows.type = windows.type)
        if (extra)
            args <- c("--args", args)
    }


    options <- asArgs(options)
    options <- shEncode(options, windows.type = windows.type)
    options <- c(name, options, file, exprs, args)
    command <- paste(options, collapse = " ")
    .system(command = command, ...)
}


R <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir,
    evaluated, simplify.brace = TRUE)
{
    windows.type <- if (os.windows) "Rterm.exe" else "R"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    exprs <- this.path:::maybeQuote(exprs, evaluated, simplify.brace)
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = name, windows.type = windows.type,
        extra = TRUE)
}


Rcmd <- function (options = NULL, command = "", args = NULL, ..., name = windows.type, dir)
{
    windows.type <- if (os.windows) "R.exe" else "R"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    options <- asArgs(options)
    options <- shEncode(options, windows.type = "R")
    command <- asArgs(command)[[1L]]
    if (!grepl("^[0123456789abcdefghijklmnopqrstuvwxyz]+$", command, ignore.case = TRUE))
        command <- shEncode(command, windows.type = "Rterm")
    args <- asArgs(args)
    args <- shEncode(args, windows.type = "R CMD")
    command <- paste(c(name, options, "CMD", command, args), collapse = " ")
    .system(command = command, ...)
}


Rgui <- function (options = NULL, args = NULL, ..., name = "Rgui.exe", dir)
{
    windows.type <- "Rgui.exe"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    .R(options = options, file = NULL, exprs = NULL, args = args,
        ..., name = name, windows.type = windows.type, extra = TRUE)
}


Rscript <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir,
    evaluated, simplify.brace = TRUE)
{
    windows.type <- if (os.windows) "Rscript.exe" else "Rscript"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    exprs <- this.path:::maybeQuote(exprs, evaluated, simplify.brace)
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = name, windows.type = windows.type,
        extra = FALSE)
}


Rterm <- function (options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir,
    evaluated, simplify.brace = TRUE)
{
    windows.type <- if (os.windows) "Rterm.exe" else "R"
    if (!missing(name)) {
        if (!missing(dir))
            stop("cannot use 'name' and 'dir'")
        if (!is.character(name) || length(name) != 1)
            stop("invalid 'name' argument")
        name <- shEncode(name)
    }
    else if (!missing(dir))
        name <- shEncode(paste0(dir, "/", windows.type))
    exprs <- this.path:::maybeQuote(exprs, evaluated, simplify.brace)
    .R(options = options, file = file, exprs = exprs, args = args,
        chdir = chdir, ..., name = name, windows.type = windows.type,
        extra = TRUE)
}

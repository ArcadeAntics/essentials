

# delayedAssign("isabs", {
#     if (os.windows)
#         function(path) grepl("^(.:|~)([/\\\\]|$)|^[/\\\\]([^/\\\\]|$)|^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", path)
#     else function(path) grepl("^~?/", path)
# })


MATCH <- function (x, table, nomatch = NA_integer_, incomparables = NULL, fromLast = FALSE)
{
    if (fromLast) {
        value <- match(x, rev(table), NA_integer_, incomparables)
        if (any(indx <- is.na(value))) {
            value <- length(table) - value + 1L
            value[indx] <- as.scalar.integer(nomatch)
            value
        }
        else length(table) - value + 1L
    }
    else match(x, table, nomatch, incomparables)
}


MATCH.1 <- function (x, table, nomatch = NA_integer_, incomparables = NULL, fromLast = FALSE)
{
    if (fromLast) {
        value <- match(x, rev(table), NA_integer_, incomparables)
        if (is.na(value))
            as.scalar.integer(nomatch)
        else length(table) - value + 1L
    }
    else match(x, table, nomatch, incomparables)
}


windows.path.join <- function (path)
{
    # path <- c("C:testing", "C:/testing", "~", "~/testing", "//host", "//host/share", "//host/share/path/to/file", "not-an-abs-path")
    # path <- c("c:/test1", "c:test2", "C:test3")
    # path <- c("C:", "test1")
    # path <- c("test1", "c:/test2", "test3", "//host/share/test4", "test5", "c:/test6", "test7", "c:test8", "test9")


    opath <- path  # original 'path' argument


    # writeLines("\nin windows.path.join\n")
    #
    #
    # cat("\n> opath\n") ; print(opath) ; cat("\n")


    len <- length(opath)


    # any of the paths are ~ or start with ~/ or ~\
    if (any(i <- startsWith(opath, "~/") |
                 startsWith(opath, "~\\") |
                 opath == "~")) {
        i <- max(which(i))
        if (i == len)
            return(opath[[len]])
        opath <- opath[i:len]
        len <- length(opath)
    }


    drives.and.paths <- splitdrive(opath)
    drives <- drives.and.paths[1L, ]
    path <- drives.and.paths[2L, ]


    # cat("\n> drives\n") ; print(drives) ; cat("\n")
    #
    #
    # cat("\n> path\n") ; print(path) ; cat("\n")


    # any of the paths contain a drive
    if (any(has.drives <- drives != "")) {
        drive_indx <- which(has.drives)


        # exactly one of the paths contain a drive
        if (length(drive_indx) == 1L) {


            # if the path containing the drive is the last one,
            # return without pasting to avoid unnecessary translation
            if (drive_indx == len)
                return(opath[[len]])


            # select the drive, then select the paths
            drive <- drives[[drive_indx]]
            if (drive_indx != 1L) {
                opath <- opath[drive_indx:len]
                path  <- path [drive_indx:len]
                len <- length(opath)
            }
        }


        # multiple paths contains a drive
        else {


            drive_indx <- max(drive_indx)
            drive <- drives[[drive_indx]]


            # the drive is absolute (remember that "d:" is not absolute)
            if (startsWith(drive, "/") ||
                startsWith(drive, "\\")) {


                # if the path containing the drive is the last one,
                # return without pasting to avoid unnecessary translation
                if (drive_indx == len)
                    return(opath[[len]])


                opath <- opath[drive_indx:len]
                path  <- path [drive_indx:len]
                len <- length(opath)
            }


            # the drive is not absolute
            else {


                tmp <- path[drive_indx:len]
                if (any(i <- startsWith(tmp, "/") |
                             startsWith(tmp, "\\"))) {


                    if (drive_indx == len)
                        return(opath[[len]])


                    path <- tmp
                }
                else {


                    if (!all(keep_indx <- tolower(drives) %in% c(tolower(drive), ""))) {
                        start_indx <- max(which(!keep_indx))
                        start_indx <- start_indx + match(TRUE, drives[(start_indx + 1L):len] != "")
                        if (drive_indx == len &&
                            (
                                start_indx == len ||
                                all(path[start_indx:(len - 1L)] == "")
                            ))
                            return(opath[[len]])
                        path <- path[start_indx:len]
                        len <- length(path)
                    }
                    else {
                        start_indx <- match(TRUE, drives != "")
                        if (start_indx != 1L) {
                            path <- path[start_indx:len]
                            len <- length(path)
                        }
                    }
                }
            }
        }


        path <- path[path != ""]
        len <- length(path)
    }
    else {
        if (any(i <- startsWith(path, "/") |
                     startsWith(path, "\\"))) {
            i <- max(which(i))
            if (i == len)
                return(opath[[len]])
            if (i != 1L) {
                path <- path[i:len]
                len <- length(path)
            }
        }
        needs.fsep <- !(endsWith(path, "/") | endsWith(path, "\\"))
        needs.fsep[[len]] <- FALSE
        if (any(needs.fsep))
            path[needs.fsep] <- paste0(path[needs.fsep], "/")
        return(paste0(path, collapse = ""))
    }
    if (!all(has.path <- path != "")) {
        path <- path[has.path]
        len <- length(path)
    }
    if (any(i <- startsWith(path, "/") |
                 startsWith(path, "\\"))) {
        i <- max(which(i))
        if (i != 1L) {
            path <- path[i:len]
            len <- length(path)
        }
    }
    needs.fsep <- !(endsWith(path, "/") | endsWith(path, "\\"))
    needs.fsep[[len]] <- FALSE
    if (any(needs.fsep))
        path[needs.fsep] <- paste0(path[needs.fsep], "/")
    paste0(c(drive, path), collapse = "")
}


unix.path.join <- function (path)
{
    len <- length(path)
    if (any(i <- startsWith(path, "/") |
                 startsWith(path, "~/") |
                 path == "~")) {
        i <- max(which(i))
        if (i == len)
            return(path[[len]])
        if (i != 1L) {
            path <- path[i:len]
            len <- length(path)
        }
    }
    needs.fsep <- !endsWith(path, "/")
    needs.fsep[[len]] <- FALSE
    if (any(needs.fsep))
        path[needs.fsep] <- paste0(path[needs.fsep], "/")
    paste0(path, collapse = "")
}


file.join <- function (...)
{
    if (...length() <= 0L)
        return(character())


    dots <- lapply(list(...), function(xx) {
        xx <- as.character(xx)
        if (!is.character(xx))
            stop("non-string argument to 'path.join'")
        xx
    })
    if (any(lengths(dots, use.names = FALSE) <= 0L))
        return(character())


    dots <- lapply(dots, function(xx) {
        if (any(Encoding(xx) == "bytes"))
            stop("strings with \"bytes\" encoding are not allowed")
        if (any(indx <- is.na(xx)))
            xx[indx] <- "NA"
        xx
    })


    # os.windows <- essentials:::os.windows ; warning("delete this later", immediate. = TRUE)
    fun <- if (os.windows)
        windows.path.join
    else unix.path.join


    pvapply(dots, function(...) {
        xx <- c(...)
        if (length(xx) <= 0L)
            return("")
        xx <- xx[nzchar(xx)]
        if (length(xx) <= 0L)
            return("")
        if (length(xx) == 1L)
            return(xx)
        fun(xx)
    }, "", USE.NAMES = FALSE)
}


path.join <- file.join


splitdrive <- function (path)
{
    value <- matrix("", 2L, length(path))
    indexes <- seq_along(path)
    if (length(path) && any(i <- substr(path, 2L, 2L) == ":")) {
        value[1L, indexes[i]] <- substr(path[i], 1L, 2L)
        value[2L, indexes[i]] <- substr(path[i], 3L, 1000000L)
        path <- path[!i]
        indexes <- indexes[!i]
    }
    if (length(path) && any(i <- startsWith(path, "~/") |
                                 startsWith(path, "~\\") |
                                 path == "~")) {
        value[1L, indexes[i]] <- "~"
        value[2L, indexes[i]] <- substr(path[i], 2L, 1000000L)
        path <- path[!i]
        indexes <- indexes[!i]
    }
    if (length(path) && any(i <- startsWith(path, "//") |
                                 startsWith(path, "\\\\") |
                                 startsWith(path, "/\\") |
                                 startsWith(path, "\\/"))) {
        m <- regexpr("^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", path[i])
        m <- attr(m, "match.length")
        value[1L, indexes[i]] <- substr(path[i], 1L, m)
        value[2L, indexes[i]] <- substr(path[i], m + 1L, 1000000L)
        path <- path[!i]
        indexes <- indexes[!i]
    }
    value[2L, indexes] <- path
    return(value)
}


splitdrive.1 <- function (path)
{
    if (substr(path, 2L, 2L) == ":")
        c(substr(path, 1L, 2L), substr(path, 3L, 1000000L))
    else if (startsWith(path, "~/") ||
             startsWith(path, "~\\") ||
             path == "~")
        c("~", substr(path, 2L, 1000000L))
    else if (startsWith(path, "//") ||
             startsWith(path, "\\\\") ||
             startsWith(path, "/\\") ||
             startsWith(path, "\\/")) {
        # path <- "//host/share/"
        m <- regexpr("^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", path)
        if (m == -1L)
            c("", path)
        else {
            m <- attr(m, "match.length")
            c(substr(path, 1L, m), substr(path, m + 1L, 1000000L))
        }
    }
    else c("", path)
}


# path <- c("C:testing", "C:/testing", "~", "~/testing", "//host", "//host/share", "//host/share/path/to/file", "not-an-abs-path")
#
#
# lapply(path, splitdrive.1)
# splitdrive(path)

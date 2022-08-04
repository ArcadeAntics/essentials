

delayedAssign("isabs", {
    if (os.windows)
        function(path) grepl("^([A-Za-z]:|~)([/\\\\]|$)|^[/\\\\]([^/\\\\]|$)|^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", path)
    else function(path) grepl("^~?/", path)
})


file.join <- function (..., fsep = .Platform$file.sep)
{
    if (!is.character(fsep) || length(fsep) <= 0 || is.na(fsep))
        stop(gettext("invalid separator", domain = "R"))
    fsep <- as.scalar.character(fsep)
    dots <- lapply(list(...), function(xx) {
        xx <- as.character(xx)
        if (!is.character(xx))
            stop("non-string argument to 'file.join'")
        xx
    })
    pvapply(dots, function(...) {
        xx <- c(...)
        if (any(I <- isabs(xx))) {
            I <- which(I)
            i <- I[[length(I)]]
            if (os.windows && grepl("^[/\\\\]([^/\\\\]|$)", xx[[i]])) {
                y <- xx[I[-length(I)]]
                y <- y[grepl("^([A-Za-z]:|~)([/\\\\]|$)|^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", y)]
                len <- length(y)
                if (len <= 0L)
                    xx <- xx[i:length(xx)]
                else {
                    y <- y[[len]]
                    y <- gsub("^([A-Za-z]:|~)([/\\\\]|$).*$", "\\1", y)
                    y <- gsub("^([/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+).*$", "\\1", y)
                    xx <- c(y, xx[i:length(xx)])
                }
            }
            else xx <- xx[i:length(xx)]
        }
        if (anyNA(xx))
            return(NA_character_)
        if (!all(i <- nzchar(xx)))
            xx <- xx[i]
        len <- length(xx)
        if (len <= 0L)
            return("")
        else if (len == 1L)
            return(xx)
        if (os.windows)
            i <- c(!(grepl("[/\\\\]$", xx[-len]) | grepl("^[/\\\\]", xx[-1L])), FALSE)
        else {
            i <- !endsWith(xx, "/")
            i[[len]] <- FALSE
        }
        if (any(i))
            xx[i] <- paste0(xx[i], fsep)
        paste(xx, collapse = "")
    }, "", USE.NAMES = FALSE)
}


# file.join("C:", "test1")
# file.join("C:/", "test1")
# file.join("C:/path/to/file1", "/path/to/file2")
# file.join("//host-name/share-name/path/to/file1", "/path/to/file2")

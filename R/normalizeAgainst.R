normalizeAgainst <- function (..., n = 0L, against = this.dir(verbose = FALSE, n = n + 1L))
{
    if (is.null(against))
        normalizePath(...)
    else if (is.character(against)) {
        if (length(against) == 0) normalizePath(...)
        else {
            owd <- getwd()
            if (is.null(owd))
                stop("cannot 'normalizeAgainst' as current directory is unknown")
            on.exit(setwd(owd))
            setwd(against[[1L]])
            normalizePath(...)
        }
    }
    else stop(gettextf("invalid '%s' argument", "against", domain = "R"))
}


.normalizePath <- function (..., wd = NULL)
normalizeAgainst(..., against = wd)

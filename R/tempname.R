tempname <- function (nm = NULL, prefix = "name", suffix = "")
{
    # tempname is used to make a name for a temporary column
    # since data.frames have unique names (or at least should), if
    #     a temporary column is needed, you'll need a unique name

    if (!is.character(nm) && !is.null(nm))
        stop(gettextf("invalid '%s' argument", "nm", domain = "R"))
    if (!is.character(prefix))
        stop(gettextf("invalid '%s' argument", "prefix", domain = "R"))
    if (!is.character(suffix))
        stop(gettextf("invalid '%s' argument", "suffix", domain = "R"))
    if (!length(prefix))
        stop("no 'prefix'")
    if (!length(suffix))
        stop("no 'suffix'")
    len <- max(length(prefix), length(suffix))
    done <- FALSE
    for (n in seq_len(100L)) {
        value <- sprintf("%s%x%s", prefix, sample.int(2147483647L, len), suffix)
        if (!any(value %in% nm)) {
            done <- TRUE
            break
        }
    }
    if (!done)
        stop("cannot find unused tempname")
    return(value)
}

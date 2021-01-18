

as.boolean <- function (x, `_R_CHECK_LENGTH_1_CONDITION_` = FALSE)
{
    o_R_CHECK_LENGTH_1_CONDITION_ <- Sys.getenv("_R_CHECK_LENGTH_1_CONDITION_")
    on.exit(Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = o_R_CHECK_LENGTH_1_CONDITION_))
    Sys.setenv(`_R_CHECK_LENGTH_1_CONDITION_` = `_R_CHECK_LENGTH_1_CONDITION_`)
    tryCatch(if (x)
        TRUE
    else FALSE, error = function(c) NA)

}


aslength1 <- function (x)
{
    if (!is.vector(x))
        x <- as.vector(x)
    len <- length(x)
    if (len == 1L) {
        x
    }
    else if (len > 1L) {
        warning(gettextf("first element used of '%s' argument",
            deparse(substitute(x), nlines = 1L)[1L], domain = NA))
        x[1L]
    }
    else stop(gettextf("'%s' must be of length 1", domain = NA,
        deparse(substitute(x), nlines = 1L)[1L]))
}


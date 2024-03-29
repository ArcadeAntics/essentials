shEncode <- function (string, type = NULL, unix.type = NULL, windows.type = "Rscript")
{
    if (!is.character(string))
        string <- as.character(string)
    if (!length(string)) {
        value <- character()
        attributes(value) <- attributes(string)
        return(unclass(value))
    }
    attrib <- attributes(string)
    attributes(string) <- NULL


    if (is.null(type))
        type <- if (.Platform$OS.type == "windows")
            windows.type
        else unix.type


    if (is.null(type))
        type <- "sh"
    else {
        type <- sub("\\.exe$", "", basename(type))
        type <- match.arg(type, c("sh", "cmd", "perl", "python3", "R", "R CMD", "Rcmd", "Rgui", "Rscript", "Rterm"))
    }
    switch(type, sh = {


        # replace every single quote with (single quote, double quote, single
        # quote, double quote, single quote)
        string <- gsub("'", "'\"'\"'", string, fixed = TRUE)


        # surround with single quotes
        string <- paste0("'", string, "'", recycle0 = TRUE)


    }, cmd = {


        # escape each of the special characters ( ) % ! ^ " < > & |
        string <- gsub("([()%!^\"<>&|])", "^\\1", string)


    }, R = , `R CMD` = , Rcmd = {


        # replace a double quote preceded by any number of backslashes
        #
        # with
        #
        # three backslashes, a double quote, preceded by four times as many
        #     preceding backslashes (are you for real, Windows??)
        string <- gsub("(\\\\*)\"", "\\1\\1\\1\\1\\\\\\\\\\\\\"", string)


        # replace a series of backslashes at the end of a string
        #
        # with
        #
        # four times as many backslashes (seriously, this is ridiculous)
        string <- sub("(\\\\+)$", "\\1\\1\\1\\1", string)


        # surround with double quotes
        string <- paste0("\"", string, "\"", recycle0 = TRUE)


    }, perl = , python = , python3 = , Rgui = , Rscript = , Rterm = {


        # replace a double quote preceded by any number of backslashes
        #
        # with
        #
        # one backslash, a double quote, preceded by
        #     two times as many preceding backslashes
        string <- gsub("(\\\\*)\"", "\\1\\1\\\\\"", string)


        # replace a series of backslashes at the end of a string
        #
        # with
        #
        # two times as many backslashes
        string <- sub("(\\\\+)$", "\\1\\1", string)


        # surround with double quotes
        string <- paste0("\"", string, "\"", recycle0 = TRUE)


    }, stop("invalid 'type'; should never happen, please report!"))
    attributes(string) <- attrib
    unclass(string)
}


commandEncode <- commandQuote <- shEncode

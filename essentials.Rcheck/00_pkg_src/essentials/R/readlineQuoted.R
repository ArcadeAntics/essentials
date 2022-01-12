readlineQuoted <- function (prompt = "", times = 1)
{
    prompt <- as.scalar.string(prompt)
    times <- as.scalar.real(times)
    times <- min(.Machine$double.base^.Machine$double.digits,
        if (times >= 1) floor(times))
    count <- 0
    do ({
        value <- readline(prompt)
        value <- tryCatch(str2expression(value), error = function(c) NULL)
        if (is.null(value) || length(value) > 1L)
            warning(gettext("invalid input, must be a properly quoted string"),
                call. = FALSE, immediate. = TRUE)
        else if (!length(value))
            return(NA_character_)
        else if (!is.character(value[[1L]]))
            warning(gettext("invalid input, must be a properly quoted string"),
                call. = FALSE, immediate. = TRUE)
        else return(value[[1L]])
    }) %while% ((count <- count + 1) < times)
    stop(sprintf(gettext(if (times == 1)
        "failed to receive valid input after %.0f attempt"
    else "failed to receive valid input after %.0f attempts"),
        times))
}

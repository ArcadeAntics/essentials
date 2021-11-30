readlineQuoted <- function (prompt = "", times = 1)
{
    prompt <- as.scalar.string(prompt)
    times <- as.scalar.real(times)
    times <- min(.Machine$double.base^.Machine$double.digits,
        if (times >= 1) floor(times))
    count <- 0
    do ({
        value <- tryCatch(str2lang(readline(prompt)), error = function(c) NA_character_)
        if (!is.character(value))
            warning(gettext("invalid input, must be a properly quoted string"),
                call. = FALSE, immediate. = TRUE)
        else return(value)
        count <- count + 1
    }) %while% (count < times)
    stop(sprintf(gettext(if (times == 1)
        "failed to receive valid input after %.0f attempt"
    else "failed to receive valid input after %.0f attempts"),
        times))
}

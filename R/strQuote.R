strQuote <- function (x)
{
    if (!is.character(x))
        x <- as.character(x)
    squote <- grepl("\"", x, fixed = TRUE) & !grepl("'", x, fixed = TRUE)
    value <- character(length(x))
    value[squote] <- encodeString(x[squote], quote = "'")
    value[!squote] <- encodeString(x[!squote], quote = "\"")
    value
}

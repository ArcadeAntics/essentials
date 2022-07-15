regexencode <- function (x, edge = FALSE, beginning = edge, end = edge, ignore.case = FALSE)
{
    # escape metacharacters . \ | ( ) [ { ^ $ * + ?
    x <- gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", x)
    if (ignore.case)
        x <- gsub("([ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz])", "[\\U\\1\\L\\1]", x, perl = TRUE)
    paste0(
        ifelse(beginning, "^", ""),
        x,
        ifelse(end, "$", ""),
        recycle0 = TRUE
    )
}

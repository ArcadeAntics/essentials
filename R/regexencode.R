regexencode <- function (x, edge = FALSE, start = edge, end = edge, ignore.case = FALSE)
{
    # escape metacharacters . \ | ( ) [ { ^ $ * + ?
    x <- gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", x)
    if (ignore.case)
        x <- paste0("(?i)", x, "(?-i)")
    paste0(
        ifelse(start, "^", ""),
        x,
        ifelse(end, "$", ""),
        recycle0 = TRUE
    )
}

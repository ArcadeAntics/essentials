regexencode <- function (x, edge = FALSE, beginning = edge, end = edge)
{
    paste0(
        ifelse(beginning, "^", ""),
        gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", x),
        ifelse(end, "$", ""),
        recycle0 = TRUE
    )
}

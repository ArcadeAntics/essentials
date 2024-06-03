dir.choose <- function (default = "", caption = "Select folder", graphics = FALSE)
{
    graphics <- if (graphics) TRUE else FALSE
    value <- path.expand(as.scalar.character(default))
    if (!file.exists(value)) {
        value <- path.expand("~")
        if (!file.exists(value))
            value <- "/"
    }
    fsep <- if (.Platform$OS.type == "windows") "\\" else "/"
    value <- normalizePath(value, winslash = fsep, mustWork = TRUE)
    fun <- if (graphics) basename2 else identity
    repeat {
        dirs <- list.dirs(value, full.names = FALSE, recursive = FALSE)
        dirs <- c("..", dirs)
        x <- utils::menu(c(".", dirs), graphics = graphics,
            title = paste0(caption, ": ", fun(value)))
        if (x == 0L)
            return(NA_character_)
        else if (x == 1L)
            return(value)
        else value <- normalizePath(path.join(value, dirs[x - 1L]), winslash = fsep, mustWork = TRUE)
    }
}

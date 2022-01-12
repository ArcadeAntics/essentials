dir.choose <- function (default = "", caption = "Select folder", graphics = FALSE)
{
    graphics <- isTRUE(graphics)
    value <- path.expand(as.scalar.character(default))
    if (!file.exists(value)) {
        value <- path.expand("~")
        if (!file.exists(value))
            value <- "/"
    }
    slash <- if (.Platform$OS.type == "windows")
        "\\"
    else "/"
    value <- normalizePath(value, winslash = slash, mustWork = TRUE)
    fun <- if (graphics) basename else force
    repeat {
        dirs <- list.dirs(value, full.names = FALSE, recursive = FALSE)
        x <- utils::menu(c(".", "..", dirs), graphics = graphics,
            title = paste0("Folder: ", fun(value)))
        if (x == 0L)
            return(NA_character_)
        else if (x == 1L)
            return(value)
        else if (x == 2L)
            value <- normalizePath(file.path(value, ".."), winslash = slash, mustWork = TRUE)
        else value <- file.path(value, dirs[x - 2L], fsep = slash)
    }
}

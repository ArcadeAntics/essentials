dir.choose <- function (default = "", caption = "Select folder", graphics = FALSE)
{
    graphics <- if (graphics) TRUE else FALSE
    value <- path.expand(as.scalar.character(default))
    if (!file.exists(value)) {
        value <- path.expand("~")
        if (!file.exists(value))
            value <- "/"
    }
    winslash <- if (.Platform$OS.type == "windows") "\\" else "/"
    mustWork <- TRUE
    value <- normalizePath(value, winslash, mustWork)
    fun <- if (graphics) basename2 else identity
    alt_choices <- c("Open", "Type Folder", "New Folder", "New Text Document", "Delete", "Rename", "..")
    n <- length(alt_choices)
    invalid_name <- function(x, message = TRUE) {
        if (.Platform$OS.type == "windows") {
            if (grepl("[\\\\/:*?\"<>|]", x)) {
                if (message)
                    cat("A file name can't contain any of the following characters:\n  \\ / : * ? \" < > |\n")
                TRUE
            }
            else FALSE
        }
        else {
            if (grepl("/", x)) {
                if (message)
                    cat("A file name can't contain the character /\n")
                TRUE
            }
            else FALSE
        }
    }
    repeat {
        files <- dir2(value, all.files = TRUE, no.. = TRUE)
        dirs <- files[dir.exists(path.join(value, files))]
        i <- utils::menu(
            c(alt_choices, dirs),
            graphics = graphics,
            title = paste0(caption, ": ", fun(value))
        )
        if (i == 0L)
            return(NA_character_)
        else if (i > n)
            value <- normalizePath(path.join(value, dirs[i - n]), winslash, mustWork)
        else switch(alt_choices[[i]],
        Open = return(value),
        `Type Folder` = {
            x <- readline("Enter folder name: ")
            if (nzchar(x)) {
                if (!this.path:::.is_abs_path(x))
                    x <- "/"
                if (!dir.exists(x))
                    x <- "/"
                value <- normalizePath(x, winslash, mustWork)
            }
        },
        `New Folder` = {
            repeat {
                x <- readline("Enter new folder name: ")
                if (nzchar(x)) {
                    if (invalid_name(x)) next
                    dir.create(path.join(value, x))
                }
                break
            }
        },
        `New Text Document` = {
            repeat {
                x <- readline("Enter new text document name: ")
                if (nzchar(x)) {
                    if (invalid_name(x)) next
                    file.create(path.join(value, x))
                }
                break
            }
        },
        Delete = {
            i <- utils::menu(
                files,
                graphics = graphics,
                title = paste0("Select file to delete: ", fun(value))
            )
            unlink(path.join(value, files[i]), recursive = TRUE, force = TRUE)
        },
        Rename = {
            i <- utils::menu(
                files,
                graphics = graphics,
                title = paste0("Select file to rename: ", fun(value))
            )
            repeat {
                x <- readline("Enter new name: ")
                if (nzchar(x)) {
                    if (invalid_name(x)) next
                    file.rename(
                        path.join(value, files[i]),
                        path.join(value, x)
                    )
                }
                break
            }
        },
        .. = {
            value <- normalizePath(path.join(value, ".."), winslash, mustWork)
        },
        stop("invalid choice")
        )
    }
}

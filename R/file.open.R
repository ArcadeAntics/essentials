file.open <- function (file)
{
    if (!is.character(file))
        stop("a character vector argument expected", domain = "R")
    if (!length(file))
        return(invisible(file))
    path <- file
    i <- !grepl("^(ftp|https?|file)://", path)
    path[i] <- normalizePath(path[i])
    if (.Platform$OS.type != "windows") {
        path <- shEncode(path)
        if (this.path::OS.type$darwin)
            command <- paste("open", path)
        else command <- paste("xdg-open", path)
        for (command in command) system(command)
    }
    else for (path in path) tryCatch(shell.exec(path), error = warning)
    return(invisible(file))
}

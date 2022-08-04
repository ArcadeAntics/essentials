basename2 <- function (path)
{
    if (typeof(path) != "character")
        stop(gettext("a character vector argument expected", domain = "R"))
    if (.Platform$OS.type == "windows") {
        pattern <- "^([/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+)[/\\\\]*$"
        if (any(i <- grepl(pattern, path))) {
            value <- character(length(path))
            value[i] <- "."
            value[!i] <- basename(path[!i])
            value
        }
        else basename(path)
    }
    else {
        pattern <- "^(//[^/]+/+[^/]+)/*$"
        if (any(i <- grepl(pattern, path))) {
            value <- character(length(path))
            value[i] <- "."
            value[!i] <- basename(path[!i])
            value
        }
        else basename(path)
    }
}


dirname2 <- function (path)
{
    if (typeof(path) != "character")
        stop(gettext("a character vector argument expected", domain = "R"))
    if (.Platform$OS.type == "windows") {
        pattern <- "^([/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+)[/\\\\]*$"
        if (any(i <- grepl(pattern, path))) {
            value <- character(length(path))
            value[i] <- sub(pattern, "\\1", path[i])
            value[!i] <- dirname(path[!i])
            value
        }
        else dirname(path)
    }
    else {
        pattern <- "^(//[^/]+/+[^/]+)/*$"
        if (any(i <- grepl(pattern, path))) {
            value <- character(length(path))
            value[i] <- sub(pattern, "\\1", path[i])
            value[!i] <- dirname(path[!i])
            value
        }
        else dirname(path)
    }
}


path <- c("//host-name/share-name/path/to/file", "//host-name/share-name/path/to",
    "//host-name/share-name/path", "//host-name/share-name/",
    "//host-name/share-name")
cbind(path, basename(path), basename2(path))
cbind(path, dirname(path), dirname2(path))
cbind(path, file.path(dirname2(path), basename2(path)))

# ext <- extension <- function (path, compression = FALSE, invert = FALSE)
# {
#     # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = TRUE )
#     # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = FALSE)
#     # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = TRUE )
#     # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = FALSE)
#
#
#     if (compression)
#         sub("^(.+?)(\\.[^.]+(\\.(gz|bz2|xz))?)$", if (invert)
#             "\\1"
#         else "\\2", path)
#     else sub("^(.+)(\\.[^.]+)$", if (invert)
#         "\\1"
#     else "\\2", path)
# }


ext <- extension <- function (path, compression = FALSE, invert = FALSE)
{
    # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = TRUE )
    # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = FALSE)
    # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = TRUE )
    # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = FALSE)


    if (is.factor(path)) {
        levels(path) <- extension(
            path = levels(path),
            compression = compression,
            invert = invert
        )
        return(path)
    }
    else if (!is.character(path))
        path <- as.character(path)


    if (compression) {
        pattern1 <- "^(.*[^/\\])(\\.[^/\\.]+(\\.(gz|bz2|xz|tgz)))$"
        pattern2 <- "^(.*[^/\\])(\\.[^/\\.]+)$"
        if (invert) {
            i1 <- grepl(pattern1, path)
            path[i1] <- sub(pattern1, "\\1", path[i1])
            i2 <- !i1 & grepl(pattern2, path)
            path[i2] <- sub(pattern2, "\\1", path[i2])
        }
        else {
            i1 <- grepl(pattern1, path)
            path[i1] <- sub(pattern1, "\\2", path[i1])
            i2 <- !i1 & grepl(pattern2, path)
            path[i2] <- sub(pattern2, "\\2", path[i2])
            path[!i1 & !i2] <- ""
        }
    }
    else {
        pattern <- "^(.*[^/\\])(\\.[^/\\.]+)$"
        if (invert)
            path[] <- sub(pattern, "\\1", path)
        else {
            i <- grepl(pattern, path)
            path[i] <- sub(pattern, "\\2", path[i])
            path[!i] <- ""
        }
    }
    path
}


rm.ext <- remove.extension <- function (path, compression = FALSE)
extension(path = path, compression = compression, invert = TRUE)

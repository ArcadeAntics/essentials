ext <- extension <- function (path, compression = FALSE, invert = FALSE)
{
    # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = TRUE )
    # extension("this.path_0.5.0.tar.gz", compression = FALSE, invert = FALSE)
    # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = TRUE )
    # extension("this.path_0.5.0.tar.gz", compression = TRUE , invert = FALSE)


    if (compression)
        sub("^(.+?)(\\.[^.]+(\\.(gz|bz2|xz))?)$", if (invert)
            "\\1"
        else "\\2", path)
    else sub("^(.+)(\\.[^.]+)$", if (invert)
        "\\1"
    else "\\2", path)
}


rm.ext <- remove.extension <- function (path, compression = FALSE)
extension(path = path, compression = compression, invert = TRUE)

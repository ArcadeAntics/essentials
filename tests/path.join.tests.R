# on windows and pasting changes encoding
if (.Platform$OS.type == "windows" &&
    {
        x <- "\xe9"
        Encoding(x) <- "latin1"
        Encoding(paste(x)) != "latin1"
    }) {


    `%eq%` <- function (x, y)
    {
        x == y && Encoding(x) == Encoding(y)
    }


    # only 1 element
    x <- "\xe9"
    Encoding(x) <- "latin1"
    stopifnot(x %eq% essentials::path.join(x))


    # last element is a tilde path
    {
        x <- "~/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join(paste0(x, "/testing"), x))


        x <- "~\\\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join(paste0(x, "/testing"), x))


        x <- "~"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join(paste0(x, "/testing"), x))
    }


    # no drives, last path is absolute
    {
        x <- "/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", "/test4", "test5", x))


        x <- "\\\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", "/test4", "test5", x))
    }


    # last element contains the 1 and only drive
    {
        x <- "C:/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", x))


        x <- "C:\\\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", x))


        x <- "C:\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", x))


        x <- "//host/share/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", x))


        x <- "\\\\host\\share\\\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("/test1", "test2", "test3", x))
    }


    # multiple drives, last element is unc drive
    {
        x <- "//host/share/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("C:/", x))


        x <- "\\\\host\\share\\\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("C:/", x))
    }


    # multiple drives, last element is letter drive
    {
        x <- "C:/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("C:/", x))


        x <- "C:/\xe9"
        Encoding(x) <- "latin1"
        stopifnot(x %eq% essentials::path.join("C:/", x))
    }


    # multiple unique drives, last element is letter drive with non-absolute path
    {
        x <- "C:\xe9"
        Encoding(x) <- "latin1"
        stopifnot(
            x %eq% essentials::path.join("D:/", x),
            x %eq% essentials::path.join("D:/", "C:", "C:", x)
        )
    }
}

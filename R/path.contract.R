path.contract <- function (path, ignore.case = os.windows, home = "~")
{
    if (!is.character(path))
        stop("a character vector argument expected", domain = "R")
    if (!length(path))
        return(character())
    attributes(path) <- NULL
    home <- path.expand(home)
    if (home == "~")  ## if the home directory is unknown or none is specified
        return(path)
    opath <- path
    if (os.windows) {
        path <- chartr("\\", "/", path)
        home <- chartr("\\", "/", home)
    }
    if (ignore.case) {
        path <- tolower(path)
        home <- tolower(home)
    }
    nc <- nchar(home) + 1L
    contract <- startsWith(path, home) & substr(path, nc, nc) %in% c("/", "")
    if (any(contract))
        opath[contract] <- paste0("~", substr(opath[contract], nc, 1000000L))
    opath
}


# path.contract <- compiler::cmpfun(utils::removeSource(`environment<-`(path.contract, getNamespace("essentials"))))
#
#
# path <- f.str("%{Sys.getenv('LOCALAPPDATA')}s\\Test\\code.R")
# path <- path.contract(path, home = Sys.getenv("LOCALAPPDATA"))
# path

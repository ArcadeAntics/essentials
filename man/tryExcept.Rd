\name{tryExcept}
\alias{tryExcept}
\title{Condition Handling and Recovery}
\description{
  \code{tryExcept} provides a mechanism for handling unusual conditions,
  including errors and warnings.
}
\usage{
tryExcept(expr, ..., finally)
}
\arguments{
  \item{expr}{expression to be evaluated.}

  \item{\dots}{handlers established for the duration of the evaluation of
    \code{expr}.}

  \item{finally}{expression to be evaluated before returning or exiting.}
}
\details{
  If \code{finally} is missing or a simple expression, \code{tryExcept} and
  \code{\link[base:conditions]{tryCatch}} will behave the same.

  However, if \code{finally} is a \emph{compound expression}, usually of the
  form \code{{ expr1 ; expr2 }}, \code{tryExcept} will split the compound
  expression into its elements and put each within their own
  \code{\link[base]{on.exit}}, whereas \code{\link[base:conditions]{tryCatch}}
  would put the whole compound expression into one \code{\link[base]{on.exit}}.
  This means that all elements of \code{finally} in \code{tryExcept} will be
  run even if one throws an error, but in
  \code{\link[base:conditions]{tryCatch}}, \code{finally} will run up until the
  first error.
}
\examples{
# the following example won't work using 'utils::example' because it signals
# errors, so copy and paste this code into the R Console


\dontrun{
tryCatch({
    stop("error in 'expr'")
}, finally = {
    stop("tryCatch will not reach the second expression in 'finally'")
    stop("but tryExcept will")
})


essentials::tryExcept({
    stop("error in 'expr'")
}, finally = {
    stop("tryCatch will not reach the second expression in 'finally'")
    stop("but tryExcept will")
})


essentials::tryExcept({
    stop("error in 'expr'")
}, finally = {
    cat("this environment = "); print(environment())
    stop("err1")
    stop("err2")
    print(5 + 6)
    stop("err3")


    # just checking that the arguments aren't evaluated in the wrong frame
    # (why would they be though??)
    expr
    finally


    # testing that code with a source reference will be evaluated properly
    print(function(x) {
        x  # testing comments only appearing in source reference
    })


    stop("err4")
    print(6 + 7)


})


essentials::tryExcept({
    cat("this one should behave the same as tryCatch",
        "because 'finally' is not a compound expression",
        "(is not of class \\"\{\\")",
        sep = "\\n")
    stop("error in 'expr'")
}, finally =


    # checking again that 'finally' is evaluated in the correct environment
    cat("this environment =", utils::capture.output(environment()), "\\n"))


essentials::tryExcept({


    cat("Here is a situation in which you might actually use this.",
        "Suppose you're changing a bunch of settings and options that you want",
        "to reset when 'tryExcept' finishes. For example:",
        "* changing the working directory",
        "* changing options stored in `options()`",
        "* changing graphical parameters stored in `graphics::par()`",
        "* shutting down a graphics device with `dev.off()`",
        "* changing the current graphics device with `dev.set()`",
        "* changing the state of `grDevices::devAskNewPage()`",
        "* deleting a temporary file created with `tempfile()`",
        "    or downloaded with `utils::download.file()`",
        "* closing a connection",
        "* changing the random number generator state",
        "    with `RNGkind()` or `set.seed()`",
        "* any other type of cleaning process",
        sep = "\\n")
    owd <- getwd()
    oopt <- options(max.print = 10, digits = 17)
    odev <- grDevices::dev.cur()
    if (names(odev) != "null device")
        oldask <- grDevices::devAskNewPage(ask = FALSE)
    FILE <- tempfile()
    setwd(dirname(FILE))
    con <- file(FILE, "w")
    if (has.Random.seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        oldSeed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    else oldRNG <- RNGkind()
    RNGkind("default", "default", "default")
    set.seed(1)


    cat("\\nYou've setup your settings and options above",
        "so now we do something with it", sep = "\\n")


    cat("\\nOnly 10 of these will print\\n")
    print(1:100)


    cat("\\nWe're plotting an image which we will remove afterwards\\n")
    plot(1:10)
    cdev <- grDevices::dev.cur()
    Sys.sleep(2)


}, finally = {


    cat("\\nNow clean up the everything, but unlike 'tryCatch', run all",
        "cleaning steps even if one signals an error", sep = "\\n")


    grDevices::dev.off(cdev)
    close(con)
    file.remove(FILE)


    if (has.Random.seed)
        assign(".Random.seed", oldSeed, envir = globalenv(), inherits = FALSE)
    else RNGkind(oldRNG[1L], oldRNG[2L], oldRNG[3L])


    if (names(odev) != "null device") {
        grDevices::dev.set(oldask)
        grDevices::devAskNewPage(oldask)
    }


    setwd(owd)
    options(oopt)
})
}
}

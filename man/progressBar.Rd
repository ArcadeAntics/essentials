\name{progressBar}
\alias{progressBar}
\alias{setProgressBarMethod}
\title{Progress Bars}
\description{
  Generalized progress bars, with 3 default flavours.
}
\usage{
progressBar(name = names(progressBars), ...)

# declare a new progress bar
setProgressBarMethod(name, definition)

\S3method{close}{progressBar}(con, ...)
}
\arguments{
  \item{name}{character string, which kind of progress bar to make or set a
    method. By default, one of \code{"txt"}, \code{"win"} (Windows exclusive),
    and \code{"tk"}.}

  \item{\dots}{Further arguments passed to other methods.}

  \item{definition}{a function that will create a progress bar corresponding to
    \code{name}.}

  \item{con}{progress bar to be closed.}
}
\value{
  A \code{"progressBar"} object.
}
\seealso{
  \code{\link[utils]{txtProgressBar}}

#ifdef windows
  \code{\link[utils]{winProgressBar}}
#endif

  \code{\link[tcltk]{tkProgressBar}}
}
\examples{
# if we use 'tryCatch', we can make use 'finally' to guarantee the progress bar
# is closed, regardless of signalling an error or user interupt. not entirely
# useful in this example, but it can be in longer, more complicated situations
tryCatch({
    pb <- progressBar(if (.Platform$OS.type == "windows")
        "win"
    else "txt", max = 20, style = 3)
    for (i in 1:20) {
        Sys.sleep(0.05)
        pb$increment()
    }
}, finally = {
    close(pb)
})
}

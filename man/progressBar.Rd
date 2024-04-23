\name{progressBar}
\alias{progressBar}
\alias{setProgressBarMethod}
\alias{getProgress}
\alias{setProgress}
\alias{increment}
\alias{decrement}
\title{Progress Bars}
\description{
  Generalized progress bars, with 3 default flavours.
}
\usage{
progressBar(name = names(progressBars), ...)

getProgress(pb)
setProgress(pb, ...)

# declare a new progress bar
setProgressBarMethod(name, definition)

increment(x, ...)
decrement(x, ...)
}
\arguments{
  \item{name}{character string, which kind of progress bar to make or set a
    method. By default, one of \code{"tk"}, \code{"txt"}, and \code{"win"}
    (Windows exclusive).}

  \item{definition}{a function that will create a progress bar corresponding to
    \code{name}.}

  \item{pb, x}{a progress bar object.}

  \item{\dots}{Further arguments passed to other methods.}
}
\value{
  For \code{progressBar}, a progress bar object.

  For \code{getProgress}, \code{setProgress}, \code{increment}, and
  \code{decrement}, a length-one numeric vector giving the previous value
  (visibly for \code{getProgress}).
}
\seealso{
  \code{\link[tcltk]{tkProgressBar}}, \code{\link[utils]{txtProgressBar}}, and
  \code{winProgressBar} on Windows.
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
        increment(pb)
    }
}, finally = close(pb))
}

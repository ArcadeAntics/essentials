\name{as.colorRampPalette}
\alias{as.colorRampPalette}
\title{
Color Interpolation
}
\description{
Create a function that interpolates a set of colors to produce new color
palettes.
}
\usage{
as.colorRampPalette(...)
}
\arguments{
  \item{\dots}{
arguments to pass to \code{\link{colorRamp}}.
  }
}
\details{
This is a preferable alternative to \code{\link{colorRampPalette}} since the
returned function has more arguments. The returned function has arguments
\code{start}, \code{end}, \code{alpha}, and \code{rev}, similar to
\code{\link{rainbow}} or \code{\link{viridis.colors}}.

If only one argument is provided that happens to be a function, it is assumed
that this function is the function that maps the interval [0,1] to a series of
colors, and so the argument will \emph{not} be passed to \code{colorRamp}.
}
\value{
A function which takes an integer \code{n} and returns a character vector of
\code{n} colors.
}
\examples{
crp <- as.colorRampPalette(1:9)

show.colors(crp(20))

show.colors(crp(20, end = 19/20))

show.colors(crp(20, alpha = 0.75))

show.colors(crp(20, rev = TRUE))

show.colors(crp(20, end = 19/20, alpha = 0.75, rev = TRUE))
}

\name{hcl.colors2}
\alias{hcl.colors2}
\alias{inferno.colors}
\alias{plasma.colors}
\alias{viridis.colors}
\alias{gg.colors}
\alias{show.colors}
\title{
Color Palettes
}
\description{
Create a vector of \code{n} contiguous colors.
}
\usage{
hcl.colors2(n, palette = "viridis", start = 0, end = if (palette \%in\%
    special.palettes) (n - 1)/n else 1, alpha, rev = FALSE)

inferno.colors(n, start = 0, end = 1, alpha, rev = FALSE)
plasma.colors(n, start = 0, end = 1, alpha, rev = FALSE)
viridis.colors(n, start = 0, end = 1, alpha, rev = FALSE)

gg.colors(n, start = 0, end = (n - 1)/n, alpha, rev = FALSE)

show.colors(x)
}
\arguments{
  \item{n}{
the number of colors (\eqn{\ge 1}) to be in the palette.
  }
  \item{palette}{
a valid palette name (one of \code{\link{hcl.pals}()}). The name is matched to
the list of available palettes, ignoring upper vs. lower case, spaces, dashes,
etc. in the matching.
  }
  \item{alpha}{
an alpha-transparency level in the range [0,1] (0 means transparent and 1 means
opaque).
  }
  \item{rev}{
logical indicating whether the ordering of the colors should be reversed.
  }
  \item{start}{
a number in the range [0,1] at which the vector of colors begins.
  }
  \item{end}{
a number in the range [0,1] at which the vector of colors ends.
  }
  \item{x}{
a vector of colors to display.
  }
}
\details{
\code{hcl.colors2} is a variant of \code{\link{hcl.colors}} that adds a
\code{start} and \code{end} argument. \code{hcl.colors2} is slightly faster than
\code{hcl.colors}, though it is not by much.
}
\value{
For \code{show.colors}, its argument.

For all others, a character vector of \code{n} hex color codes.
}
\examples{
show.colors(hcl.colors2(256, palette = "Spectral"))


show.colors(inferno.colors(256))
show.colors(plasma.colors(256))
show.colors(viridis.colors(256, end = 0.9))


show.colors(gg.colors(256))
}

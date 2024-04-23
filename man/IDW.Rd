\name{InverseDistanceWeighting}
\alias{InverseDistanceWeighting}
\alias{IDW}
\title{Inverse Distance Weighting}
\description{
  Interpolate values in n-th dimensional space.
}
\usage{
IDW(x0, u0, x, p = 2, na.rm = FALSE)
}
\arguments{
  \item{x0, x}{numeric matrix of coordinates. Each row is a new coordinate, and
    each column is a new dimension.}

  \item{u0}{numeric or complex vector; known values corresponding to the
    coordinates of \code{x0}.}

  \item{p}{a positive number; the influence of closer points.}

  \item{na.rm}{logical. Should missing values (including \code{NaN}) in
    \code{u0} and rows containing missing values in \code{x0} be removed?}
}
\details{
  \code{x0} and \code{x} must have the same number of dimensions.
}
\value{
  a numeric or complex vector, equal in length to the number of rows of
  \code{x}.
}
\examples{
x0 <- c(0, 1, 4, 5)
u0 <- c(1, 2, 2, 1)
x <- seq.int(-4, 9, length.out = 1001)
u <- IDW(x0, u0, x)
graphics::plot(
    panel.first = graphics::grid(col = "gray69"),
    x, u, type = "l", col = "blue", lwd = 2
)
graphics::points(x0, u0, pch = 16, cex = 1.5)
}

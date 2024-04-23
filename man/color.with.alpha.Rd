\name{color.with.alpha}
\alias{color.with.alpha}
\alias{as.hex.code}
\title{Color Opacity}
\description{
  Change the alpha (opacity) of a series of colors.
}
\usage{
color.with.alpha(x, alpha)
as.hex.code(x)
}
\arguments{
  \item{x}{character vector, a matrix as produced by
    \code{\link[grDevices]{col2rgb}}, or a matrix or data.frame where the first
    three columns are the red, green, and blue values.}

  \item{alpha}{an alpha-transparency level in the range [0,1] (0 means
    transparent and 1 means opaque).}
}
\details{
  When \code{alpha} is missing, the alpha-transparency is taken from \code{x}.
  In that case, \code{color.with.alpha} is just decoding the colors into a
  character vector of hex colors codes, and not actually changing the opacity
  of the colors.
}
\value{
  A character vector of hex color codes.
}
\examples{
example.colors <- c("red", "#FFA50099", "yellow", "green", "blue", "purple")
show.colors(example.colors)
show.colors(color.with.alpha(example.colors, alpha = NULL))  # remove all opacity
show.colors(color.with.alpha(example.colors, alpha = 0.25))  # one quarter opaque
show.colors(color.with.alpha(example.colors, alpha = 0.5))   # half opaque
show.colors(color.with.alpha(example.colors, alpha = 0.75))  # three quarters opaque
show.colors(color.with.alpha(example.colors, alpha = 1))     # fully opaque, same as NULL


example.colors2 <- 1:8
show.colors(example.colors2)
show.colors(color.with.alpha(example.colors2, alpha = 0.5))
show.colors(color.with.alpha(example.colors2, alpha = 0.75))


example.colors3 <- grDevices::col2rgb(gg.colors(10))


## show.colors would not be able to show example.colors3 because it's a matrix
## use as.hex.codes to decode the values to their hex codes, then display
show.colors(as.hex.code(example.colors3))
show.colors(color.with.alpha(example.colors3, alpha = 0.5))
show.colors(color.with.alpha(example.colors3, alpha = 0.75))


example.colors4 <- data.frame(
    red   = c( 75,  66,  24,   0,   0,   0,   0, 108, 187, 253)/255,
    green = c(  0,  44,  80, 112, 142, 168, 190, 208, 221, 227)/255,
    blue  = c( 85, 112, 134, 148, 152, 144, 125,  94,  56,  51)/255
)
show.colors(as.hex.code(example.colors4))
show.colors(color.with.alpha(example.colors4, alpha = 0.5))
show.colors(color.with.alpha(example.colors4, alpha = 0.75))

# alpha can be a vector, here we provide 10 different alpha values
show.colors(color.with.alpha(example.colors4,
    alpha = seq.int(0.5, 1, length.out = 10)))
}

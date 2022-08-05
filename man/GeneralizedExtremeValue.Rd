\name{GeneralizedExtremeValue}
\alias{GeneralizedExtremeValue}
\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}
\title{
The Generalized Extreme Value Distribution
}
\description{
Density, distribution function, quantile function and random generation for the
generalized extreme value distribution with location equal to \code{location},
scale equal to \code{scale} and shape equal to \code{shape}.
}
\usage{
dgev(x, location = 0, scale = 1, shape = 0, log = FALSE)
pgev(q, location = 0, scale = 1, shape = 0, lower.tail = TRUE, log.p = FALSE)
qgev(p, location = 0, scale = 1, shape = 0, lower.tail = TRUE, log.p = FALSE)
rgev(n, location = 0, scale = 1, shape = 0)
}
\arguments{
  \item{x, q}{
vector of quantiles.
  }
  \item{p}{
vector of probabilities.
  }
  \item{n}{
number of observations. If \code{length(n) > 1}, the length is taken to be the
number required.
  }
  \item{location}{
vector of locations.
  }
  \item{scale}{
vector of scales.
  }
  \item{shape}{
vector of shapes.
  }
  \item{log, log.p}{
logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
logical; if TRUE (default), probabilities are \eqn{P[X \le x]} otherwise,
\eqn{P[X > x]}.
  }
}
\details{
If \code{location}, \code{scale} or \code{shape} are not specified they assume
the default values of 0, 1 and 0, respectively.

The generalized extreme value distribution with location \eqn{\mu}, scale
\eqn{\sigma > 0} and shape \eqn{\xi} has density

\deqn{f(s) = \frac{1}{\sigma} (1 + \xi s)^{-(1 + 1/\xi)} \exp(-(1 + \xi s)^{-1/\xi})}{
      f(s) = 1/\sigma (1 + \xi s)^-(1 + 1/\xi) e^-((1 + \xi s)^{-1/\xi})}

for \eqn{1 + \xi s > 0} where
\eqn{s = \frac{(x - \mu)}{\sigma}}{
     s = (x - \mu)/\sigma}.
In the limit \eqn{\xi → 0}, the density simplifies to

\deqn{f(s) = \frac{1}{\sigma} \exp(-s) \exp(-\exp(-s))}{
      f(s) = 1/\sigma e^-s e^-(e^-s)}
}
\value{
\code{dgev} gives the density, \code{pgev} gives the distribution function,
\code{qgev} gives the quantile function, and \code{rgev} generates random
deviates.

The length of the result is determined by \code{n} for \code{rgev}, and is the
maximum of the lengths of the numerical arguments for the other functions.

The numerical arguments other than \code{n} are recycled to the length of the
result. Only the first elements of the logical arguments are used.

For \code{scale = 0} this gives the limit as \eqn{\sigma} decreases to 0, a
point mass at \eqn{\mu}. \code{scale < 0} is an error and returns \code{NaN}.
}
\examples{
shapes <- expression(-1/2, 0, +1/2)
legend.text <- as.expression(lapply(shapes, function(shape) {
    call("==", as.symbol("xi"), shape)
}))
shapes <- vapply(shapes, eval, 0)
cols <- c("green3", "red", "blue")
x <- seq.int(-4, 4, length.out = 1001)


# we use plapply here instead of lapply because
# plapply allows us to name the looping arguments
ys <- essentials::plapply(
    list(shape = shapes),
    essentials::dgev,
    x = x
)
graphics::par(mar = c(4.9, 4.5, 2.1, 0.4))
graphics::plot(
    xlim = range(x), ylim = range(ys),
    panel.first = graphics::grid(col = "gray69"),
    x = NA_real_, y = NA_real_,
    xlab = "x", ylab = ~f(list(x, mu, sigma, xi)),
    main = "Probability density function",
    bty = "L"
)
essentials::mfor(y, col, list(ys, cols), {
    graphics::lines(x, y, col = col, lwd = 2)
})
graphics::legend(
    x = "topleft",
    legend = legend.text,
    col = cols,
    lwd = 2,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ list(mu == 0, sigma == 1), adj = 1)





ys <- essentials::plapply(
    list(shape = shapes),
    essentials::pgev,
    q = x
)
graphics::plot(
    xlim = range(x), ylim = c(0, 1),
    panel.first = graphics::grid(col = "gray69"),
    x = NA_real_, y = NA_real_,
    xlab = "x", ylab = ~F(list(x, mu, sigma, xi)),
    main = "Cumulative probability function",
    bty = "L"
)
essentials::mfor(y, col, list(ys, cols), {
    graphics::lines(x, y, col = col, lwd = 2)
})
graphics::legend(
    x = "topleft",
    legend = legend.text,
    col = cols,
    lwd = 2,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ list(mu == 0, sigma == 1), adj = 1)
}

\name{GeneralizedPareto}
\alias{GeneralizedPareto}
\alias{dgpd}
\alias{pgpd}
\alias{qgpd}
\alias{rgpd}
\title{
The Generalized Pareto Distribution
}
\description{
Density, distribution function, quantile function and random generation for the
generalized pareto distribution with location equal to \code{location}, scale
equal to \code{scale} and shape equal to \code{shape}.
}
\usage{
dgpd(x, location = 0, scale = 1, shape = 0, log = FALSE)
pgpd(q, location = 0, scale = 1, shape = 0, lower.tail = TRUE, log.p = FALSE)
qgpd(p, location = 0, scale = 1, shape = 0, lower.tail = TRUE, log.p = FALSE)
rgpd(n, location = 0, scale = 1, shape = 0)
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
vector of scale.
  }
  \item{shape}{
vector of shapes.
  }
  \item{log, log.p}{
logical; if TRUE, probabilities p are given as log(p).
  }
  \item{lower.tail}{
logical; if TRUE (default), probabilities are \eqn{P[X \le x]}{P[X <= x]}
otherwise, \eqn{P[X > x]}.
  }
}
\details{
If \code{location}, \code{scale} or \code{shape} are not specified they assume
the default values of \code{0}, \code{1} and \code{0}, respectively.

The generalized pareto distribution with location \eqn{\mu}, scale
\eqn{\sigma > 0} and shape \eqn{\xi} has density

\deqn{f(s) = \frac{1}{\sigma} (1 + \xi s)^{-(1 + 1/\xi)}}{
      f(s) = 1/\sigma (1 + \xi s)^-(1 + 1/\xi)}

for \eqn{s \ge 0}{s >= 0} when \eqn{\xi \ge 0}{xi >= 0}, and for
\eqn{0 \le s \le -1/\xi}{0 <= s <= -1/xi} when \eqn{\xi \lt 0}{xi < 0}, where \eqn{s = \frac{(x - \mu)}{\sigma}}{
     s = (x - \mu)/\sigma}. In the limit \eqn{\xi → 0}{\xi -> 0}, the density simplifies
to

\deqn{f(s) = \frac{1}{\sigma} \exp(-s)}{
      f(s) = 1/\sigma e^-s}
}
\value{
\code{dgpd} gives the density, \code{pgpd} gives the distribution function,
\code{qgpd} gives the quantile function, and \code{rgpd} generates random
deviates.

The length of the result is determined by \code{n} for \code{rgpd}, and is the
maximum of the lengths of the numerical arguments for the other functions.

The numerical arguments other than \code{n} are recycled to the length of the
result. Only the first elements of the logical arguments are used.

For \code{scale = 0} this gives the limit as \eqn{\sigma} decreases to 0, a
point mass at \eqn{\mu}. \code{scale < 0} is an error and returns \code{NaN}.
}
\examples{
scales <- c( 1,  1,  1,  2,  2,  2)
shapes <- c( 1,  5, 20,  1,  5, 20)
legend.text <- as.expression(essentials::plapply(list(scales, shapes),
    function(scale, shape) {
        call("list",
             call("==", as.symbol("sigma"), scale),
             call("==", as.symbol("xi"), shape))
    }))
cols <- c("red", "green3", "blue", "red", "green3", "blue")
ltys <- c("solid", "solid", "solid", "dashed", "dashed", "dashed")
x <- seq.int(0, 5, length.out = 1001)


# we use plapply here instead of lapply because
# plapply allows us to name the looping arguments
ys <- essentials::plapply(
    list(scale = scales, shape = shapes),
    essentials::dgpd,
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
essentials::mfor(y, col, lty, list(ys, cols, ltys), {
    graphics::lines(x, y, col = col, lty = lty, lwd = 2)
})
graphics::legend(
    x = "topright",
    legend = legend.text,
    col = cols,
    lwd = 2,
    lty = ltys,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ mu == 0, adj = 1)





ys <- essentials::plapply(
    list(scale = scales, shape = shapes),
    essentials::pgpd,
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
essentials::mfor(y, col, lty, list(ys, cols, ltys), {
    graphics::lines(x, y, col = col, lty = lty, lwd = 2)
})
graphics::legend(
    x = "topleft",
    legend = legend.text,
    col = cols,
    lwd = 2,
    lty = ltys,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ mu == 0, adj = 1)
}

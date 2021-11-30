\name{hypot}
\alias{hypot}
\alias{phypot}
\title{
Hypotenuse
}
\description{
\code{hypot} returns the \dQuote{hypotenuse} of all the values present in its
arguments.

\code{phypot} returns the \strong{p}arallel \dQuote{hypotenuse} of the input
values. It takes any number of vectors as arguments, recycle them to common
length, and return a single vector giving the \sQuote{\emph{parallel}}
\dQuote{hypotenuse} of the argument vectors.
}
\usage{
hypot(\dots, na.rm = FALSE)
phypot(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{
numeric or complex arguments.
  }
  \item{na.rm}{
a logical indicating whether missing values should be removed.
  }
}
\details{
The hypotenuse is the longest side of a right-angled triangle, the side opposite
the right angle. The length of the hypotenuse is defined as

\deqn{\sqrt(x^2 + y^2)}{
      \sqrt(x^2 + y^2)}

The 3-dimensional \dQuote{hypotenuse} is defined as

\deqn{\sqrt(x^2 + y^2 + z^2)}{
      \sqrt(x^2 + y^2 + z^2)}

The n-dimensional \dQuote{hypotenuse} is defined as

\deqn{\sqrt{\sum_{k = 1}^n x_k^2}}{
      \sqrt(\sum x[k]^2 for k = 1, \ldots, n)}

Suppose we have a vector \code{x} and we want to know its \dQuote{hypotenuse}.

If any of \code{x} is infinite, the \dQuote{hypotenuse} is always \code{Inf}.

If \code{na.rm = FALSE} and any of \code{x} is \code{NA} or \code{NaN}, the
\dQuote{hypotenuse} is \code{NaN}.

Otherwise, the \dQuote{hypotenuse} will be calculated using the above
definition. If \code{na.rm = TRUE} all \code{NA} and \code{NaN} values are
treated as \code{0}.
}
\value{
For \code{hypot} a numeric vector of length 1.

For \code{phypot} a numeric vector. If any of the input values is a zero-length
vector the result has length zero. Otherwise, the result has length equal to the
length of the longest vector. The rules for determining the attributes of the
result are rather complicated. Attributes are only copied from input values
whose lengths are equal to the length of the result. If any such input values
have a \code{\link{dim}} attribute, the first \code{dim} attribute is copied to
the result. \code{\link{dimnames}} are copied in a similar manner (but only
\emph{after} the result has a \code{dim} attribute). If any such input values
have a \emph{conformable} \code{dimnames} attribute, the first conformable
\code{dimnames} attribute is copied to the result. If a \code{dim} attribute has
\emph{not} been assigned to the result, then finally \code{\link{names}} are
copied in a similar manner. If any such input values have a \code{names}
attribute, the first \code{names} attribute is copied to the result. A result
can have a \code{dim} attribute, a \code{names} attribute, neither, but cannot
have both. \code{dim} has priority over \code{names} (similar to
\code{\link{Arithmetic}} operators).
}
\note{
\sQuote{Numeric} arguments are vectors of type integer and numeric, and logical
(coerced to integer). \code{NULL} is accepted as equivalent to
\code{numeric(0)}.
}
\examples{
## when a side is infinite, the hypotenuse is Inf
hypot(Inf, NaN)   # Inf
hypot(-Inf, NaN)  # Inf (applies to negative infinity too)

## when a side is NA or NaN, the hypotenuse is NaN
hypot(NaN, 0)     # NaN
hypot(NA , 0)     # NaN

## numbers whose squares would overflow normally are handled well
hypot(.Machine$double.xmax, 5     )
hypot(1e+300              , 1e+300)


## hypotenuse
hypot(3, 4)      # 5
hypot(3+4i)      # 5 (works for complex numbers as well)

## 3-dimensional "hypotenuse"
hypot(3, 4, 12)  # 13

## n-dimensional "hypotenuse"
hypot(1:100)


x <- seq.int(-3, 3, length.out = 101)
y <- 1
(h <- phypot(x, 1))  # parallel hypotenuse
graphics::plot(
    panel.first = graphics::grid(col = "gray69"),
    x = x, y = h, type = "l",
    main = "Distance from" ~ (list(0, 0)) ~ "to" ~ (list(x, 1))
)
}

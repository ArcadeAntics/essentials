\name{scalar}
\alias{scalar}
\alias{as.scalar}
\alias{is.scalar}
\alias{as.scalar.logical}
\alias{as.scalar.integer}
\alias{as.scalar.double}
\alias{as.scalar.numeric}
\alias{as.scalar.complex}
\alias{as.scalar.character}
\alias{as.scalar.string}
\alias{as.scalar.raw}
\alias{as.scalar.number}
\title{
Scalars
}
\description{
Coerce objects to scalars (vectors of length 1).
}
\usage{
as.scalar(x, mode = "any")
is.scalar(x, mode = "any")

as.scalar.logical(x)

as.scalar.integer(x)

as.scalar.double(x)
as.scalar.numeric(x)

as.scalar.complex(x)

as.scalar.character(x)
as.scalar.string(x)

as.scalar.raw(x)

as.scalar.number(x, strict = TRUE)
}
\arguments{
  \item{x}{
object to be coerced or tested.
  }
  \item{mode}{
character string naming an atomic mode or \code{"any"}.
  }
  \item{strict}{
\code{TRUE} or \code{FALSE}, should a complex number that is strictly real (real
component is NA or NaN or imaginary component is NA or NaN or 0) be converted to
a real number?
  }
}
\details{
\code{as.scalar.logical} coerces an object to a vector of type \dQuote{logical}
of length 1.

\code{as.scalar.integer} coerces an object to a vector of type \dQuote{integer}
of length 1.

\code{as.scalar.real}, \code{as.scalar.double} and \code{as.scalar.numeric}
coerces an object to a vector of type \dQuote{numeric} of length 1.

\code{as.scalar.complex} coerces an object to a vector of type \dQuote{complex}
of length 1.

\code{as.scalar.number} coerces an object to a vector of type \dQuote{numeric}
or \dQuote{complex} of length 1.

\code{as.scalar.string} and \code{as.scalar.character} coerces an object to a
vector of type \dQuote{character} of length 1.

\code{as.scalar} coerces an object to a vector of length 1 of a specified mode.
}
\value{
A vector of length 1.
}
\examples{
## if the type converting from and converting to are identical,
## as.scalar() is a much shorter way of writing what you intend.
as.scalar(c(TRUE, FALSE, NA))
as.scalar(1:100)
as.scalar(1:10 + 0.5)
as.scalar(exp((0+1i) * 6 * (-4:4)))
as.scalar(letters)


## if the type converting from and converting to are not identical, it
## is better to specify the type converting to.
as.scalar.logical(c(TRUE, FALSE, NA))
as.scalar.integer(c(TRUE, FALSE, NA))
as.scalar.numeric(c(TRUE, FALSE, NA))
as.scalar.complex(c(TRUE, FALSE, NA))
as.scalar.character(c(TRUE, FALSE, NA))

as.scalar(TRUE, "character")
}

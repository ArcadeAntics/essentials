\name{strip}
\alias{strip}
\title{
Remove Leading and Trailing White Spaces
}
\description{
\code{strip} removes leading and trailing white space from each element of a
character vector. White space characters include tab, newline, vertical tab,
form feed, carriage return, space and possible other locale-dependent
characters.
}
\usage{
strip(x)
}
\arguments{
  \item{x}{
a character vector, or an object which can be coerced by \code{as.character} to
a character vector. \code{\link[base:LongVectors]{Long vectors}} are supported.
}
}
\value{
\code{strip} returns a character vector of the same length and with the same
attributes as \code{x} (after possible coercion to character). Elements of
character vectors \code{x} which are not substituted will be returned unchanged
(including any declared encoding).
}
\examples{
x <- c(
    "  the quick brown fox jumps over a lazy dog  ",
    "  the quick brown fox jumps over a lazy dog\t\n"
)
strip(x)  # the leading and trailing tab, newline, and space are removed


## x is intended to be in encoding latin1
x <- "fa\xE7ile"
Encoding(x) <- "latin1"
y <- strip(x)


## since 'x' has no leading or trailing white space, 'strip(x)' retains the
## encoding of 'x'
Encoding(y)
y
}

\name{seq2}
\alias{seq2}
\title{
Sequence Generation
}
\description{
Generate regular sequences.
}
\usage{
seq2(from, to, by, length.out, along.with, endpoint, ...)
}
\arguments{
  \item{from, to}{
the starting and (maximal) end values of the sequence. Of length 1 unless
just \code{from} is supplied.
  }
  \item{by}{
number: increment of the sequence.
  }
  \item{length.out}{
desired length of the sequence. A negative number will return a zero-length
array, while a non-negative number will be rounded up if fractional.
  }
  \item{along.with}{
take the length from the length of this argument.
  }
  \item{endpoint}{
if the last element of the result is equal to \code{to}, should it be kept?
  }
  \item{\dots}{
unused, to match the signature of \code{\link[base:seq]{seq.default}} and
\code{\link[base:seq]{seq.int}}.
  }
}
\details{
A negative \code{length.out} will return a zero-length array, unlike
\code{seq} where an error would be raised.

Additionally, specifying \code{to - from} and \code{by} of opposite signs will
return a zero-length array, unlike \code{seq} where an error would be raised.
}
\value{
A vector of type \code{"integer"} or \code{"double"}.
}
\examples{seq2(from =  26, endpoint = TRUE )
seq2(from =  26, endpoint = FALSE)
seq2(from =   0, endpoint = TRUE )
seq2(from =   0, endpoint = FALSE)
seq2(from = -26, endpoint = TRUE )
seq2(from = -26, endpoint = FALSE)


seq2(from = letters, endpoint = TRUE )
seq2(from = letters, endpoint = FALSE)


seq2(to =  26, endpoint = TRUE )
seq2(to =  26, endpoint = FALSE)
seq2(to = -24, endpoint = TRUE )
seq2(to = -24, endpoint = FALSE)


seq2(by =  26, endpoint = TRUE )
seq2(by =  26, endpoint = FALSE)
seq2(by =   0, endpoint = TRUE )
seq2(by =   0, endpoint = FALSE)
seq2(by = -26, endpoint = TRUE )
seq2(by = -26, endpoint = FALSE)


seq2(length.out =  26, endpoint = TRUE )
seq2(length.out =  26, endpoint = FALSE)
seq2(length.out =   0, endpoint = TRUE )
seq2(length.out =   0, endpoint = FALSE)
seq2(length.out = -26, endpoint = TRUE )
seq2(length.out = -26, endpoint = FALSE)


seq2(along.with = letters, endpoint = TRUE )
seq2(along.with = letters, endpoint = FALSE)





seq2(from =  1, to = 26, endpoint = TRUE )
seq2(from =  1, to = 26, endpoint = FALSE)
seq2(from = 26, to =  1, endpoint = TRUE )
seq2(from = 26, to =  1, endpoint = FALSE)


seq2(from =  26, by =  1, endpoint = TRUE )
seq2(from =  26, by =  1, endpoint = FALSE)
seq2(from = -24, by =  1, endpoint = TRUE )
seq2(from = -24, by =  1, endpoint = FALSE)
seq2(from =  26, by = -1, endpoint = TRUE )
seq2(from =  26, by = -1, endpoint = FALSE)
seq2(from = -24, by = -1, endpoint = TRUE )
seq2(from = -24, by = -1, endpoint = FALSE)


seq2(from = 1, length.out = 26, endpoint = TRUE )
seq2(from = 1, length.out = 26, endpoint = FALSE)


seq2(from = 1, along.with = letters, endpoint = TRUE )
seq2(from = 1, along.with = letters, endpoint = FALSE)


seq2(to =  26, by =  1, endpoint = TRUE )
seq2(to =  26, by =  1, endpoint = FALSE)
seq2(to = -24, by =  1, endpoint = TRUE )
seq2(to = -24, by =  1, endpoint = FALSE)
seq2(to =  26, by = -1, endpoint = TRUE )
seq2(to =  26, by = -1, endpoint = FALSE)
seq2(to = -24, by = -1, endpoint = TRUE )
seq2(to = -24, by = -1, endpoint = FALSE)


seq2(to = 26, length.out = 26, endpoint = TRUE )
seq2(to = 26, length.out = 26, endpoint = FALSE)


seq2(to = 26, along.with = letters, endpoint = TRUE )
seq2(to = 26, along.with = letters, endpoint = FALSE)


seq2(by = 3, length.out = 26, endpoint = TRUE )
seq2(by = 3, length.out = 26, endpoint = FALSE)


seq2(by = 3, along.with = letters, endpoint = TRUE )
seq2(by = 3, along.with = letters, endpoint = FALSE)





seq2(from = -25, to = 25, by = 2, endpoint = TRUE )
seq2(from = -25, to = 25, by = 2, endpoint = FALSE)


seq2(from = 0, to = 25, length.out = 26, endpoint = TRUE )
seq2(from = 0, to = 25, length.out = 26, endpoint = FALSE)


seq2(from = 0, to = 25, along.with = letters, endpoint = TRUE )
seq2(from = 0, to = 25, along.with = letters, endpoint = FALSE)


seq2(from = 0, by = 2, length.out = 26, endpoint = TRUE )
seq2(from = 0, by = 2, length.out = 26, endpoint = FALSE)


seq2(from = 0, by = 2, along.with = letters, endpoint = TRUE )
seq2(from = 0, by = 2, along.with = letters, endpoint = FALSE)


seq2(to = 50, by = 2, length.out = 26, endpoint = TRUE )
seq2(to = 50, by = 2, length.out = 26, endpoint = FALSE)


seq2(to = 50, by = 2, along.with = letters, endpoint = TRUE )
seq2(to = 50, by = 2, along.with = letters, endpoint = FALSE)





seq2(length.out = 5)


seq2(0, 60, length.out = 21, endpoint = TRUE )
seq2(0, 60, length.out = 21, endpoint = FALSE)
seq2(0, 60, length.out = 20, endpoint = TRUE )
seq2(0, 60, length.out = 20, endpoint = FALSE)


seq2(60, length.out = 20, endpoint = FALSE)


seq2(-0.5, 0.5 - .Machine$double.eps/2, endpoint = FALSE)
seq2(-0.5, 0.5 - .Machine$double.eps/2, endpoint = TRUE )
seq2(-0.5, 0.5 - .Machine$double.eps/2, endpoint = FALSE, by = 1)
seq2(-0.5, 0.5 - .Machine$double.eps/2, endpoint = TRUE , by = 1)
seq2(-0.5, 0.5                        , endpoint = FALSE)
seq2(-0.5, 0.5                        , endpoint = TRUE )
seq2(-0.5, 0.5                        , endpoint = FALSE, by = 1)
seq2(-0.5, 0.5                        , endpoint = TRUE , by = 1)
seq2(-0.5, 0.5 + .Machine$double.eps/2, endpoint = FALSE)
seq2(-0.5, 0.5 + .Machine$double.eps/2, endpoint = TRUE )
seq2(-0.5, 0.5 + .Machine$double.eps/2, endpoint = FALSE, by = 1)
seq2(-0.5, 0.5 + .Machine$double.eps/2, endpoint = TRUE , by = 1)
}

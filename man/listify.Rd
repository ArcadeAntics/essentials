\name{listify}
\alias{listify}
\title{Create a List from an Object}
\description{
  Coerce an object to a list.
}
\usage{
listify(x)
}
\arguments{
  \item{x}{an object.}
}
\details{
  \code{listify} is mostly used in combination with \code{\link{mapply}} or
  \code{\link{.mapply}}. For the \code{dots} argument of \code{.mapply}, each
  element of which is looped over, \code{listify} is a convenient way to turn
  each element into an iterable version of itself.
}
\value{
  A list.
}
\note{
  \code{listify} has unusual behaviour for data.frame objects.

  An S4 data.frame (created by \code{x <- \link[methods]{new}("data.frame")})
  will return \code{x}. This is because \code{\link{inherits}(x, "list")} for
  an S4 object \code{x} uses
  \code{\link[methods:is]{extends}(class(x), "list")} to determine if \code{x}
  inherits from \code{"list"} (which it does for and S4 data.frame).

  An unmodified S3 data.frame (created by \code{x <- \link{data.frame}()}) will
  return \code{list(x)}. This is because \code{\link{inherits}(x, "list")} for
  an S3 object \code{x} with a \code{class} attribute uses \code{"list" \%in\%
  \link{oldClass}(x)} to determine if \code{x} inherits from \code{"list"}
  (which it does not in this case).

  An S3 data.frame could be modified (such as
  \code{x <- \link{structure}(data.frame(), class = c("data.frame", "list"))})
  such that \code{listify} would return \code{x}. Where
  \code{"list" \%in\% \link{oldClass}(x)} was \code{FALSE} before, it is
  \code{TRUE} here.
}
\examples{
listify(5)        # when 'x' is not a list, returns 'list(x)'
listify(list(5))  # when 'x' is a list, returns 'x'


## when 'x' is an S4 data.frame, returns 'x'
listify(methods::new("data.frame"))


## when 'x' is an unmodified S3 data.frame, returns 'list(x)'
listify(data.frame())


## S3 data.frame 'x' could be modified such that 'listify' returns 'x'
listify(structure(data.frame(), class = c("data.frame", "list")))
}

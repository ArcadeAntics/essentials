\name{flat.list}
\alias{flat.list}
\title{Combine Values into a Flat List (a List with no List Elements)}
\description{
  Construct a flat list (a list with no list elements).
}
\usage{
flat.list(...)
}
\arguments{
  \item{\dots}{objects, possible named.}
}
\details{
  \code{\link{rapply}} is used to find each non-list element, then is assigned
  into the next element of a list. As the non-list elements are being recursed
  through, the names are taken and assigned to the return list.
}
\value{
  A list.
}
\examples{
x <- list(a = 1:5, list(list(b = 6:10), c = 11:15), list(d = exp(-4)))
print(x)
flat.list(x)
flat.list(x, e = "testing")
}

\name{rowmatch}
\alias{rowmatch}
\alias{\%rowin\%}
\alias{row.match}
\alias{row.match,data.frame,data.frame-method}
\alias{row.match,data.frame,ANY-method}
\alias{row.match,ANY,data.frame-method}
\alias{\%row.in\%}
\alias{row.match.data.frame}
\title{Row Matching}
\description{
  \code{rowmatch} and \code{row.match} return a vector of the positions of
  (first) row matches of its first argument in its second.

  \code{\%rowin\%} and \code{\%row.in\%} are more intuituve interfaces as
  binary operator, which return a logical vector indicating if there is a row
  match or not for its left operand.
}
\usage{
rowmatch(x, table, nomatch = NA_integer_, incomparables = NULL)

x \%rowin\% table

row.match(x, table, nomatch = NA_integer_, incomparables = NULL)

\S4method{row.match}{data.frame,data.frame}(x, table, nomatch = NA_integer_, incomparables = NULL)

\S4method{row.match}{data.frame,ANY}(x, table, nomatch = NA_integer_, incomparables = NULL)

\S4method{row.match}{ANY,data.frame}(x, table, nomatch = NA_integer_, incomparables = NULL)

x \%row.in\% table

## specifically for row matching data frames
row.match.data.frame(x, table, nomatch = NA_integer_, incomparables = NULL)
}
\arguments{
  \item{x}{vector, matrix, array, data.frame or \code{NULL}: the row values to
    be matched.}

  \item{table}{vector, matrix, array, data.frame or \code{NULL}: the row values
    to be matched against.}

  \item{nomatch}{the value to be returned in the case when no row match is
    found. Note that it is coerced to \code{integer}.}

  \item{incomparables}{a vector, matrix, array or data.frame of rows that
    cannot be matched. Any row in \code{x} matching a value in this object is
    assigned the \code{nomatch} value. In \code{\link[base]{match}},
    \code{FALSE} is equivalent to \code{NULL}, the same is true here.}
}
\details{
  \code{\%rowin\%} and \code{\%row.in\%} are currently defined as
  \verb{"\%rowin\%" <- function(x, table) rowmatch(x, table, nomatch = 0) > 0}
  \verb{"\%row.in\%" <- function(x, table) row.match(x, table, nomatch = 0) > 0}

  \code{rowmatch} is intended for matrix-like objects including vectors,
  matrices, and arrays. Data frames are coerced to arrays.

  Similar to \code{\link{row.names}} being generic with \code{\link{rownames}}
  as the default method, \code{row.match} is generic with \code{rowmatch} as
  the default method.

  \code{row.match.data.frame} is the method used for \code{row.match} when one
  of \code{x}, \code{table} and \code{incomparables} is a data frame. It is
  available incase you want to call it directly. Instead of coercing to arrays,
  it coerces to data frames.

  That \code{\%rowin\%} and \code{\%row.in\%} never return \code{NA} makes them
  particularly useful in \code{if} conditions.
}
\value{
  A vector of the same length as the number of rows in \code{x}.

  \code{rowmatch} and \code{row.match}: An integer vector giving the row
  position in \code{table} of the first row match if there is a row match,
  otherwise \code{nomatch}.

  If the \code{i}-th row of \code{x} is found to equal the \code{j}-th row of
  \code{table} then the value returned in the \code{i}-th position of the
  return value is \code{j}, for the smallest possible \code{j}. If no match is
  found, the value is \code{nomatch}.

  \code{\%rowin\%} and \code{\%row.in\%}: A logical vector, indicating if a row
  match was located for each row of \code{x}: thus the values are \code{TRUE}
  or \code{FALSE} and never \code{NA}.
}

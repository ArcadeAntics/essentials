\name{wrapper}
\alias{wrapper}
\title{Wrapper Functions}
\description{
  Create a call that can be substituted into a wrapper function.
}
\usage{
wrapper(fun, defaults = NULL, with.pkg = TRUE)
}
\arguments{
  \item{fun}{a character string, symbol or call.}

  \item{defaults}{a list of default arguments for the call, or \code{NULL}.}

  \item{with.pkg}{logical; if the function is from a namespace, should the
    namespace name be used in the call?}
}
\details{
  A wrapper function is a function whose purpose is to call another function.
  Wrapper functions are useful for hiding details of a function's implementation.
}
\value{
  A call.
}
\examples{
# I don't particularly like that the function data.frame has
# the formal argument "check.names" default to TRUE. Here, we
# will use 'wrapper' to make a wrapper for data.frame that has
# "check.names" set to FALSE


# we want the function body to look like this
wrapper(data.frame)


## make the function with the appropriate function body
data.frame2 <- function() NULL
body(data.frame2) <- wrapper(data.frame)


## add the function formals, changing "check.names" from TRUE to FALSE
formals(data.frame2) <- formals(data.frame)
formals(data.frame2)$check.names <- FALSE


print(data.frame2)
}

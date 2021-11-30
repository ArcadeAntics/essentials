\name{.plapply}
\alias{.plapply}
\alias{.psapply}
\alias{.pvapply}
\title{
Apply a Function to Multiple List or Vector Arguments
}
\description{
Alternate versions of \code{plapply}, \code{psapply}, and \code{pvapply} which
accept an argument \code{dots} instead of \code{\dots}.
}
\usage{
.plapply(X, FUN, dots = NULL)

.psapply(X, FUN, dots = NULL, simplify = TRUE, USE.NAMES = TRUE)

.pvapply(X, FUN, FUN.VALUE, dots = NULL, USE.NAMES = TRUE)
}
\arguments{
  \item{X}{
list of arguments to vectorize over.
  }
  \item{FUN}{
function to apply, found via \code{\link[base]{match.fun}}.
  }
  \item{dots}{
list of optional arguments to \code{FUN}.
  }
  \item{simplify}{
logical or character string; should the result be simplified to a vector,
matrix, or higher dimensional array if possible? The default value, \code{TRUE},
returns a vector or matrix if appropriate, whereas if \code{simplify = "array"}
the result may be an \code{\link[base]{array}} of higher dimension.
  }
  \item{USE.NAMES}{
logical; if \code{TRUE} and one of \code{X} is character and of equal length to
the result, use that element of \code{X} as \code{\link[base]{names}} for the
result unless it had names already.
  }
  \item{FUN.VALUE}{
a (generalized) vector; a template for the return value from \code{FUN}.
  }
}
\details{
While \code{plapply} would build a call like:

\code{FUN(X[[1L]][[j]], X[[2L]][[j]], ...)}

\code{.plapply} builds a call like:

\code{FUN(X[[1L]][[j]], X[[2L]][[j]], dots[[1L]], dots[[2L]])}

with more or less \code{dots[[k]]} depending on the length of \code{dots}.
Additionally, the \code{dots} arguments in the call will be named if \code{dots}
has names.

\code{dots} is not coerced by \code{base::\link[base:list]{as.list}}, instead
its subsetting (\code{\link[base:Extract]{[[}}), \code{\link[base]{length}},
and \code{\link[base]{names}} methods will be used.
}
\note{
\code{plapply} will only evaluate its \code{\dots} argument as necessary, and
even when forced, it will only evaluate the desired elements.

In contrast, \code{.plapply} will \emph{ALWAYS} evaluate its \code{dots}
argument, even if it is never used. This is because we need to know \code{dots}
length and names to build the call.
}
\seealso{
\code{\link{plapply}}
}
\examples{
# you should see here that plapply will not evaluate its
# optional arguments to FUN (because they are not used in
# this example)
#
# but .plapply will evaluate its optional arguments, even
# though they are not used in this example
invisible(essentials:: plapply(NA, function(...) {
    print(substitute(list(...)))
},      k = cat("evaluated optional arguments to FUN\\n") ))
invisible(essentials::.plapply(NA, function(...) {
    print(substitute(list(...)))
}, list(k = cat("evaluated optional arguments to FUN\\n"))))


# also, plapply will only evaluate optional arguments as
# requested
invisible(essentials:: plapply(NA, function(x, ...) ..1,
         cat("evaluated first optional argument to FUN\\n"),
         cat("evaluated second optional argument to FUN\\n") ))
invisible(essentials::.plapply(NA, function(x, ...) ..1,
    list(cat("evaluated first optional argument to FUN\\n"),
         cat("evaluated second optional argument to FUN\\n"))))
}

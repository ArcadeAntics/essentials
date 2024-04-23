\name{plapply}
\alias{plapply}
\alias{psapply}
\alias{pvapply}
\title{Apply a Function to Multiple List or Vector Arguments}
\description{
  \code{plapply}, \code{psapply}, and \code{pvapply} are multivariate
  (\strong{p}arallel) versions of \code{\link[base]{lapply}},
  \code{\link[base:lapply]{sapply}}, and \code{\link[base:lapply]{vapply}}.
  They take any number of vectors as arguments, recycle them to common length,
  and return a single vector.
}
\usage{
plapply(X, FUN, ...)

psapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)

pvapply(X, FUN, FUN.VALUE, ..., USE.NAMES = TRUE)
}
\arguments{
  \item{X}{list of arguments to vectorize over.}

  \item{FUN}{function to apply, found via \code{\link[base]{match.fun}}.}

  \item{\dots}{optional arguments to \code{FUN}.}

  \item{simplify}{logical or character string; should the result be simplified
    to a vector, matrix, or higher dimensional array if possible? The default
    value, \code{TRUE}, returns a vector or matrix if appropriate, whereas if
    \code{simplify = "array"} the result may be an \code{\link[base]{array}} of
    higher dimension.}

  \item{USE.NAMES}{logical; if \code{TRUE} and one of \code{X} is character and
    of equal length to the result, use that element of \code{X} as
    \code{\link[base]{names}} for the result unless it had names already.}

  \item{FUN.VALUE}{a (generalized) vector; a template for the return value from
    \code{FUN}.}
}
\details{
  In \code{plapply}, after the arguments have been recycled to common length, a
  list is made with length equal to the common length. The value returned in
  the \code{j}-th position of the list is the function \code{FUN} applied to
  the \code{j}-th elements of \code{X}, that is:

  \code{value[[j]] <- FUN(X[[1L]][[j]], X[[2L]][[j]], ...)}

  with more or less \code{X[[i]][[j]]} depending on the length of \code{X}.
  Additionally, the arguments in the call will be named if \code{X} has names.

  Unlike \code{\link[base]{lapply}}, \code{X} and its elements will not be
  coerced by \code{base::\link[base:list]{as.list}}. Instead, their subsetting
  (\code{\link[base:Extract]{[[}}), \code{\link[base]{length}},
  \code{\link[base]{lengths}}, and \code{\link[base]{names}} methods will be
  used.

  Simplification in \code{psapply} and \code{pvapply} is the same as
  \code{\link[base:lapply]{sapply}} and \code{\link[base:lapply]{vapply}}.
}
\value{
  For \code{plapply} a list. If any of the input values is a zero-length vector
  the result has length zero. Otherwise, the result has length equal to the
  length of the longest vector. Names are copied from input values where
  possible. Names are only copied from input values whose lengths are equal to
  the length of the result. If any such input values have a
  \code{\link[base]{names}} attribute, the first \code{names} attribute is
  copied to the result.

  For \code{psapply} a list, the same as \code{plapply}. If \code{USE.NAMES}
  and \code{plapply} did not give names to its return value, then names are
  added where possible. If the input values contain a character vector whose
  length is equal to the length of the result, the names of the the result will
  be said character vector. Then the result may be simplified to a matrix / /
  array via \code{\link[base:lapply]{simplify2array}}.

  For \code{vapply} a vector or array of type matching the \code{FUN.VALUE}. If
  \code{length(FUN.VALUE) == 1} a vector of the common length of \code{X} is
  returned, otherwise an array. If \code{FUN.VALUE} is not an
  \code{\link[base]{array}}, the result is a matrix with
  \code{length(FUN.VALUE)} rows and common length of \code{X} columns,
  otherwise an array with \code{\link[base]{dim}}
  \code{c(dim(FUN.VALUE), }<common length of \code{X}>\code{)}

  The (Dim)names of the array value are taken from the \code{FUN.VALUE} if it
  is named, otherwise from the result of the first function call. Column names
  of the matrix or more generally the names of the last dimension of the array
  value or names of the vector value are set from \code{X} as in
  \code{psapply}.
}
\note{
  \code{plapply} is based on \code{\link[base]{mapply}}. The important
  differences are:

  \describe{
    \item{Argument \code{FUN}}{\code{plapply} has \code{FUN} as the second
      argument (same as \code{lapply}) while \code{mapply} has \code{FUN} as
      the first argument.}

    \item{Arguments to Vectorize Over}{\code{plapply} has \code{X} as the first
      argument which is a collection of arguments to vectorize over (same as
      \code{lapply}, aside from the multivariate aspect) while \code{mapply}
      has \code{\dots} as the collection of arguments to vectorize over. This
      means you'll need to use \code{\link[base]{do.call}} or similar if your
      collection of arguments to vectorize over are already stored in a list,
      pretty inconvenient.}

    \item{Optional / / Other / / Further Arguments to \code{FUN}}{
      \code{plapply} has \code{\dots} immediately after argument \code{FUN} as
      the additional arguments to provide to \code{FUN} each time it is called
      (same as \code{lapply}) while \code{mapply} has \code{MoreArgs} as the
      list of additional arguments to provide to \code{FUN}. If your additional
      arguments are already stored in a list, this makes \code{plapply}
      inconvenient to use. However, instead of abandoning \code{plapply} in
      favour of \code{mapply}, consider using \code{\link{.plapply}}, an
      alternate version that accepts an argument \code{dots} instead of
      \code{\dots}.}

    \item{More on Additional Arguments to \code{FUN}}{When using \code{mapply},
      if \code{MoreArgs} contains a symbol or call, it will not be properly
      protected from early evaluation (despite begging R-Core to change it and
      even offering a patch :/). I consider this problem big enough that I
      would never use \code{mapply}. It is rare that you'd run into such a
      scenario, but when you do, it's incredibly hard to debug, and even more
      annoying to fix while still using \code{mapply}. \code{plapply} and
      \code{.plapply} have no such issue when \code{\dots} or \code{dots}
      contains a symbol or call, they will never be evaluated unnecessarily.}
  }
}
\seealso{
  \code{\link{.plapply}}
}
\examples{
plapply(list(
    col = c("red", "green", "blue"),
    cex = c(1, 1.5, 2),
    main = c("title 1", "title 2", "title 3")
), graphics::plot, x = 1:5, pch = 16)
}
% x <- list(1:3, 6:11, 7, k = expression(a, b + c))
% fun1 <- function(...) as.null(print(substitute(list(...))))
% fun2 <- function(...) as.null(print(sys.call()))
% fun3 <- function(...) as.null(print(list(...)))
% invisible(essentials:: plapply(FUN = fun1, x,      n = 5 ))
% invisible(             .mapply(FUN = fun1, x, list(n = 5)))
% invisible(essentials::.plapply(FUN = fun1, x, list(n = 5)))
% invisible(essentials::plapply(FUN = fun2, x,      n = 5 ))
% invisible(            .mapply(FUN = fun2, x, list(n = 5)))
% invisible(essentials::.plapply(FUN = fun1, x, list(n = 5)))
% invisible(essentials::plapply(FUN = fun3, x,      n = 5 ))
% invisible(            .mapply(FUN = fun3, x, list(n = 5)))
% invisible(essentials::.plapply(FUN = fun1, x, list(n = 5)))
%
%
% X <- list(quote(a))
% dots <- list(quote(b + c))
% invisible(forceAndCall(2L, fun1, X[[1L]], dots[[1L]]))
% invisible(forceAndCall(2L, fun2, X[[1L]], dots[[1L]]))
% invisible(forceAndCall(2L, fun3, X[[1L]], dots[[1L]]))

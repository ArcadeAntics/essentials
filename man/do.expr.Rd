\name{do.expr}
\alias{do.expr}
\title{Python-Style Argument Unpacking}
\description{
  Evaluate a \code{\link[base]{call}} with Python-like argument unpacking, with
  or without names.
}
\usage{
do.expr(expr, envir = parent.frame(),
              enclos = if (is.list(envir) || is.pairlist(envir))
                           parent.frame() else baseenv())
}
\arguments{
  \item{expr}{the (unevaluated) expression to be evaluated after unpacking its
    arguments.}

  \item{envir, enclos}{See \code{\link[base]{eval}}; used to construct the
    environment in which the expression will be evaluated.}
}
\details{
  In Python, an individual argument to a function may be unpacked into many
  arguments. The syntax looks something like:

  \verb{fun(*iter)}

  where \code{iter} would be some type of iterable, normally a tuple or list.
  This is parsed to:

  \verb{fun(iter[0], iter[1], ..., iter[-2], iter[-1])}

  Python has a similar syntax for unpacking named arguments:

  \verb{fun(**dict)}

  where \code{dict} is a dictionary. This is parsed to:

  \verb{fun(dict.keys[0] = dict.values[0], dict.keys[1] = dict.values[1], ...,
    dict.keys[-2] = dict.values[-2], dict.keys[-1] = dict.values[-1])}

  The syntax with \code{do.expr} is much the same:

  \verb{fun(`*`(list))}

  is parsed to:

  \verb{fun(list[[1]], list[[2]], ...)}

  and:

  \verb{fun(`**`(list))}

  is parsed to:

  \verb{fun(names(list)[[1]] = list[[1]], names(list)[[2]] = list[[2]], ...)}

  The object being unpacked does not have to be a list: any object with
  subsetting (\code{\link[base:Extract]{[[}}), \code{\link[base]{length}}, and
  \code{\link[base]{names}} methods will suffice.
}
\value{
  The result of evaluating \code{expr} after unpacking its arguments.
}
\examples{
x <- structure(1:5, names = letters[1:5])

do.expr( list(k = 1:4, `*`(x) ) )  # without names
do.expr( list(k = 1:4, `**`(x)) )  # with names

## use argument unpacking to select from multi-dimensional array
## (with unknown number of dimensions)

ndim <- sample(3:4, size = 1)
dim <- sample(3:4, size = ndim, replace = TRUE)
x <- array(seq_len(prod(dim)), dim)

## select rows whose sums are less than the mean row sum
sums <- rowSums(x)
i <- sums < mean(sums)
missing_indexes <- rep(list(quote(expr = )), length(dim(x)) - 1)
do.expr( x[i, `*`(missing_indexes)] )

## easy to add more arguments, such as 'drop'
do.expr( x[i, `*`(missing_indexes), drop = FALSE] )
}

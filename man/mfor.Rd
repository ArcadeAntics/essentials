\name{mfor}
\alias{mfor}
\title{
Multivariate For Loop
}
\description{
\code{mfor} is a multivariate version of \code{\link[base:Control]{for}}.
}
\usage{
mfor(...)
}
\arguments{
  \item{\dots}{
arguments to \code{mfor}. The last argument (called \code{expr}) is the
\emph{expression} of the \code{mfor} loop (expression in a formal sense). This
is either a simple expression or a so-called \emph{compound expression},
usually of the form \code{{ expr1 ; expr2 }}. The second last argument (called
\code{seqs}) is the object being iterated over (a collection of sequences).
Sequences are recycled as necessary. All other arguments (called \code{vars})
are syntactical names for variables.
  }
}
\details{
If exactly one variable is specified, the behaviour is similar to
\code{\link[base:Control]{for}}. See section \strong{Examples}.

\code{\link[base:Control]{break}} breaks out of an \code{mfor} loop,
\code{\link[base:Control]{next}} halts the processing of the current iteration
and advances the looping index; exactly the same as a
\code{\link[base:Control]{for}}, \code{\link[base:Control]{while}}, or
\code{\link[base:Control]{repeat}} loop.

The \code{seqs} in an \code{mfor} loop is evaluated at the start of the loop;
changing it subsequently does affect the loop. If any sequence in \code{seqs}
has length zero, the body of the loop is skipped. Otherwise, the variables
\code{vars} are assigned in turn the elements of the sequences of \code{seqs}.
You can assign to any of \code{vars} within the body of the loop, but this will
not affect the next iteration. When the loop terminates, \code{vars} remains as
the variables containing their last values.

Unlike \code{\link[base:Control]{for}}, the sequences will not be coerced to a
vector or to a \code{\link[base:list]{pairlist}}. Instead, their subsetting
(\code{\link[base:Extract]{[[}}), \code{\link[base]{length}}, and
\code{\link[base]{lengths}} methods will be used.
}
\value{
\code{mfor} returns \code{NULL} invisibly. It also sets \code{vars} to the last
used values of \code{seqs}, or to \code{NULL} if any sequence was length zero.
}
\seealso{
\code{\link[base:Control]{for}} for the single variate for loop.
}
\examples{
# when exactly one variable is specified,
# the behaviour is similar to 'for'
mfor(i, 1:5, print(1:i))


# 'mfor' works on classed objects, 'for' does not
mfor(date, Sys.time() + 0:9, print(date))


# sequences are recycled as necessary,
# with a warning for fractional recycling
mfor(i, j, k, list(1:4, 6:10, 11:15), {
    print(c(i = i, j = j, k = k))
})


# mfor works well with data frames
mfor(          col              , cex, main               ,
    data.frame(palette.colors(3), 1:3, paste("title", 1:3)),
    graphics::plot(x = 1:5, col = col, cex = cex, main = main,
        pch = 16)
)
}

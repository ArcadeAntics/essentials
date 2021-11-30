\name{legend.dimensions}
\alias{legend.dimensions}
\title{
Size of a Legend
}
\description{
This function is used to determine the size of a legend as it appears on a plot.
}
\usage{
legend.dimensions(expr, envir = parent.frame(),
                        enclos = if (is.list(envir) || is.pairlist(envir))
                                     parent.frame() else baseenv(),
    trace = FALSE)
}
\arguments{
  \item{expr}{
a \code{\link[base]{call}}, an \code{\link[base]{expression}} of length 1, or a
\code{\link[stats]{formula}} of length 2.
  }
  \item{envir}{
the \code{\link[base]{environment}} in which \code{expr} is to be evaluated. May
also be \code{NULL}, a list, a data frame, a pairlist or an integer as specified
to \code{\link[base:sys.parent]{sys.call}}.
  }
  \item{enclos}{
Relevant when \code{envir} is a (pair)list or a data frame. Specifies the
enclosure, i.e., where \R looks for objects not found in \code{envir}. This can
be \code{NULL} (interpreted as the base package environment,
\code{\link[base:environment]{baseenv()}}) or an environment.
  }
  \item{trace}{
Should \code{legend.dimensions} print debugging messages?
  }
}
\details{
\code{expr} would usually be a call to
\code{\link[graphics:legend]{graphics::legend}}, though anything that adds a
legend to a plot and returns identical output to \code{graphics::legend} is
accepted.

If \code{expr} is a formula, it should be of the form \code{~call( .. )} where
\code{call( .. )} is the call to be evaluated (the call that produces the legend
when evaluated). \code{envir} will be ignored when \code{expr} has an
environment, and \code{enclos} will always be ignored.
}
\value{
A list with components
  \describe{
    \item{\code{w,h}}{
positive numbers giving width and height of the legend's box \emph{IN INCHES!}
    }
  }
}
\examples{\dontshow{omar <- graphics::par("mar")}
# the expression that produces the desired legend
expr <- ~graphics::legend(


    # the top-left corner of the legend will appear in the
    # top-right corner of the plot
    x = essentials::fix.xlog(graphics::par("usr")[2L]),
    y = essentials::fix.ylog(graphics::par("usr")[4L]),


    legend = letters[1:5], fill = 1:5,
    xpd = TRUE
)


# we'll start by drawing a plot with a legend without
# adjusting the margins. the margins will likely look too
# small or too large
graphics::plot(1:5) ; eval(expr[[2L]])






# now, we'll adjust the margins using 'legend.dimensions'
# capture the dimensions of the resultant legend
ld <- legend.dimensions(expr)


adj.margins(ld)
graphics::plot(1:5) ; add.legend(expr)





# now, we'll adjust the legend such that it is centered
# vertically
location(expr) <- location(ld, adj = 0.5)
graphics::plot(1:5) ; add.legend(expr)





# you do not need to use 'legend.dimensions' explicitly, you
# could supply 'expr' directly to 'adj.margins' and
# 'location'
adj.margins(expr)
location(expr) <- location(expr, adj = 0.75)
graphics::plot(1:5) ; add.legend(expr)
\dontshow{graphics::par(mar = omar)}}

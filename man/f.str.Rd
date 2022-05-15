\name{f.str}
\alias{f.str}
\title{
Use C-Style String Formatting Commands with Interpolation
}
\description{
A variant of \code{\link[base]{sprintf}} where values are specified
\emph{within} the format strings.
}
\usage{
f.str(x, envir = parent.frame(),
         enclos = if (is.list(envir) || is.pairlist(envir))
                      parent.frame() else baseenv(),
      simplify = TRUE)
}
\arguments{
  \item{x}{
a character vector of format strings.
  }
  \item{envir}{
the \code{\link[base]{environment}} in which the values are to be evaluated.
May also be \code{NULL}, a list, a data frame, a pairlist, or an integer as
specified to \code{\link[base:sys.parent]{sys.call}}.
  }
  \item{enclos}{
Relevant when \code{envir} is a (pair)list or a data frame. Specifies the
enclosure, i.e., where \R looks for objects not found in \code{envir}. This
can be \code{NULL} (interpreted as the base package environment,
\code{\link[base:environment]{baseenv}()}) or an environment.
  }
  \item{simplify}{
should the result be simplified to a character vector if possible?
  }
}
\details{
Values in the format strings are specified in a similar manner to that of 
\code{\link[base:Quotes]{raw character constants}}: \verb{(...)} with
\verb{...} any character sequence, except that it must not contain the closing
delimiter \sQuote{\verb{)}}. The delimiter pairs \verb{[]} and \verb{{}} can
also be used. For additional flexibility, a number of dashes can be placed
immediately before the opening delimiter, as long as the same number of dashes
appear immediately after the closing delimiter.

A field width or precision (but not both) may be indicated by an asterisk
\verb{*}: see \strong{Examples} for two methods of specifying such an argument.

These values should appear immediately after the \verb{\%} in the conversion
specifications, and immediately before the formatting specifications and type.
}
\value{
For \code{f.str(simplify = FALSE)}, a list of the same length as \code{x}.

For \code{f.str(simplify = TRUE)}:
if \code{x} has length zero, \code{character(0)}.
\verb{
}Otherwise, if \code{x} is length one, the first element of the list.
\verb{
}Otherwise, if each element of the list is length one, the list is unlisted.
\verb{
}Otherwise, the list is returned as is.
}
\seealso{
\code{\link[base]{sprintf}}

\code{glue::glue} for another method of string interpolation.
}
\examples{
name <- "Fred"
age <- 50
anniversary <- as.Date("1991-10-12")

## all 3 delimiter pairs:
f.str(x <- "My name is \%(name)s, my age next year is \%[age + 1]i, my anniversary is \%{format(anniversary, '\%A, \%B \%d, \%Y')}s.")

## evaluate in a different environment:
f.str(x, list(name = "Joe", age = 40, anniversary = as.Date("2001-10-12")))

## use a literal \% :

f.str("\%{sum(responses == 'yes')/length(responses) * 100}.0f\%\% said yes (out of a sample of size \%{length(responses)}.0f)",
    list(responses = c("yes", "no", "yes")))%f.str("\%(66.666).0f\%\% said yes (out of a sample of size \%(3).0f)", list(responses = c("yes", "no", "yes")))

## re-use one argument three times, show difference between \%x and \%X
xx <- f.str("\%(0:15)d \%1$x \%1$X")
xx <- matrix(xx, dimnames = list(rep("", 16), "\%d\%x\%X"))
noquote(format(xx, justify = "right"))

## Platform-dependent bad example from qdapTools 1.0.0:
## may pad with spaces or zeroes.
f.str("\%(month.name)09s")

n <- 1:18
f.str(paste0("e with \%(", n, ")2d digits = \%[exp(1)].", n, "g"))

## Using asterisk for width or precision
f.str("precision \%(3;pi).*f, width '\%(8;pi)*.3f'")

## Asterisk and argument re-use, 'e' example reiterated:
f.str("e with \%(n)2d digits = \%[exp(1)].*1$g")

## re-cycle arguments
f.str("\%(letters)s \%(1:13)d")

## binary output showing rounding/representation errors
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, f.str("\%(x)a"), f.str("\%(y)a"))
}

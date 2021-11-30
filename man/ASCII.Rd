\name{ASCII}
\alias{ASCII}
\title{
ASCII Characters
}
\description{
\code{ASCII} generates a character vector of the ASCII characters. If
\code{plot = TRUE}, the resulting characters are plotted, before they are
returned invisibly.
}
\usage{
ASCII(extended = TRUE, cex = par("cex"), family = par("family"),
    mar = c(0, 2.1, 2.1, 0), plot = TRUE, warn.unused = TRUE)
}
\arguments{
  \item{extended}{
logical. Should the extended ASCII character set be returned? Note that
\dQuote{Extended ASCII} does \emph{NOT} mean that the ASCII standard has been
updated to include more than 128 characters.
  }
  \item{cex}{
A numerical value giving the amount by which text should be magnified relative
to the default.
  }
  \item{family}{
The name of a font family for drawing text. See \code{\link[graphics]{par}}.
  }
  \item{mar}{
A numerical vector of the form \code{c(bottom, left, top, right)} which gives
the number of lines of margin to be specified on the four sides of the plot.
  }
  \item{plot}{
logical. If \code{TRUE} (default), the ASCII characters are plotted, and the
ASCII characters are returned invisibly. Otherwise, the ASCII characters are
returned.
  }
  \item{warn.unused}{
logical. If \code{plot = FALSE} and \code{warn.unused = TRUE}, a warning will be
issued when graphical parameters are passed to \code{ASCII}.
  }
}
\details{
The first 32 ASCII characters are control characters (as well as the 128-th,
that is \code{"\\x7F"} or \code{"\\177"}), consisting of non-printable
characters and whitespace characters, so they will likely plot as an empty box
or nothing at all.

Characters \code{"\\x81"}, \code{"\\x8D"}, \code{"\\x8F"}, \code{"\\x90"}, and
\code{"\\x9D"} (or in octal notation as \code{"\\201"}, \code{"\\215"},
\code{"\\217"}, \code{"\\220"}, and \code{"\\235"}) are unused in extended
ASCII, and will plot unusually. In an ISO8859-1 locale, they seem to plot empty.
In a UTF-8 locale, they seem to plot as their bytes codes (that is \code{<81>},
\code{<8D>}, \code{<8F>}, \code{<90>}, and \code{<9D>}). In other locales, they
may plot as question marks, or any other unusual behaviour.

Characters \code{"\\x20"} and \code{"\\xA0"} are space and non-breaking space,
so they will plot empty.

Character \code{"\\xAD"} is a soft hyphen. In an ISO8859-1 locale, it appears
the same as a regular hyphen (\code{"\\x2D"}). In a UTF-8 locale, it appears
empty. Unknown behvaiour for other locales. This character may also plot
differently depending on \code{family}.
}
\value{
character vector, the ASCII character set. When \code{extended}, 255 characters,
otherwise 127 characters (NUL character is not included since R does not allow
nul character within strings).

if \code{plot}, returned invisibly.
}
\examples{
ASCII()
}

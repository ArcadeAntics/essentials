\name{shEncode}
\alias{shEncode}
\alias{commandEncode}
\alias{commandQuote}
\title{
Quote Strings for Use in OS Shells
}
\description{
Quote a string to be passed to an operating system shell.
}
\usage{
     shEncode(string, type = NULL, unix.type = NULL, windows.type = "Rscript")

commandEncode(string, type = NULL, unix.type = NULL, windows.type = "Rscript")

 commandQuote(string, type = NULL, unix.type = NULL, windows.type = "Rscript")
}
\arguments{
  \item{string}{
a character vector, usually of length one.
  }
  \item{type}{
character string: the type of quoting. Partial matching is supported.
  }
  \item{unix.type}{
character string: the type of quoting under Unix-alikes when \code{type} is
\code{NULL}.
  }
  \item{windows.type}{
character string: the type of quoting in Windows when \code{type} is
\code{NULL}.
  }
}
\details{
The choices for quoting are \code{"sh"}, \code{"perl"}, \code{"python"},
\code{"R"}, \code{"R CMD"}, \code{"Rcmd"}, \code{"Rgui"}, \code{"Rscript"}, and
\code{"Rterm"}. The default for quoting under Unix-alikes is \code{"sh"}, while
in Windows it is \code{"Rscript"}.

From my testing with \code{\link[base]{shQuote}}, it seems like it fails in a
few edge cases that \code{shEncode} can handle correctly.

Those few cases that \code{shQuote} handles incorrectly are:
\itemize{
  \item{under Unix-alikes}{
with \code{type = "csh"} and \code{string} contains a dollar sign or grave
accent and \code{string} has a trailing single quote, \code{string} loses its
trailing single quote.
  }
  \item{in Windows}{
with \code{type = "cmd"} and \code{string} contains a double quote preceded by
at least one backslash, the preceding backslashes aren't escaped. Also, when
\code{string} contains a set of trailing backslashes, they aren't escaped.
  }
}
}
\value{
character vector, the same length as \code{string}, with the attributes of
\code{string} (after possible coercion to character), excluding \code{class}.
}
\seealso{
\code{\link{Rscript}}, \code{\link{python}}
}
\examples{
fun <- function(string) {
    cat(c(
        "string  ", string                                        , "\\n",
        "sh      ", essentials::shEncode(string, type = "sh"     ), "\\n",
        "Rscript ", essentials::shEncode(string, type = "Rscript"), "\\n",
        "R       ", essentials::shEncode(string, type = "R"      ), "\\n"
    ), sep = "")
}

fun("abc$def`gh`i\\\\j")

fun("testing \\\\\\"this\\\\\\"")

fun("\\"testing\\" $this$ 'out'")


\dontrun{
essentials:::.system(paste(c(
    "perl",
    "-e",
    essentials::shEncode(r"{print "test \\"this\\" out\\n";}",
        windows.type = "perl")
), collapse = " "))
}


\dontrun{
essentials:::.system(paste(c(
    "python",
    "-c",
    essentials::shEncode(r"{print("test \\"this\\" out")}",
        windows.type = "python")
), collapse = " "))
}
}

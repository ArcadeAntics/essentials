\name{asWindowsbasename}
\alias{asWindowsbasename}
\title{
Create a Basename Valid in Windows
}
\description{
Windows basenames must not contain the control characters \code{"\\001"} through
\code{"\\037"}, double quote \code{\"}, asterisk \code{*}, slash \code{/}, colon
\code{:}, left and right chevrons \code{<>}, question mark \code{?}, backslash
\code{\\}, and vertical bar \code{|}. Additionally, a Windows basename must not
begin with a space, and must not end with a space or full stop. A basename which
is valid in Windows is likely to be valid on any OS.
}
\usage{
asWindowsbasename(path)
}
\arguments{
  \item{path}{
character vector; the strings to make into valid paths.
  }
}
\value{
A character vector, the same length and attributes as \code{path} (after
possible coercion to character).
}
\examples{
asWindowsbasename(c(
    "  test  ",
    "  testing?.",
    "already valid name"
))
}

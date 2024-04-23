\name{shPrompt}
\alias{shPrompt}
\title{Replicate Terminal Prompts Seen in a Few Common Shells}
\description{
  Get the prompt seen at Windows cmd, Windows Powershell, Bash, and macOS Bash
  (bash 3.0). The function will return whichever is most appropriate, unless
  over-ridden by its argument
}
\usage{
shPrompt(type = NULL)
}
\arguments{
  \item{type}{\code{NULL} or a character string; the shell prompt to return.}
}
\details{
  The choices of \code{type} are \code{"windows"}, \code{"cmd"},
  \code{"powershell"}, \code{"macOS"}, \code{"bash"}, \code{"ubuntu"}, and
  \code{"unix"}, with case insensitive partial matching.

  If \code{type} is a character string, it is matched against the above
  choices. If \code{type} is \code{NULL} or does not match one of the above
  choices, \code{type} is pulled from option
  \option{essentials::shPrompt(type)}. If this option is \code{NULL} or does
  not match one of the above choices, \code{type} is pulled from environment
  variable \env{R_ESSENTIALS_SH_PROMPT_TYPE}. If this environment variable is
  unset or does not match one of the above choices, \code{type} is determined
  by \code{\link{.Platform}$OS.type} and
  \code{\link[base]{capabilities}("aqua")}.
}
\value{
  character string.
}
\examples{
cat(
    essentials::shPrompt("cmd"       ),
    essentials::shPrompt("powershell"),
    essentials::shPrompt("macOS"     ),
    essentials::shPrompt("bash"      ),
    sep = "\\n"
)
}

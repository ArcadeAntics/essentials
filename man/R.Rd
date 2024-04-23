\name{R}
\alias{R}
\alias{Rcmd}
\alias{Rscript}
\alias{Rterm}
\title{Start a New R Session}
\description{
  Start a new \R session with the specified options.
}
\usage{
R(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir, evaluated,
    simplify.brace = TRUE)

Rcmd(options = NULL, command = "", args = NULL, ..., name = windows.type,
    dir)

Rscript(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir, evaluated,
    simplify.brace = TRUE)

Rterm(options = NULL, file = NULL, exprs = NULL, args = NULL,
    chdir = FALSE, ..., name = windows.type, dir, evaluated,
    simplify.brace = TRUE)
}
\arguments{
  \item{options}{character vector. The options to be provided for the new \R
    session. For \code{Rcmd} on Windows, \code{options} will not do anything.}

  \item{file}{character string. The filename of the \R script to run in the new
    \R session, provided to option \option{--file=}.}

  \item{exprs}{character vector. The expressions to be run in the new \R
    session, provided to option \option{-e}.}

  \item{args}{any \R object. The command-line arguments to be provided
    \emph{after} \option{--args}, or for \code{Rcmd}, arguments provided after
    \code{command}. Converted to character in a similar manner to
    \code{\link{withArgs}}.}

  \item{chdir}{logical; if \code{TRUE} and \code{file} is a pathname, the \R
    working directory is temporarily changed to the directory containing
    \code{file} for evaluating.}

  \item{\dots}{further arguments passed to \code{\link[base]{system}()}.}

  \item{name}{the name or path of the \R executable to use. By default, they
    will use the \R executable found on \env{Path}. Specifying \code{name} can
    allow you to select a different version of \R to run, or select a version
    of \R tied to another application.}

  \item{dir}{character string; directory in which the \R executables are
    located.}

  \item{evaluated, simplify.brace}{Should \code{exprs} be evaluated or
    \code{\link{substitute}d}? Should \code{exprs} then be simplified if it is
    wrapped with braces?}

  \item{command}{character string. Naming one of the commands to execute from
    \command{R CMD} usage message.}
}
\details{
  \sQuote{Rterm} is an executable only available on Windows, but it is
  preferable because it has more intuitive quoting rules than \sQuote{R} (see
  \code{\link{commandQuote}}). If \code{Rterm} is called on a Unix-alike OS,
  \code{R} is called instead.
}
\value{
  The value of \code{\link[base]{system}()}. If \code{intern = FALSE}, it is
  returned invisibly.
}
\examples{
# if you're on Windows, you should notice that the quoting rules for 'Rterm' are
# far more intuituve than for 'R'
#
# if you're under a Unix-alike, you should notice that 'Rterm' and 'R' do the
# same thing


essentials::R      (exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rterm  (exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}", quiet = TRUE)


essentials::Rcmd(command = "INSTALL", args = "--help")
}

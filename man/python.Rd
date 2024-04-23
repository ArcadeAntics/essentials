\name{python}
\alias{python}
\title{Start a New Python Session}
\description{
  Start a new Python session with the specified options.

  Ensure you have \href{https://www.python.org/downloads/}{python} installed.
}
\usage{
python(options = NULL, command = NULL, module = NULL, file = NULL,
    args = NULL, chdir = FALSE, ..., name = windows.type, dir)
}
\arguments{
  \item{options}{character vector. The options to be provided for the new
    Python session.}

  \item{command}{character vector. The expressions to be run in the new Python
    session, provided to option \option{-c cmd}.}

  \item{module}{character string. The library module to run as a script,
    provided to option \option{-m mod}.}

  \item{file}{character string. The filename of the Python script to run in the
    new Python session, provided to option \option{file}.}

  \item{args}{Converted to character in a similar manner to
    \code{\link{withArgs}}.}

  \item{chdir}{logical; if \code{TRUE} and \code{file} is a pathname, the \R
    working directory is temporarily changed to the directory containing
    \code{file} for evaluating.}

  \item{\dots}{further arguments passed to \code{\link[base]{system}()}.}

  \item{name}{the name or path of the Python executable to use. By default, it
    will use the Python executable found on \env{Path}. Specifying \code{name}
    can allow you to select a different version of Python to run, or select a
    version of Python tied to another application (for example: ArcGIS comes
    with a Python executable and the ArcPy module).}

  \item{dir}{character string; directory in which the Python executable is
    located.}
}
\value{
  The value of \code{\link[base]{system}()}. If \code{intern = FALSE}, it is
  returned invisibly.
}
\examples{
\dontrun{
essentials::python(command = essentials::dedent(r"{
    print("Hello World!")
    print("An example python script...")
}"))
}
}

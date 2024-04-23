\name{envvars}
\alias{envvar}
\alias{envvars}
\alias{getEnvvar}
\title{Environment Variables}
\description{
  Get, set, or remove environment variables.
}
\usage{
envvars(...)

getEnvvar(x, default = NULL)
}
\arguments{
  \item{\dots}{environment variables to get, set, or remove. See
    \strong{Examples}.}

  \item{x}{character string holding an environment variable name.}

  \item{default}{if the specified environment variable is not set, this value
    is evaluated and returned. This facilitates retrieving an environment
    variable and checking whether it is set and setting it separately if not.}
}
\details{
  \code{envvars} provides a mechanism for getting, setting, and removing
  environment variables in a way more in line with \code{\link[base]{options}}.

  Invoking \code{envvars()} with no arguments returns a list with the current
  values of the environment variables. To access the value of a single
  environment variable, one should use, e.g., \code{getEnvvar("HOME")} rather
  than \code{envvars("HOME")} which is a \emph{list} of length one.
}
\value{
  For \code{getEnvvar}, the current value set for environment variable
  \code{x}, or \code{default} if the option is unset.

  For \code{envvars()}, a list of all set environment variables sorted by name.
  For \code{envvars(name)}, a list of length one containing the set value, or
  \code{NA} if it is unset. For uses setting one or more environment variables,
  a list with the previous values of the environment variables changed
  (returned invisibly).
}
\seealso{
  \code{\link[base:EnvVar]{Environment Variables}}
}
%\examples{
%# this example runs incredibly weird using 'example', best
%# to copy and paste this text within the R console and run
%# it from there
%\dontrun{
\examples{
oenv <- envvars(); utils::str(oenv)  # oenv is a named list

getEnvvar("PATH") == envvars()$PATH  # the latter is slower, needs more memory

# change the language, and save the previous value
old.env <- envvars(LANGUAGE = "nn")
old.env              # previous value
envvars("LANGUAGE")  # current value


# restore LANGUAGE back to its previous value,
# or remove it if it previously did not exist
envvars(old.env)


envvars(oenv)  # reset (all) initial environment variables
envvars("LANGAUGE")
}%}

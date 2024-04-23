\name{Args}
\alias{Args}
\title{Extract Arguments From an Object of Class "ParsedArgs"}
\description{
  Provides access to a copy of the arguments supplied to an object of class
  "ParsedArgs".
}
\usage{
Args(x, type = c("original", "all", "trailingOnly"))
}
\arguments{
  \item{x}{an object of class "ParsedArgs".}

  \item{type}{a string naming the type of arguments to extract from \code{x}.}
}
\details{
  \code{Args} extracts the arguments provided to an object of class
  "ParsedArgs". The types have the following meaning:

  \describe{
    \item{"original"}{original arguments supplied when \code{x} was created}

    \item{"all"}{arguments after resolving all from-file arguments}

    \item{"trailingOnly"}{arguments supplied after the \code{"skip"} argument}
  }
}
\value{
  character vector containing the arguments supplied when \code{x} was created.
}

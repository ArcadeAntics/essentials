\name{Commands}
\alias{Commands}
\title{Extract Commands From an Object of Class "ParsedArgs"}
\description{
  Provides access to a copy of the commands supplied to an object of class
  "ParsedArgs".
}
\usage{
Commands(x, type = c("original", "string"))
}
\arguments{
  \item{x}{an object of class "ParsedArgs".}

  \item{type}{a string naming the type to return.}
}
\value{
  If \code{type = "original"}, a character vector of the commands supplied when
  \code{x} was created.

  If \code{type = "string"}, a character string of the commands supplied when
  \code{x} was created, separated by \code{"/"}.
}
\examples{
parser <- essentials::ArgumentParser()
`parser CMD1` <- parser$add.parser("CMD1")
`parser CMD2` <- parser$add.parser("CMD2")
`parser CMD1 a` <- `parser CMD1`$add.parser(c("a", "b"))
`parser CMD1 c` <- `parser CMD1`$add.parser(c("c", "d"))
`parser CMD2 e` <- `parser CMD2`$add.parser(c("e", "f"))
`parser CMD2 g` <- `parser CMD2`$add.parser(c("g", "h"))


essentials::Commands(parser$parse.args())
essentials::Commands(parser$parse.args(c("CMD1")))
essentials::Commands(parser$parse.args(c("CMD2")))
essentials::Commands(parser$parse.args(c("CMD1", "a")))
essentials::Commands(parser$parse.args(c("CMD1", "b")))
essentials::Commands(parser$parse.args(c("CMD1", "c")))
essentials::Commands(parser$parse.args(c("CMD1", "d")))
essentials::Commands(parser$parse.args(c("CMD2", "e")))
essentials::Commands(parser$parse.args(c("CMD2", "f")))
essentials::Commands(parser$parse.args(c("CMD2", "g")))
essentials::Commands(parser$parse.args(c("CMD2", "h")))
}

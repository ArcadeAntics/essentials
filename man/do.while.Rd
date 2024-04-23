\name{do.while}
\alias{do.while}
\alias{do.until}
\alias{\%while\%}
\alias{\%until\%}
\title{Do While/Until Loops in R}
\description{
  Allows for the do while/until loop syntax seen in other languages.
}
\usage{
expr \%while\% cond
expr \%until\% cond
}
\arguments{
  \item{expr}{An \emph{expression} in a formal sense. This is either a simple
    expression or a so-called \emph{compound expression}, usually of the form
    \code{{ expr1 ; expr2 }}. \strong{MUST} be wrapped with \code{do ( .. )}.}

  \item{cond}{A length-one logical vector that is not \code{NA}. Other types
    are coerced to logical if possible, ignoring any class. \strong{MUST} be
    wrapped with parenthesis.}
}
\details{
  First, the code block \code{expr} is evaluated, and then the condition
  \code{cond} is evaluated. This repeats while/until the condition is
  \code{TRUE}. This contrasts from a \code{\link[base:Control]{while}} loop
  where the condition is evaluated before the code block.

  \code{\link[base:Control]{break}} breaks out of a do while/until loop,
  \code{\link[base:Control]{next}} halts the processing of the current
  iteration and advances the looping index; exactly the same as a
  \code{\link[base:Control]{for}}, \code{\link[base:Control]{while}}, or
  \code{\link[base:Control]{repeat}} loop.
}
\value{
  \code{NULL} invisibly.
}
\seealso{
  \code{\link[base:Control]{while}}

  \href{https://en.wikipedia.org/wiki/Do_while_loop}{Do while loop}
}
\examples{
# Suppose you want a unique name for a temporary file (we'll ignore that
# `tempfile` exists for now). We can use a do while loop to create a new
# random name until a unique name is found.
do ({
    value <- sprintf("\%x", sample(1000000000, 1))
    print(value)
}) \%while\% (file.exists(value))


## note that the following is equivalent:
# do ({
#     value <- sprintf("\%x", sample(1000000000, 1))
#     print(value)
# }) \%until\% (!file.exists(value))


# Suppose you want a random number that is greater than one million (we'll ignore
# that `stats::runif` exists for now).
do ({
    value <- sample(1.01e+06, 1)
    print(value)
}) \%until\% (value > 1e+06)


## note that the following is equivalent:
# do ({
#     value <- sample(1.01e+06, 1)
#     print(value)
# }) \%while\% (value <= 1e+06)


# Finally suppose you wanted to ask the user for input, but wanted to make sure
# it was valid. Here, we'll say the input is valid if it is all numeric
# characters (ignore leading and trailing whitespace).
# Let's put a limit on it too, only ask a certain amount of times
count <- 0L
do ({
    value <- readline("Enter an integer: ")
    value <- gsub("^\\\\s+|\\\\s$", "", value)
    valid <- grepl("^[[:digit:]]+$", value)
    count <- count + 1L
}) \%until\% (count >= 5L || valid)
print(list(value = value, valid = valid, count = count))


## note that the following is equivalent:
# do ({
#     value <- readline("Enter an integer: ")
#     value <- gsub("^\\\\s+|\\\\s$", "", value)
#     valid <- grepl("^[[:digit:]]+$", value)
#     count <- count + 1L
# }) \%while\% (count < 5L && !valid)
}

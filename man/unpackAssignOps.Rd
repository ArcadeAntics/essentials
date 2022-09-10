\name{unpackAssignOps}
\alias{unpackAssignOps}
\alias{\%<-\%}
\alias{\%<<-\%}
\alias{\%->\%}
\alias{\%->>\%}
\alias{\%=\%}
\alias{unpack.assign}
\alias{unpack.super.assign}
\title{
Assignment Operators
}
\description{
Assign a value to a name, or many values to many names.
}
\usage{
x \%<-\% value
x \%<<-\% value
value \%->\% x
value \%->>\% x

x \%=\% value

unpack.assign(x, value, evaluated = TRUE)
unpack.super.assign(x, value, evaluated = TRUE)
}
\arguments{
  \item{x}{
a variable name (possibly quoted), or many variables names.
  }
  \item{value}{
a value to be assigned to \code{x}, or many values to be assigned.
  }
  \item{evaluated}{
should \code{x} be evaluated?
  }
}
\details{
These operators will assign into the same environments as their non-SPECIAL
variants: see \code{\link[base:assignOps]{<-}},
\code{\link[base:assignOps]{<<-}}, \code{\link[base:assignOps]{->}},
\code{\link[base:assignOps]{->>}}, and \code{\link[base:assignOps]{=}}.

\code{x} can be a name or an expression defining a part of an object to be
replaced (e.g., \code{z[[1]]}), or many names or expressions defining parts of
objects to be replaced.

These operators add support for using \code{list} to unpack a vector of
arguments, \code{*} to collect a remainder of arguments, and discarding unwanted
portions of \code{value}, for example:\verb{
list(a, b, c) \%<-\% value}

which does something along the lines of:\verb{
a <- value[[1]]
b <- value[[2]]
c <- value[[3]]}

You can use \code{*} to collect a variable number of arguments, for
example:\verb{
list(a, b, `*`(args), z) \%<-\% value}

which does something along the lines of:\verb{
a <- value[[1]]
b <- value[[2]]
args <- value[3:length(value) - 1]
z <- value[[length(value)]]}

You can assign to an empty argument which will discard that portion of
\code{value}, for example:\verb{
list(a, , z) \%<-\% value}

which does something along the lines of:\verb{
a <- value[[1]]
z <- value[[3]]}

You can combine assigning to an empty argument with \code{*}, for example:\verb{
list(a, `*`(), z) \%<-\% value}

which does something along the lines of:\verb{
a <- value[[1]]
z <- value[[length(value)]]}

You can combine all of these to make some interesting and complicated
expressions, for example:\verb{
list(a, b[[1]], `*`(args), , x, names(y), attr(z, "test")) \%<-\% value}

which does something along the lines of:\verb{
a <- value[[1]]
b[[1]] <- value[[2]]
args <- value[3:length(value) - 4]
x <- value[[length(value) - 2]]
names(y) <- value[[length(value) - 1]]
attr(z, "test") <- value[[length(value)]]}
}
\value{
\code{value}. Thus one can use \code{a \%<-\% b \%<-\% c \%<-\% 6}.
}
\examples{
`\%<-\%` <- essentials::`\%<-\%`


# using 'list' to unpack a vector
list(x, y, z) \%<-\% 1:3
print(list(x = x, y = y, z = z))


# using '*' to collect vectors
list(a, b, `*`(args), y, z) \%<-\% 4:14
print(list(a = a, b = b, args = args, y = y, z = z))


# using a missing argument to discard an element
list(a, , b) \%<-\% 15:17
print(list(a = a, b = b))


# combining '*' with missing argument to discard several elements
list(a, `*`(), z) \%<-\% 18:28
print(list(a = a, z = z))


# taken from section 'Details'
b <- list(NULL)
y <- 1:26
z <- pi
list(a, b[[1]], `*`(args), , x, names(y), attr(z, "test")) \%<-\% c(
    list(1:5),
    list(6:10),
    1:10,
    list(letters),
    "example attr"
)
print(list(a = a, b = b, args = args, x = x, y = y, z = z))


# check if an expression would automatically print
list(value, visible) \%<-\% withVisible(5 + 6)
print(list(value = value, visible = visible))

list(value, visible) \%<-\% withVisible(invisible(letters[1:5]))
print(list(value = value, visible = visible))
}

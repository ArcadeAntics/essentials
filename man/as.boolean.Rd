\name{as.boolean}
\alias{as.boolean}
\title{
Booleans
}
\description{
Coerce objects to type \dQuote{logical} with length 1
}
\usage{
as.boolean(x, `_R_CHECK_LENGTH_1_CONDITION_` = FALSE)
}
\arguments{
  \item{x}{
  object to be coerced.
}
  \item{_R_CHECK_LENGTH_1_CONDITION_}{
  when FALSE, \code{x} of length greater than one is accepted with a warning, but
  only the first element is used. Otherwise, NA is returned.
}
}
\details{
\code{as.boolean} uses \code{\link{if}} to coerce objects to type \dQuote{logical}
with length 1. This means that \code{as.boolean} ignores any class of the object
(it will not respect methods written for \code{\link{as.logical}}).
}
\value{
a logical vector of length 1
}
\examples{
## logical vectors of length 1 return the same
as.boolean(TRUE)
as.boolean(FALSE)
as.boolean(NA)


## integer vectors of length 1 return x != 0L
as.boolean(0L)           # FALSE
as.boolean(2L)           # TRUE
as.boolean(NA_integer_)  # NA


## numeric vectors of length 1 return x != 0
as.boolean(0)         # FALSE
as.boolean(2)         # TRUE
as.boolean(Inf)       # TRUE
as.boolean(NA_real_)  # NA
as.boolean(NaN)       # NA


## complex vectors of length 1 return NA when is.na(x),
## otherwise Re(x) != 0 || Im(x) != 0
as.boolean(complex(real =        0, imaginary =        0))  # FALSE
as.boolean(complex(real =        2, imaginary =        0))  # TRUE
as.boolean(complex(real = NA_real_, imaginary =        0))  # NA
as.boolean(complex(real =        0, imaginary =        2))  # TRUE
as.boolean(complex(real =        2, imaginary =        2))  # TRUE
as.boolean(complex(real = NA_real_, imaginary =        2))  # NA
as.boolean(complex(real =        0, imaginary = NA_real_))  # NA
as.boolean(complex(real =        2, imaginary = NA_real_))  # NA
as.boolean(complex(real = NA_real_, imaginary = NA_real_))  # NA


## character vectors of length 1 return:
## TRUE  when x \%in\% c("T", "TRUE", "True", "true")
## FALSE when x \%in\% c("F", "FALSE", "False", "false")
## NA otherwise
as.boolean("T")
as.boolean("TRUE")
as.boolean("True")
as.boolean("true")
as.boolean("F")
as.boolean("FALSE")
as.boolean("False")
as.boolean("false")
as.boolean("NA")
as.boolean(NA_character_)
as.boolean("unrecognized string")


## list vectors of length 1 always return NA,
## even when the first element is a boolean
as.boolean(list(TRUE))
as.boolean(list(FALSE))
as.boolean(list(NA))


## when `_R_CHECK_LENGTH_1_CONDITION_` is FALSE and
## vectors of length greater than one are supplied,
## only the first element is used
as.boolean(c(TRUE, FALSE, NA))        # logical   vector of length 3
as.boolean(0:10)                      # integer   vector of length 11
as.boolean(seq.int(exp(1), pi, 0.1))  # numeric   vector of length 5
as.boolean(exp((0+6i) * -4:4))        # complex   vector of length 9
as.boolean(c("FALSE", "true"))        # character vector of length 2


## when `_R_CHECK_LENGTH_1_CONDITION_` is TRUE and
## vectors of length greater than one are supplied,
## the return value is always NA
as.boolean(c(TRUE, FALSE, NA)      , `_R_CHECK_LENGTH_1_CONDITION_` = TRUE)
as.boolean(0:10                    , `_R_CHECK_LENGTH_1_CONDITION_` = TRUE)
as.boolean(seq.int(exp(1), pi, 0.1), `_R_CHECK_LENGTH_1_CONDITION_` = TRUE)
as.boolean(exp((0+6i) * -4:4)      , `_R_CHECK_LENGTH_1_CONDITION_` = TRUE)
as.boolean(c("FALSE", "true")      , `_R_CHECK_LENGTH_1_CONDITION_` = TRUE)
}

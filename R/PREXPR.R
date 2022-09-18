PREXPR <- function (x)
.Call(C_PREXPR, substitute(x), parent.frame())


PRENV <- function (x)
.Call(C_PRENV, substitute(x), parent.frame())


PRINFO <- function (x)
.Call(C_PRINFO, substitute(x), parent.frame())

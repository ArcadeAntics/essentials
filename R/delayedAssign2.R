delayedAssign2 <- function (x, value, eval.env = parent.frame(), assign.env = parent.frame(),
    evaluated = TRUE)
.External2(C_delayedassign2, x, if (evaluated) value else substitute(value),
    eval.env, assign.env)


mkPROMISE <- function (value, eval.env = parent.frame(), evaluated = FALSE)
.External2(C_mkpromise, if (evaluated) value else substitute(value), eval.env)

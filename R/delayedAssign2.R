delayedAssign2 <- function (x, value, eval.env = parent.frame(), assign.env = parent.frame(),
    evaluated = FALSE)
{
    x
    eval.env
    assign.env
    eval(substitute(delayedAssign(x, value, eval.env, assign.env),
        list(value = if (evaluated) value else substitute(value))))
}


mkPROMISE <- function (value, eval.env = parent.frame(), evaluated = FALSE)
.Call(C_mkPROMISE, if (evaluated) value else substitute(value), eval.env)

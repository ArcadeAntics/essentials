utils::globalVariables("do")


`%while%` <- function (expr, cond)
.External2(C_dowhile)


`%until%` <- function (expr, cond)
.External2(C_dountil)

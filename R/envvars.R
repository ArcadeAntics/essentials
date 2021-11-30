envvars <- function (...)
{
    value <- .Call(C_envvars, pairlist(...), visible <- TRUE)
    if (visible)
        value
    else invisible(value)
}


getEnvvar <- function (x, default = NULL)
.Call(C_getEnvvar, x, default)

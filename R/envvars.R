envvars <- function (...)
{
    value <- .Call(C_envvars, pairlist(...), visible <- logical(1L))
    if (visible)
        value
    else invisible(value)
}


getEnvvar <- function (x, default = NULL)
.Call(C_getEnvvar, x, default)

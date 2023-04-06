envvars <- function (...)
.External2(C_envvars)


getEnvvar <- function (x, default = NULL)
.External2(C_getenvvar, x)

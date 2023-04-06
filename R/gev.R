dgev <- function (x, location = 0, scale = 1, shape = 0, log = FALSE)
.External2(C_dgev, x, location, scale, shape, log)


pgev <- function (q, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.External2(C_pgev, q, location, scale, shape, lower.tail, log.p)


qgev <- function (p, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.External2(C_qgev, p, location, scale, shape, lower.tail, log.p)


rgev <- function (n, location = 0, scale = 1, shape = 0)
.External2(C_rgev, n, location, scale, shape)

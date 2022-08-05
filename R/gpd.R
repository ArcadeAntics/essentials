dgpd <- function (x, location = 0, scale = 1, shape = 0, log = FALSE)
.Call(C_dgpd, x, location, scale, shape, log)


pgpd <- function (q, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.Call(C_pgpd, q, location, scale, shape, lower.tail, log.p)


qgpd <- function (p, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.Call(C_qgpd, p, location, scale, shape, lower.tail, log.p)


rgpd <- function (n, location = 0, scale = 1, shape = 0)
.Call(C_rgpd, n, location, scale, shape)


# curve(essentials:::dgpd(x, scale = 1, shape =  1), 0, 5, col = "red"  , lwd = 3)
# curve(essentials:::dgpd(x, scale = 1, shape =  5), 0, 5, col = "green", lwd = 3, add = TRUE)
# curve(essentials:::dgpd(x, scale = 1, shape = 20), 0, 5, col = "blue" , lwd = 3, add = TRUE)
# curve(essentials:::dgpd(x, scale = 2, shape =  1), 0, 5, col = "red"  , lwd = 3, lty = "dashed", add = TRUE)
# curve(essentials:::dgpd(x, scale = 2, shape =  5), 0, 5, col = "green", lwd = 3, lty = "dashed", add = TRUE)
# curve(essentials:::dgpd(x, scale = 2, shape = 20), 0, 5, col = "blue" , lwd = 3, lty = "dashed", add = TRUE)

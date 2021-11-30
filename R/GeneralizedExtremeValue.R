

# log1mexp <- function (x)
# {
#     i <- x < -0.5671439
#     value <- x
#     value[i] <- log1p(-exp(x[i]))
#     value[!i] <- log(-expm1(x[!i]))
#     value
# }


# dgev <- function (x, location = 0, scale = 1, shape = 0, log = FALSE)
# {
#     len <- range(length(x), length(location), length(scale), length(shape))
#     if (!len[[1L]])
#         return(numeric())
#     len <- len[[2L]]
#     x     <- rep(x - location, length.out = len)
#     scale <- rep(scale       , length.out = len)
#     shape <- rep(shape       , length.out = len)
#     i <- scale < 0
#     isNA <- function(x) is.na(x) & !is.nan(x)
#     nas <- isNA(x) | isNA(scale) | isNA(shape)
#     nans <- is.nan(x) | is.nan(scale) | is.nan(shape)
#     if (any(!(nas | nans) & i))
#         warning("NaNs produced")
#     nans <- nans | i
#     s <- x/scale
#     f <- rep(1L, length.out = len)
#     f[shape != 0] <- 2L
#     attributes(f) <- list(levels = c("gumbel", "other"), class = "factor")
#     log <- !isFALSE(suppressWarnings(essentials::as.boolean(log)))
#     if (log) {
#         funs <- list(gumbel = function(s, xi) {
#             value <- -s - exp(-s)
#             value[is.na(value) & !is.na(s) & !is.na(xi)] <- -Inf
#             value
#         }, other = function(s, xi) {
#             a <- 1 + xi * s
#             b <- -1/xi
#             value <- (-1 + b) * suppressWarnings(log(a)) - a^b
#             value[a <= 0] <- -Inf
#             value[is.infinite(xi)] <- log(s[is.infinite(xi)] == 0) - 1
#             value
#         })
#         k1 <- -Inf
#         k2 <- Inf
#     }
#     else {
#         funs <- list(gumbel = function(s, xi) {
#             a <- exp(-s)
#             value <- a * exp(-a)
#             value[is.na(value) & !is.na(s) & !is.na(xi)] <- 0
#             value
#         }, other = function(s, xi) {
#             a <- 1 + xi * s
#             b <- -1/xi
#             value <- a^(-1 + b) * exp(-a^b)
#             value[a <= 0] <- 0
#             value[is.infinite(xi)] <- (s[is.infinite(xi)] == 0)/exp(1)
#             value
#         })
#         k1 <- 0
#         k2 <- Inf
#     }
#     value <- unsplit(.mapply(function(fun, s, xi) fun(s, xi),
#         list(funs, split(s, f), split(shape, f)), NULL), f)/scale
#     i <- scale == 0
#     value[i][x[i] != 0] <- k1
#     value[i][x[i] == 0] <- k2
#     value[nans] <- NaN
#     value[nas] <- NA_real_
#     value
# }


# pgev <- function (q, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
#     log.p = FALSE)
# {
#     len <- range(length(q), length(location), length(scale), length(shape))
#     if (!len[[1L]])
#         return(numeric())
#     len <- len[[2L]]
#     q     <- rep(q - location, length.out = len)
#     scale <- rep(scale       , length.out = len)
#     shape <- rep(shape       , length.out = len)
#     i <- scale < 0
#     isNA <- function(x) is.na(x) & !is.nan(x)
#     nas <- isNA(q) | isNA(scale) | isNA(shape)
#     nans <- is.nan(q) | is.nan(scale) | is.nan(shape)
#     if (any(!(nas | nans) & i))
#         warning("NaNs produced")
#     nans <- nans | i
#     s <- q/scale
#     f <- rep(1L, length.out = len)
#     f[shape != 0] <- 2L
#     attributes(f) <- list(levels = c("gumbel", "other"), class = "factor")
#     lower.tail <- !isFALSE(suppressWarnings(essentials::as.boolean(lower.tail)))
#     log.p <- !isFALSE(suppressWarnings(essentials::as.boolean(log.p)))
#     if (log.p) {
#         if (lower.tail) {
#             funs <- list(gumbel = function(s, xi) {
#                 -exp(-s)
#             }, other = function(s, xi) {
#                 a <- 1 + xi * s
#                 value <- -a^(-1/xi)
#                 value[xi > 0 & a <= 0] <- -Inf
#                 value[xi < 0 & a <= 0] <- 0
#                 value
#             })
#             k1 <- -Inf
#             k2 <- 0
#         }
#         else {
#             funs <- list(gumbel = function(s, xi) {
#                 log1mexp(-exp(-s))
#             }, other = function(s, xi) {
#                 a <- 1 + xi * s
#                 value <- log1mexp(-a^(-1/xi))
#                 value[xi > 0 & a <= 0] <- 0
#                 value[xi < 0 & a <= 0] <- Inf
#                 value
#             })
#             k1 <- 0
#             k2 <- -Inf
#         }
#     }
#     else {
#         if (lower.tail) {
#             funs <- list(gumbel = function(s, xi) {
#                 exp(-exp(-s))
#             }, other = function(s, xi) {
#                 a <- 1 + xi * s
#                 value <- exp(-a^(-1/xi))
#                 value[xi > 0 & a <= 0] <- 0
#                 value[xi < 0 & a <= 0] <- 1
#                 value
#             })
#             k1 <- 0
#             k2 <- 1
#         }
#         else {
#             funs <- list(gumbel = function(s, xi) {
#                 -expm1(-exp(-s))
#             }, other = function(s, xi) {
#                 a <- 1 + xi * s
#                 value <- -expm1(-a^(-1/xi))
#                 value[xi > 0 & a <= 0] <- 1
#                 value[xi < 0 & a <= 0] <- 0
#                 value
#             })
#             k1 <- 1
#             k2 <- 0
#         }
#     }
#     value <- unsplit(.mapply(function(fun, s, xi) fun(s, xi),
#         list(funs, split(s, f), split(shape, f)), NULL), f)
#     i <- scale == 0 | is.infinite(shape)
#     value[i][q[i] < 0] <- k1
#     value[i][q[i] >= 0] <- k2
#     value[nans] <- NaN
#     value[nas] <- NA_real_
#     value
# }


# qgev <- function (p, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
#     log.p = FALSE)
# {
#     len <- range(length(p), length(location), length(scale), length(shape))
#     if (!len[[1L]])
#         return(numeric())
#     len <- len[[2L]]
#     p        <- rep(p       , length.out = len)
#     location <- rep(location, length.out = len)
#     scale    <- rep(scale   , length.out = len)
#     shape    <- rep(shape   , length.out = len)
#     lower.tail <- !isFALSE(suppressWarnings(essentials::as.boolean(lower.tail)))
#     log.p <- !isFALSE(suppressWarnings(essentials::as.boolean(log.p)))
#     if (!log.p)
#         p <- log(p)
#     i <- scale < 0 | p > 0
#     isNA <- function(x) is.na(x) & !is.nan(x)
#     nas <- isNA(p) | isNA(location) | isNA(scale) | isNA(shape)
#     nans <- is.nan(p) | is.nan(location) | is.nan(scale) | is.nan(shape)
#     if (any(!(nas | nans) & i))
#         warning("NaNs produced")
#     nans <- nans | i
#     f <- rep(1L, length.out = len)
#     f[shape != 0] <- 2L
#     attributes(f) <- list(levels = c("gumbel", "other"), class = "factor")
#     funs <- list(gumbel = function(p, mu, sigma, xi) {
#         mu - sigma * log(-p)
#     }, other = function(p, mu, sigma, xi) {
#         value <- mu + sigma/xi * ((-p)^-xi - 1)
#         value[xi/sigma == Inf & p != 0] <- Inf
#         value
#     })
#     value <- unsplit(.mapply(function(fun, p, mu, sigma, xi) {
#         fun(p, mu, sigma, xi)
#     }, list(funs, split(p, f), split(location, f), split(scale,
#         f), split(shape, f)), NULL), f)
#     # value[p == -Inf] <- -Inf
#     # value[p == 0] <- Inf
#     if (!lower.tail)
#         value <- 1 - value
#     value[nans] <- NaN
#     value[nas] <- NA_real_
#     value
# }


# rgev <- function (n, location = 0, scale = 1, shape = 0)
# {
#     n <- stats::runif(n)
#     len <- length(n)
#     if (!len) {
#         return(n)
#     }
#     else if (!min(length(location), length(scale), length(shape))) {
#         warning("NAs produced")
#         return(rep(NA_real_, len))
#     }
#     location <- rep(location, length.out = len)
#     scale    <- rep(scale   , length.out = len)
#     shape    <- rep(shape   , length.out = len)
#     i <- scale < 0
#     isNA <- function(x) is.na(x) & !is.nan(x)
#     nas <- isNA(location) | isNA(scale) | isNA(shape)
#     nans <- is.nan(location) | is.nan(scale) | is.nan(shape)
#     if (any(!(nas | nans) & i))
#         warning("NaNs produced")
#     nans <- nans | i
#     f <- rep(1L, length.out = len)
#     f[shape != 0] <- 2L
#     attributes(f) <- list(levels = c("gumbel", "other"), class = "factor")
#     funs <- list(gumbel = function(p, mu, sigma, xi) {
#         mu - sigma * log(-log(n))
#     }, other = function(p, mu, sigma, xi) {
#         mu + sigma/xi * ((-log(n))^-xi - 1)
#     })
#     value <- unsplit(.mapply(function(fun, n, mu, sigma, xi) {
#         fun(n, mu, sigma, xi)
#     }, list(funs, split(n, f), split(location, f), split(scale, f), split(shape, f)), NULL), f)
#     value[nans] <- NaN
#     value[nas] <- NA_real_
#     value
# }


dgev <- function (x, location = 0, scale = 1, shape = 0, log = FALSE)
.Call(C_dgev, x, location, scale, shape, log)


pgev <- function (q, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.Call(C_pgev, q, location, scale, shape, lower.tail, log.p)


qgev <- function (p, location = 0, scale = 1, shape = 0, lower.tail = TRUE,
    log.p = FALSE)
.Call(C_qgev, p, location, scale, shape, lower.tail, log.p)


rgev <- function (n, location = 0, scale = 1, shape = 0)
.Call(C_rgev, n, location, scale, shape)

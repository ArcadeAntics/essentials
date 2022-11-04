ASCII <- function (extended = TRUE, cex = par("cex"), family = par("family"),
    mar = c(0, 2.1, 2.1, 0), plot = TRUE, warn.unused = TRUE)
{
    xlim <- c(0, 16)
    xvals <- 0:15
    if (extended) {
        codes <- 1:255
        ylim <- c(16, 0)
        yvals <- 0:15
    } else {
        codes <- 1:127
        ylim <- c(8, 0)
        yvals <- 0:7
    }
    ASCII <- vapply(as.raw(codes), (rawToChar), "", USE.NAMES = FALSE)
    Encoding(ASCII) <- "latin1"
    ASCII <- enc2utf8(ASCII)
    names(ASCII) <- sprintf("0x%02X", codes)
    if (plot) {
        par(cex = cex, family = family, mar = mar)
        plot(
            xlim = xlim - 0.5, ylim = ylim - 0.5,
            x = NA_real_, y = NA_real_,
            axes = FALSE, ann = FALSE
        )
        text(
            x = rep(xvals, 16), y = rep(yvals, each = 16),
            labels = c(NA, ASCII)
        )
        axis(side = 3L, at = xvals, labels = sprintf("%X", xvals), las = 1)
        axis(side = 2L, at = yvals, labels = sprintf("%X", yvals), las = 1)
        invisible(ASCII)
    } else {
        if (warn.unused) {
            nf <- names(formals())
            nf <- setdiff(nf, c("extended", "plot", "warn.unused"))
            missE <- lapply(nf, function(n) substitute(missing(.),
                list(. = as.name(n))))
            not.miss <- !vapply(missE, eval, environment(), FUN.VALUE = NA)
            if (any(not.miss)) {
                warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of",
                  "arguments %s are not made use of"),
                  paste(sQuote(nf[not.miss]), collapse = ", ")),
                  domain = NA)
            }
        }
        ASCII
    }
}

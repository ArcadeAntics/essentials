.colours.formals <- pairlist(n = quote(expr = ), start = 0, end = 1,
    alpha = quote(expr = ), rev = FALSE)





## to keep Rcmd check happy
palette <- NULL


roundcolor <- function (rgb)
pmax.int(pmin.int(rgb, 1), 0)





simplify.colorRamp <- function (x)
{
    `stats::splinefun.wo.deriv` <- function(x) {
        y <- as.list(body(x))
        body(x) <- as.call(y[!vapply(y, FUN = function(x) {
            "deriv" %in% all.vars(x)
        }, FUN.VALUE = NA)])
        x
    }
    if (identical(environment(x)$interpolate, stats::splinefun)) {
        environment(x)$palette <- lapply(environment(x)$palette,
            FUN = "stats::splinefun.wo.deriv")
    }
    return(x)
}


.colorRampPalette <- function (x)
{
    x <- simplify.colorRamp(x)
    formals(x) <- alist(x = , alpha = )
    alpha <- as.symbol("alpha")
    if (environment(x)$alpha) {
        alpha <- call("if", call("missing", as.symbol("alpha")),
            call("roundcolor", as.call(list(call("[[", as.symbol("palette"), 4L), as.symbol("x")))),
            alpha)
    }
    switch(space <- environment(x)$space, rgb = {
        environment(x)$red   <- function(x) roundcolor(palette[[1L]](x))
        environment(x)$green <- function(x) roundcolor(palette[[2L]](x))
        environment(x)$blue  <- function(x) roundcolor(palette[[3L]](x))
        environment(x)$roundcolor <- function(rgb) pmax.int(pmin.int(rgb, 1), 0)
        environment(environment(x)$red) <- environment(environment(x)$green) <- environment(environment(x)$blue) <- environment(environment(x)$roundcolor) <- environment(x)
        body(x) <- call("rgb", call("red", as.symbol("x")), call("green", as.symbol("x")), call("blue", as.symbol("x")), alpha = alpha)
    }, Lab = {
        body(x) <- call("{",
            call("<-", as.symbol("x"), call("roundcolor", call("convertColor", call("cbind",  as.call(list(call("[[", as.symbol("palette"), 1L), as.symbol("x"))),
                    as.call(list(call("[[", as.symbol("palette"), 2L), as.symbol("x"))), as.call(list(call("[[", as.symbol("palette"), 3L), as.symbol("x")))), from = "Lab",
                    to = "sRGB"))),
            call("rgb", call("[", as.symbol("x"), quote(expr = ), 1L), call("[", as.symbol("x"), quote(expr = ), 2L), call("[", as.symbol("x"), quote(expr = ), 3L), alpha = alpha))
    }, stop(sprintf("unrecognized 'space' (%s)", encodeString(space, quote = "\""))))
    x
}


palette.function <- function (ramp, with.namespace.accessor = TRUE, envir = parent.frame())
{
    value <- function() NULL
    formals(value) <- .colours.formals
    asi <- if (with.namespace.accessor)
        call("::", as.symbol("essentials"), as.symbol("as.scalar.integer"))
    else as.symbol("as.scalar.integer")
    if (!is.call(ramp))
        ramp <- as.symbol(ramp)
    body(value) <- call("{",
        call("if", call(">", call("<-", as.symbol("n"), as.call(list(asi, as.symbol("n")))), 0), as.call(c(as.symbol("{"),
            call("if", call("||", call("||", call("||", call("<", as.symbol("start"), 0), call("<", as.symbol("end"), 0)), call(">", as.symbol("start"), 1)), call(">", as.symbol("end"), 1)),
                call("stop", "'start' and 'end' must be in [0, 1].")),
            call("<-", as.symbol("cols"), as.call(list(ramp, call("seq.int", as.symbol("start"), as.symbol("end"), length.out = as.symbol("n")), as.symbol("alpha")))),
            call("if", as.symbol("rev"),
                call("rev", as.symbol("cols")),
            as.symbol("cols")))),
        call("character", 0)))
    environment(value) <- envir
    return(value)
}


as.colorRampPalette <- function (...)
{
    if (nargs() == 1L && is.function(..1))
        ramp <- ..1
    else ramp <- .colorRampPalette(colorRamp(...))
    palette.function("ramp")
}





special.palettes <- character(0)


hcl.color.ramps <- sapply(grDevices::hcl.pals(), function(palette) {
    if (grDevices::hcl.colors(256, palette)[256] == grDevices::hcl.colors(2, palette)[2]) {
        .colorRampPalette(
            grDevices::colorRamp(
                grDevices::hcl.colors(256, palette),
                interpolate = "spline"
            )
        )
    }
    else {
        special.palettes <<- c(special.palettes, palette)
        .colorRampPalette(
            grDevices::colorRamp(
                c(grDevices::hcl.colors(255, palette), grDevices::hcl.colors(1, palette)),
                interpolate = "spline"
            )
        )
    }
}, simplify = FALSE)


inferno.ramp <- function (x, alpha)
hcl.color.ramps[["Inferno"]](x, alpha)


inferno.colors <- palette.function("inferno.ramp", with.namespace.accessor = FALSE)


plasma.ramp <- function (x, alpha)
hcl.color.ramps[["Plasma"]](x, alpha)


plasma.colors <- palette.function("plasma.ramp", with.namespace.accessor = FALSE)


viridis.ramp <- function (x, alpha)
hcl.color.ramps[["Viridis"]](x, alpha)


viridis.colors <- palette.function("viridis.ramp", with.namespace.accessor = FALSE)


hcl.colors2 <- function (n, palette = "viridis", start = 0, end = if (palette %in%
    special.palettes) (n - 1)/n else 1, alpha, rev = FALSE)
{
    if ((n <- as.scalar.integer(n)) > 0) {
        fx <- function(x) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]",
            "", x))
        p <- charmatch(fx(palette), fx(names(hcl.color.ramps)))
        if (is.na(p))
            stop("'palette' does not match any given palette")
        if (p < 1L)
            stop("'palette' is ambiguous")
        palette <- names(hcl.color.ramps)[[p]]
        if (start < 0 || end < 0 || start > 1 || end > 1)
            stop("'start' and 'end' must be in [0, 1].")
        cols <- hcl.color.ramps[[p]](seq.int(start, end, length.out = n),
            alpha)
        if (rev)
            rev(cols)
        else cols
    }
    else character(0)
}





gg.colors <- function ()
{
    if ((n <- as.scalar.integer(n)) > 0) {
        if (start < 0 || end < 0 || start > 1 || end > 1)
            stop("'start' and 'end' must be in [0, 1].")
        cols <- hcl(h = 15 + 360 * seq.int(start, end, length.out = n),
            c = 100, l = 65, alpha = alpha)
        if (rev)
            rev(cols)
        else cols
    }
    else character(0)
}
formals(gg.colors) <- .colours.formals
formals(gg.colors)$end <- quote((n - 1)/n)





color.with.alpha <- function (x, alpha)
{
    i <- NULL
    if (is.matrix(x) && is.integer(x) && all(!is.na(x)) && all(x >=
        0L) && all(x <= 255L) && (identical(rownames(x), c("red",
        "green", "blue")) || identical(rownames(x), c("red",
        "green", "blue", "alpha")))) {
        x <- t(x)/255
        if (missing(alpha) && ncol(x) == 4L)
            alpha <- x[, 4L]
    }
    else if (!is.matrix(x) && !is.data.frame(x)) {
        x <- as.character(x)
        i <- is.na(x)
        x <- t(col2rgb(x, alpha = TRUE))/255
        if (missing(alpha))
            alpha <- x[, 4L]
    }
    value <- rgb(x, alpha = alpha)
    if (!is.null(i))
        value[i] <- NA_character_
    return(value)
}


as.hex.code <- function (x)
color.with.alpha(x)

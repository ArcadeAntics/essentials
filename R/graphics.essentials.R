as.ee <- function (expr, envir)
{
    if (inherits(expr, "formula")) {
        envir <- environment(expr)
        if (!is.environment(envir))
            stop("invalid 'expr', 'environment(expr)' must be an environment")
        if (length(expr) != 2)
            stop("'expr' should be of length 2, see ?legend.dimensions")
        expr <- expr[[2L]]
    }
    else {
        if (!is.environment(envir))
            stop("invalid 'envir', must be an environment")
        if (is.expression(expr))
            expr <- aslength1(expr)[[1L]]
    }
    if (!is.call(expr))
        stop("invalid 'expr', must be a call, expression, or formula")
    list(expr, envir)
}


legend.dimensions <- function (expr, envir = parent.frame(),
                enclos = if (is.list(envir) || is.pairlist(envir))
                             parent.frame() else baseenv(),
    trace = FALSE)
{
    trace <- if (trace) TRUE else FALSE
    ee <- as.ee(expr, as.env(envir, enclos))


    # do NOT prompt the user before a new page of output is started.
    # this is intended for use with 'example'
    op <- options(device.ask.default = FALSE)
    on.exit(options(op), add = TRUE, after = FALSE)
    if (trace && !identical(op, options("device.ask.default")))
        cat("Changed option \"device.ask.default\" from", deparse(op[[1L]]),
            "to", deparse(getOption("device.ask.default")), "\n\n")


    opar <- NULL
    if (.Device != "null device") {


        # upon exiting, reset the graphics device to its previous value
        odev <- dev.cur()
        on.exit(dev.set(odev), add = TRUE, after = FALSE)
        if (trace) {
            cat("The current graphics device is:\n")
            print(odev)
            cat("\n")
        }


        # we're assuming that the graphics device in which the user is plotting
        # the legend is, in fact, the current graphics device. as such, we grab
        # the following graphical parameters that may affect the legend size
        # from the current graphics device, and will use them in our new
        # graphics device
        opar <- par("cex", "family", "font", "lheight")
        if (trace) {
            cat(strwrap("Using the following graphical parameters from the current graphics device in the new graphics device:"),
                sep = "\n")
            print(opar)
            cat("\n")
        }


        oldask <- devAskNewPage(FALSE)
        if (oldask) {
            on.exit(devAskNewPage(oldask), add = TRUE, after = FALSE)
            if (trace)
                cat("Changed 'grDevices::devAskNewPage' from TRUE to FALSE\n\n")
        }
    }


    # upon exiting, remove the image file 'filename'
    filename <- tempfile()
    on.exit(unlink(filename), add = TRUE, after = FALSE)
    if (trace)
        cat("Creating temporary image in file:\n  ", sQuote(filename), "\n\n", sep = "")


    png(filename = filename, width = 480, height = 480, res = 48)


    # upon exiting, shut down the graphics device started by 'png'
    cdev <- dev.cur()
    on.exit(dev.off(cdev), add = TRUE, after = FALSE)
    if (trace) {
        cat("sys.on.exit() = ")
        print(sys.on.exit())
        cat("\n")
    }


    if (!is.null(opar))
        par(opar)


    # strictly speaking, not necessary, but it makes the conversion from user
    # coordinates to inches much nicer (conversion is almost 1:1)
    par(mar = rep(0, 4L))


    plot.default(
        xlim = c(0, 10), ylim = c(0, 10), xaxs = "i", yaxs = "i",
        x = NA_real_, y = NA_real_,
        axes = FALSE, frame.plot = FALSE, ann = FALSE
    )
    value <- eval(ee[[1L]], ee[[2L]])
    if (trace) {
        cat("'expr' evaluated to:\n")
        print(value)
        cat("\n")
    }
    if (!is.list(value) || !is.list(value[["rect"]]) ||
        !is.numeric(value[["rect"]][["w"]]) || !is.numeric(value[["rect"]][["h"]]))
        stop("invalid 'expr', did not evaluate to a form like 'graphics::legend'")
    list(w = value[["rect"]][["w"]]/xinch(warn.log = FALSE),
         h = value[["rect"]][["h"]]/yinch(warn.log = FALSE))
}


add.legend <- function (expr, envir = parent.frame(),
                enclos = if (is.list(envir) || is.pairlist(envir))
                             parent.frame() else baseenv())
{
    ee <- as.ee(expr, as.env(envir, enclos))
    eval(ee[[1L]], ee[[2L]])
}


legendPart <- function (expr)
as.ee(expr, environment())[[1L]]


`legendPart<-` <- function (expr, value)
{
    if (!is.call(value))
        stop("invalid 'value', must be a call")
    if (inherits(expr, "formula"))
        expr[[2L]] <- value
    else if (is.expression(expr))
        expr[[1L]] <- value
    else if (is.call(expr))
        expr <- value
    else stop("invalid 'expr'")
    expr
}


show.colors <- function (x)
{
    opar <- par(mar = rep(0, 4L))
    on.exit(par(opar))
    y <- as.matrix(x)
    if (length(y))
        image(x = array(seq_along(y), dim(y)), col = y)
    invisible(x)
}


fix.xlog <- function (x)
if (par("xlog")) 10^x else x


fix.ylog <- function (y)
if (par("ylog")) 10^y else y


as.ld <- function (x, envir = parent.frame(),
                enclos = if (is.list(envir) || is.pairlist(envir))
                             parent.frame() else baseenv())
{
    if (is.list(x) && is.numeric(x[["w"]]) && is.numeric(x[["h"]]))
        x
    else legend.dimensions(x, as.env(envir, enclos))
}


adj.margins <- function (x, envir = parent.frame(),
             enclos = if (is.list(envir) || is.pairlist(envir))
                          parent.frame() else baseenv(),
    extra = 0.1, side = 4L)
{
    x <- as.ld(x, as.env(envir, enclos))
    side <- as.scalar.integer(side)
    if (!side %in% 1:4)
        stop("invalid 'side' argument")
    side <- as.character(side)

    extra <- as.scalar.numeric(extra) * par("cex")

    mai <- par("mai")
    switch(side, `1` = {
        mai[1L] <- max(x[["h"]]) - extra
    }, `2` = {
        mai[2L] <- max(x[["w"]]) - extra
    }, `3` = {
        mai[3L] <- max(x[["h"]]) + extra
    }, `4` = {
        mai[4L] <- max(x[["w"]]) + extra
    })
    par(mai = mai)
}


location <- function (x, envir = parent.frame(),
             enclos = if (is.list(envir) || is.pairlist(envir))
                          parent.frame() else baseenv(),
    adj = 1, extra = 0.1, side = 4L)
{
    x <- as.ld(x, as.env(envir, enclos))
    side <- as.scalar.integer(side)
    if (!side %in% 1:4)
        stop("invalid 'side' argument")
    side <- as.character(side)


    adj <- as.scalar.numeric(adj)
    extra <- as.scalar.numeric(extra) * par("cex")


    # side refers to
    # 1: bottom
    # 2: left
    # 3: top
    # 4: right
    #
    #
    # adj refers to
    # side = 1,3: 0 being left, 1 being right
    # side = 2,4: 0 being bottom, 1 being top
    list(x = as.call(list(quote(essentials::fix.xlog), switch(side, `1` = , `3` = {


        if (adj == 0)


            quote(


                # the left side position
                graphics::par("usr")[1])


        else if (adj == 1) {



            bquote(


                # the right side position
                graphics::par("usr")[2]


                # subtract the width of the legend
                - graphics::xinch(.(max(x[["w"]])), warn.log = FALSE))


        }
        else bquote(


            # the left side position
            graphics::par("usr")[1]


            # add the distance between left and right side, multiplied by 'adj'
            + diff(graphics::par("usr")[1:2]) * .(adj)


            # subtract the width of the legend, multiplied by 'adj'
            - graphics::xinch(.(max(x[["w"]]) * adj), warn.log = FALSE))


    }, `2` = {


        bquote(


            # the left side position
            graphics::par("usr")[1]


            # subtract the width of the legend
            - graphics::xinch(.(max(x[["w"]])), warn.log = FALSE))


    }, `4` = {


        quote(


            # the right side position
            graphics::par("usr")[2])


    }))), y = as.call(list(quote(essentials::fix.ylog), switch(side, `2` = , `4` = {


        if (adj == 1)


            quote(


                # the top side position
                graphics::par("usr")[4])


        else if (adj == 0)


            bquote(


                # the bottom side position
                graphics::par("usr")[3]


                # add the height of the legend
                + graphics::yinch(.(max(x[["h"]])), warn.log = FALSE))


        else bquote(


            # the bottom side position
            graphics::par("usr")[3]


            # add the distance between bottom and top side, multiplied by 'adj'
            + diff(graphics::par("usr")[3:4]) * .(adj)


            # add the height of the legend, multiplied by '1 - adj'
            + graphics::yinch(.(max(x[["h"]]) * (1 - adj)), warn.log = FALSE))



    }, `3` = {


        bquote(


            # the top side position
            graphics::par("usr")[4]


            # add the height of the legend
            + graphics::yinch(.(max(x[["h"]])), warn.log = FALSE))


    }, `1` = {


        quote(


            # the bottom side position
            graphics::par("usr")[3])


    }))))
}


`location<-` <- function (x, envir = parent.frame(),
             enclos = if (is.list(envir) || is.pairlist(envir))
                          parent.frame() else baseenv(),
    adj = 1, extra = 0.1, side = 4L, value)
{
    legendPart(x)[c("x", "y")] <- value
    x
}


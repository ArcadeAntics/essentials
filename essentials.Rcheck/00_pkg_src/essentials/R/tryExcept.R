delayedAssign2 <- function (x, value, eval.env = parent.frame(1), assign.env = parent.frame(1),
    evaluated = FALSE)
{
    eval(substitute(delayedAssign(x, value, eval.env, assign.env),
        list(x = x, value = if (evaluated) value else substitute(value),
            eval.env = eval.env, assign.env = assign.env)))
}


tryExcept <- function (expr, ..., finally)
{
    if (!missing(finally)) {
        sfinally <- substitute(finally)


        # straight from `withAutoprint`
        if (is.call(sfinally) && sfinally[[1]] == quote(`{`)) {
            sfinally <- as.list(sfinally[-1])


            if (length(sfinally)) {


                # this is likely a horrible way to do this, but let me explain
                #
                # * we make a bunch of promises to the expressions in `sfinally`
                #     using `delayAssign2`
                # * then we add those promises to `on.exit` with
                #     `C_tryExcept_on.exit_setup` (i tried doing that part in R,
                #     but couldn't do it without using `eval`, which would put
                #     `on.exit` in the wrong frame)
                nms <- paste0("sfinally", seq_along(sfinally))
                .mapply(delayedAssign2, list(
                    x = nms,
                    value = sfinally
                ), list(
                    eval.env = parent.frame(),
                    assign.env = environment(),
                    evaluated = TRUE
                ))
                .Call(C_tryExcept_on.exit_setup,
                    lapply(nms, "as.symbol"), environment())
            }
        }
        else on.exit(finally)
    }
    tryCatch(expr = expr, ...)
}

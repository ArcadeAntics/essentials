EM <- function (x, h, f)
.mapply(function(x, f, h) x + h * f, list(x, f), list(h))


EM2 <- function (x, y)
.mapply(`+`, list(x, y))


EM3 <- function (x, y)
.mapply(`*`, list(x), list(y))


EMT1 <- r"{
{
    nm <- names(initialConditions)
    if (is.null(nm)) {
        warning("initial conditions should have names")
        nm <- character(length(initialConditions))
    }
    else if (anyNA(nm) || !all(nzchar(nm)))
        warning("initial conditions names contain NA or empties")
    value <- ""
    if (is.list(independent)) {
        value <- names(independent)[1L]
        if (is.null(value))
            value <- ""
        independent <- independent[[1L]]
    }
    if (!is.numeric(independent))
        stop("'independent' must be a list or numeric")
    if (is.list(initialConditions)) {
        if (!all(vapply(initialConditions, is.numbers, NA)))
            stop("each initial condition must be numeric or complex")
    }
    else if (!is.numbers(initialConditions))
        stop("initial conditions must be numeric or complex")
    nm <- c(value, nm)
    if (!length(independent)) {
        value <- lapply(c("numeric", vapply(initialConditions, mode, "")), vector)
        names(value) <- nm
        return(value)
    }
    fun <- match.fun(fun)
    value <- formals(fun)
    if (!"..." %in% names(value) && length(value) < 2L)
        stop("'fun' must accept at least two arguments\n",
            "* first an independent variable value\n",
            "* second a series of dependent variable values\n")
    tmp <- fun(independent[1L], initialConditions)
    if (mode(initialConditions) != mode(tmp))
        stop(sprintf("initial conditions are mode \"%s\" while 'fun' produces mode \"%s\"",
            mode(initialConditions), mode(tmp)))
    if (length(tmp) != length(initialConditions))
        stop(sprintf("initial conditions are of length %d while 'fun' produces length %d",
            length(initialConditions), length(tmp)))
    if (is.list(initialConditions)) {
        m1 <- vapply(initialConditions, mode, "")
        m2 <- vapply(tmp, mode, "")
        if (m <- match(FALSE, m1 == m2, 0L))
            stop(sprintf("initial condition [%d] is mode \"%s\" while 'fun' produces mode \"%s\"",
                m, m1[m], m2[m]))
        m1 <- lengths(initialConditions)
        m2 <- lengths(tmp)
        if (m <- match(FALSE, m1 == m2, 0L))
            stop(sprintf("initial condition [%d] is of length %d while 'fun' produces length %d",
                m, m1[m], m2[m]))
    }
    names(initialConditions) <- NULL
    value <- vector("list", length(independent))
    value[1L] <- list(initialConditions)
    h <- diff(independent)
    if (is.list(initialConditions))
        for (n in seq_along(h)) }"


EMT2 <- r"{
    else for (n in seq_along(h)) }"


EMT3 <- r"{
    value <- c(list(independent), if (is.list(initialConditions))
        .mapply(function(cond, i) if (cond)
            sapply(value, "[[", i)
        else lapply(value, "[[", i), list(lengths(initialConditions) ==
            1L, seq_along(initialConditions)), NULL)
    else lapply(seq_len(nrow(value <- do.call(cbind, value))),
        function(i) value[i, ]))
    names(value) <- nm
    value
}}"


EMF <- function (is.listcode, elsecode, envir = parent.frame())
{
    fun <- function(code, indent) {
        if (code[[1L]] == quote(`{`)) {
            n <- length(code)
            value <- deparse(code[-n])
            value <- value[-c(1L, length(value))]
            value <- c(value, paste0("    value[[n + 1L]] <- ", paste(deparse(code[[n]]), collapse = "\n")), "}")
            value <- paste0(strrep(" ", indent), value)
            value <- c("{", value)
            value <- paste(value, collapse = "\n")
        }
        else {
            value <- deparse(code)
            value[1L] <- paste0("value[[n + 1L]] <- ", value[1L])
            value[-1L] <- paste0(strrep(" ", indent), value)[-1L]
            value <- paste(value, collapse = "\n")
        }
        value
    }
    is.listcode <- fun(substitute(is.listcode), 8L)
    elsecode <- fun(substitute(elsecode), 4L)
    value <- eval(parse(text = paste0("function (independent, initialConditions, fun) ",
        EMT1, is.listcode, EMT2, elsecode, EMT3)))
    environment(value) <- envir
    value
}


EulerMethod <- RK1 <- EMF({
    x <- value[[n]]
    EM(x, h[n], fun(independent[n], x))
}, {
    x <- value[[n]]
    x + h[n] * fun(independent[n], x)
})


ImprovedEulerMethod <- RK2 <- EMF({
    x <- value[[n]]
    t <- independent[n]
    h1 <- h[n]
    k1 <- fun(t, x)
    k2 <- fun(t + h1, EM(x, h1, k1))
    EM(x, h1/2, EM2(k1, k2))
}, {
    x <- value[[n]]
    t <- independent[n]
    h1 <- h[n]
    k1 <- fun(t, x)
    k2 <- fun(t + h1, x + h1 * k1)
    x + h1/2 * (k1 + k2)
})


RungeKuttaMethod <- RK4 <- EMF({
    x <- value[[n]]
    t <- independent[n]
    h1 <- h[n]
    h2 <- h1/2
    k1 <- fun(t, x)
    k2 <- fun(t + h2, EM(x, h2, k1))
    k3 <- fun(t + h2, EM(x, h2, k2))
    k4 <- fun(t + h1, EM(x, h1, k3))
    EM(x, h1/6, EM2(EM2(EM2(k1, EM3(k2, 2)), EM3(k3, 2)), k4))
}, {
    x <- value[[n]]
    t <- independent[n]
    h1 <- h[n]
    h2 <- h1/2
    k1 <- fun(t, x)
    k2 <- fun(t + h2, x + h2 * k1)
    k3 <- fun(t + h2, x + h2 * k2)
    k4 <- fun(t + h1, x + h1 * k3)
    x + h1/6 * (k1 + 2 * k2 + 2 * k3 + k4)
})


remove(EMT1, EMT2, EMT3, EMF)


.RK4 <- function (independent, initialConditions, fun, ..., xname)
{
    fun <- match.fun(fun)
    if (missing(xname))
        xname <- deparse1(substitute(independent), collapse = "\n",
            width.cutoff = 60L)
    xname
    # .External2(.C_rk4, independent, initialConditions, fun, xname)
}

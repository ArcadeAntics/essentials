write.csv <- function ()
{
    prov <- !c(sep = missing(sep), dec = missing(dec), qmethod = missing(qmethod))
    if (any(prov))
        warning(gettextf("attempt to set %s ignored",
            paste(sQuote(names(prov)[prov]), collapse = ", ")))
    col.names <- if (is.logical(row.names) && !row.names)
        TRUE
    else NA
    sep <- ","
    dec <- "."
    qmethod <- "double"
    utils::write.table()
}
formals(write.csv) <- formals(utils::write.table)
body(write.csv)[[length(body(write.csv))]] <- local({
    nf <- names(formals(write.csv))
    x <- lapply(nf, base::as.symbol)
    names(x) <- nf
    as.call(c(list(quote(utils::write.table)), x))
})

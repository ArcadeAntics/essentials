write.csv <- function ()
{
    provided <- !c(col.names = missing(col.names), sep = missing(sep),
        dec = missing(dec), qmethod = missing(qmethod))
    if (any(provided))
        warning(gettextf("attempt to set %s ignored",
            paste(sQuote(names(provided)[provided]), collapse = ", ")))
    col.names <- if (append)
        FALSE
    else if (is.logical(row.names) && !row.names)
        TRUE
    else NA
    sep <- ","
    dec <- "."
    qmethod <- "double"
    write.table()
}
formals(write.csv) <- formals(utils::write.table)
formals(write.csv)[c("sep", "dec", "qmethod")] <- list(",", ".", "double")
body(write.csv)[[length(body(write.csv))]] <- local({
    nf <- names(formals(write.csv))
    x <- lapply(nf, base::as.symbol)
    names(x) <- nf
    as.call(c(list(quote(write.table)), x))
})

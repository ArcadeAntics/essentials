write.csv <- function() NULL
formals(write.csv) <- formals(utils::write.table)
formals(write.csv)[c("sep", "dec", "qmethod")] <- list(",", ".", "double")
body(write.csv) <- bquote({
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
    .({
        on.exit(rm(tmp))
        tmp <- sapply(names(formals(write.csv)), as.name)
        names(tmp)[tmp == "..."] <- ""
        as.call(c(as.name("write.table"), tmp))
    })
})

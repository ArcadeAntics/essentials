tryCatch({
    FILE2 <- essentials::writeArgs(letters)
    FILE1 <- essentials::writeArgs(paste0("@", basename(FILE2)))
    parser <- essentials::ArgumentParser()
    parser$add.argument("strings", nargs = "*")
    pargs <- parser$parse.args(FILE1)
    stopifnot(
        identical(essentials::Args(pargs, "all"), letters)
    )
}, finally = unlink(substring(c(FILE1, FILE2), 2L)))

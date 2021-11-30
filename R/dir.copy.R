dir.copy <- function (from, to, overwrite = FALSE, ..., all.files = TRUE,
    full.names = FALSE, recursive = TRUE)
{
    dir.create(to, showWarnings = FALSE, recursive = TRUE)


    files <- list.files(path = from, ..., all.files = all.files,
        full.names = FALSE, recursive = recursive)


    for (path in unique(dirname(file.path(to, files)))) {
        dir.create(path, showWarnings = FALSE, recursive = TRUE)
    }


    if (!all(i <- file.copy(
        from = file.path(from, files),
        to   = file.path(to  , files),
        overwrite = overwrite
    )))
        stop(sprintf(ngettext(sum(!i),
            "unable to copy file%s\nfrom %s\nto   %s",
            "unable to copy files%s\nfrom %s\nto   %s"),
            paste0("\n  ", dQuote(files[!i]), collapse = ""),
            dQuote(from), dQuote(to)))
}

delayedAssign(".list_files2.py", system.file(package = .packageName, "python", "list_files2.py", mustWork = TRUE))
delayedAssign(".list_dirs2.py" , system.file(package = .packageName, "python", "list_dirs2.py" , mustWork = TRUE))


list.files2 <- function (path = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
    include.dirs = FALSE, no.. = FALSE)
{
    if (.Platform$OS.type != "windows" || identical(R.version[["crt"]], "ucrt"))
        return(list.files(path = path, pattern = pattern, all.files = all.files,
            full.names = full.names, recursive = recursive, ignore.case = ignore.case,
            include.dirs = include.dirs, no.. = no..))


    if (is.character(pattern) && length(pattern) >= 1 && !is.na(pattern[[1L]]))
        pattern <- pattern[[1L]]
    else if (!is.null(pattern) && !(is.character(pattern) && length(pattern) < 1))
        stop(gettextf("invalid '%s' argument", "pattern", domain = "R"), domain = NA)


    ignore.case <- if (ignore.case) TRUE else FALSE


    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))


    true <- jsonlite::unbox(TRUE)
    false <- jsonlite::unbox(FALSE)
    jsonlite::write_json(
        list(
            path.expand(path),
            if (all.files)    true else false,
            if (full.names)   true else false,
            if (recursive)    true else false,
            if (include.dirs) true else false,
            if (no..)         true else false
        ),
        tmpfile
    )


    python(file = .list_files2.py, args = tmpfile, mustWork = TRUE, quiet = TRUE)
    value <- readLines(tmpfile, encoding = "UTF-8")
    if (length(pattern))
        value <- value[grepl(pattern, basename2(value), ignore.case = ignore.case)]
    value
}


dir2 <- list.files2


list.dirs2 <- function (path = ".", full.names = TRUE, recursive = TRUE)
{
    if (.Platform$OS.type != "windows" || identical(R.version[["crt"]], "ucrt"))
        return(list.dirs(path = path, full.names = full.names,
            recursive = recursive))


    tmpfile <- tempfile()
    on.exit(unlink(tmpfile))


    true <- jsonlite::unbox(TRUE)
    false <- jsonlite::unbox(FALSE)
    jsonlite::write_json(
        list(
            path.expand(path),
            if (full.names) true else false,
            if (recursive)  true else false
        ),
        tmpfile
    )


    python(file = .list_dirs2.py, args = tmpfile, mustWork = TRUE, quiet = TRUE)
    readLines(tmpfile, encoding = "UTF-8")
}


# local({
#     FILES <- tempfile(c("dir", ".dir", "file", ".file"))
#     on.exit(unlink(FILES, recursive = TRUE, force = TRUE))
#     lapply(FILES[1:2], dir.create)
#     file.create(FILES[3:4])
#     fun <- function(tmpdir) {
#         FILES <- tempfile(c("dir", ".dir", "file", ".file"), tmpdir)
#         lapply(FILES[1:2], dir.create)
#         file.create(FILES[3:4])
#         FILES[1:2]
#     }
#     lapply(fun(FILES[1:2]), function(x) {
#         lapply(fun(x), function(xx) {
#             lapply(fun(xx), function(xxx) {
#
#             })
#         })
#     })
#     path <- paste0(tempdir(), if (.Platform$OS.type == "windows") "\\" else "/")
#     fun <- function(expr) {
#         writeLines(deparse1(substitute(expr), "\n"))
#         writeLines(expr)
#         cat("\n\n\n\n\n")
#     }
#     list.files(path, recursive = TRUE , include.dirs = TRUE , all.files = TRUE , full.names = TRUE ) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = TRUE , all.files = TRUE , full.names = FALSE) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = TRUE , all.files = FALSE, full.names = TRUE ) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = TRUE , all.files = FALSE, full.names = FALSE) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = FALSE, all.files = TRUE , full.names = TRUE ) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = FALSE, all.files = TRUE , full.names = FALSE) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = FALSE, all.files = FALSE, full.names = TRUE ) |> fun()
#     list.files(path, recursive = TRUE , include.dirs = FALSE, all.files = FALSE, full.names = FALSE) |> fun()
#
#
#     list.files(path, recursive = FALSE, no.. = TRUE , all.files = TRUE , full.names = TRUE ) |> fun()
#     list.files(path, recursive = FALSE, no.. = TRUE , all.files = TRUE , full.names = FALSE) |> fun()
#     list.files(path, recursive = FALSE, no.. = TRUE , all.files = FALSE, full.names = TRUE ) |> fun()
#     list.files(path, recursive = FALSE, no.. = TRUE , all.files = FALSE, full.names = FALSE) |> fun()
#     list.files(path, recursive = FALSE, no.. = FALSE, all.files = TRUE , full.names = TRUE ) |> fun()
#     list.files(path, recursive = FALSE, no.. = FALSE, all.files = TRUE , full.names = FALSE) |> fun()
#     list.files(path, recursive = FALSE, no.. = FALSE, all.files = FALSE, full.names = TRUE ) |> fun()
#     list.files(path, recursive = FALSE, no.. = FALSE, all.files = FALSE, full.names = FALSE) |> fun()
#     invisible()
# })

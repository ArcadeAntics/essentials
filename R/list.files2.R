delayedAssign("list.files2_py.script", system.file(package = .packageName, "python", "list_files2.py", mustWork = TRUE))
delayedAssign("list.dirs2_py.script" , system.file(package = .packageName, "python", "list_dirs2.py" , mustWork = TRUE))


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
        stop("invalid 'pattern' argument")


    ignore.case <- if (ignore.case) TRUE else FALSE


    outfile <- tempfile()
    on.exit(unlink(outfile))


    # since tab is not allowed in Windows paths, we can safely paste together and separate later
    # but just to be sure, remove any NA elements or elements containing tab
    path <- path.expand(path)
    path <- paste(path[!is.na(path) & !grepl("\t", path, fixed = TRUE)], collapse = "\t")


    oenv <- envvars(


        R_ESSENTIALS_LIST_FILES2_PATH         = path,
        R_ESSENTIALS_LIST_FILES2_ALL_FILES    = if (all.files)    TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_FULL_NAMES   = if (full.names)   TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_RECURSIVE    = if (recursive)    TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS = if (include.dirs) TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_NODOTDOT     = if (no..)         TRUE else FALSE,


        R_ESSENTIALS_LIST_FILES2_OUTFILE      = outfile
    )
    on.exit(envvars(oenv), add = TRUE)


    python(file = list.files2_py.script, mustWork = TRUE, quiet = TRUE)
    value <- readLines(outfile, encoding = "UTF-8")
    if (length(pattern))
        value <- value[grepl(pattern, basename(value), ignore.case = ignore.case)]
    return(value)
}


dir2 <- list.files2


list.dirs2 <- function (path = ".", full.names = TRUE, recursive = TRUE)
{
    if (.Platform$OS.type != "windows" || identical(R.version[["crt"]], "ucrt"))
        return(list.dirs(path = path, full.names = full.names,
            recursive = recursive))


    outfile <- tempfile()
    on.exit(unlink(outfile))


    path <- path.expand(path)
    path <- paste(path[!is.na(path) & !grepl("\t", path, fixed = TRUE)], collapse = "\t")


    oenv <- envvars(


        R_ESSENTIALS_LIST_DIRS2_PATH       = path,
        R_ESSENTIALS_LIST_DIRS2_FULL_NAMES = if (full.names) TRUE else FALSE,
        R_ESSENTIALS_LIST_DIRS2_RECURSIVE  = if (recursive)  TRUE else FALSE,


        R_ESSENTIALS_LIST_DIRS2_OUTFILE    = outfile
    )
    on.exit(envvars(oenv), add = TRUE)


    python(file = list.dirs2_py.script, mustWork = TRUE, quiet = TRUE)
    return(readLines(outfile, encoding = "UTF-8"))
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

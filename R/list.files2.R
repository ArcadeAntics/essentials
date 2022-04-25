list.files2 <- function (path = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
    include.dirs = FALSE, no.. = FALSE)
{
    if (.Platform$OS.type != "windows" || getRversion() >= "4.2.0")
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


    oenv <- envvars(


        # since tab is not allowed in Windows paths, we can safely paste together and separate later
        R_ESSENTIALS_LIST_FILES2_PATH         = paste(path.expand(path), collapse = "\t"),
        R_ESSENTIALS_LIST_FILES2_ALL_FILES    = if (all.files)    TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_FULL_NAMES   = if (full.names)   TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_RECURSIVE    = if (recursive)    TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS = if (include.dirs) TRUE else FALSE,
        R_ESSENTIALS_LIST_FILES2_NODOTDOT     = if (no..)         TRUE else FALSE,


        R_ESSENTIALS_LIST_FILES2_OUTFILE      = outfile
    )
    on.exit(envvars(oenv), add = TRUE)


    FILE <- tempfile(fileext = ".py")
    on.exit(unlink(FILE), add = TRUE)


    writeLines(dedent("
        import os


        all_files    = bool(os.environ['R_ESSENTIALS_LIST_FILES2_ALL_FILES'   ])
        full_names   = bool(os.environ['R_ESSENTIALS_LIST_FILES2_FULL_NAMES'  ])
        recursive    = bool(os.environ['R_ESSENTIALS_LIST_FILES2_RECURSIVE'   ])
        include_dirs = bool(os.environ['R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS'])
        nodotdot     = bool(os.environ['R_ESSENTIALS_LIST_FILES2_NODOTDOT'    ])


        outfile      = os.environ['R_ESSENTIALS_LIST_FILES2_OUTFILE']
        outfile      = open(outfile, mode = 'w', encoding = 'UTF-8')


        if not recursive:
            include_dirs = True


        if recursive or not all_files:
            nodotdot = True


        dotdot = not nodotdot


        for path in os.environ['R_ESSENTIALS_LIST_FILES2_PATH'].split('\\t'):
            n = len(path) + 1
            for root, dirs, files in os.walk(path):
                if root == path:
                    if dotdot:
                        dirs = ['.', '..'] + dirs
                    if include_dirs:
                        for dir in dirs:
                            if all_files or not dir.startswith('.'):
                                if full_names:
                                    outfile.writelines(path + '/')
                                outfile.writelines(dir + '\\n')
                    for file in files:
                        if all_files or not file.startswith('.'):
                            if full_names:
                                outfile.writelines(path + '/')
                            outfile.writelines(file + '\\n')
                elif recursive:
                    root2 = root[n:].replace('\\\\', '/')
                    do = True
                    if not all_files:
                        for x in root2.split('/'):
                            if x.startswith('.'):
                                do = False
                                break
                    if do:
                        if include_dirs:
                            for dir in dirs:
                                if all_files or not dir.startswith('.'):
                                    if full_names:
                                        outfile.writelines(path + '/')
                                    outfile.writelines(root2 + '/' + dir + '\\n')
                        for file in files:
                            if all_files or not file.startswith('.'):
                                if full_names:
                                    outfile.writelines(path + '/')
                                outfile.writelines(root2 + '/' + file + '\\n')
        outfile.close()
    "), FILE)
    python(file = FILE, mustWork = TRUE, quiet = TRUE)
    value <- readLines(outfile, encoding = "UTF-8")
    if (length(pattern))
        value <- value[grepl(pattern, basename(value), ignore.case = ignore.case)]
    return(value)
}


dir2 <- list.files2

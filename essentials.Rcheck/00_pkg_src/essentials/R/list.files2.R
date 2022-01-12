list.files2 <- function (path = ".", pattern = NULL, all.files = FALSE,
    full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
    include.dirs = FALSE, no.. = FALSE)
{
    if (.Platform$OS.type != "windows")
        return(list.files(path = path, pattern = pattern, all.files = all.files,
            full.names = full.names, recursive = recursive, ignore.case = ignore.case,
            include.dirs = include.dirs, no.. = no..))


    outfile <- tempfile()
    on.exit(unlink(outfile))


    oenv <- envvars(
        R_ESSENTIALS_LIST_FILES2_PATH         = paste(path.expand(path), collapse = "\t"),
        R_ESSENTIALS_LIST_FILES2_ALL_FILES    = if (all.files)    "True" else "False",
        R_ESSENTIALS_LIST_FILES2_FULL_NAMES   = if (full.names)   "True" else "False",
        R_ESSENTIALS_LIST_FILES2_RECURSIVE    = if (recursive)    "True" else "False",
        R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS = if (include.dirs) "True" else "False",
        R_ESSENTIALS_LIST_FILES2_NODOTDOT     = if (no..)         "True" else "False",


        R_ESSENTIALS_LIST_FILES2_OUTFILE      = outfile
    )
    on.exit(envvars(oenv), add = TRUE)


    FILE <- tempfile(fileext = ".py")
    on.exit(unlink(FILE), add = TRUE)


    writeLines(dedent(r"{
        import os


        all_files    = os.environ["R_ESSENTIALS_LIST_FILES2_ALL_FILES"   ] == "True"
        full_names   = os.environ["R_ESSENTIALS_LIST_FILES2_FULL_NAMES"  ] == "True"
        recursive    = os.environ["R_ESSENTIALS_LIST_FILES2_RECURSIVE"   ] == "True"
        include_dirs = os.environ["R_ESSENTIALS_LIST_FILES2_INCLUDE_DIRS"] == "True"
        nodotdot     = os.environ["R_ESSENTIALS_LIST_FILES2_NODOTDOT"    ] == "True"


        outfile      = os.environ["R_ESSENTIALS_LIST_FILES2_OUTFILE"]
        outfile      = open(outfile, mode = "w", encoding = "UTF-8")


        if not recursive:
            include_dirs = True


        if recursive or not all_files:
            nodotdot = True


        dotdot = not nodotdot


        for path in os.environ["R_ESSENTIALS_LIST_FILES2_PATH"].split("\t"):
            n = len(path) + 1
            for root, dirs, files in os.walk(path):
                if root == path:
                    if dotdot:
                        dirs = [".", ".."] + dirs
                    if include_dirs:
                        for dir in dirs:
                            if all_files or not dir.startswith("."):
                                if full_names:
                                    outfile.writelines(path + "/")
                                outfile.writelines(dir + "\n")
                    for file in files:
                        if all_files or not file.startswith("."):
                            if full_names:
                                outfile.writelines(path + "/")
                            outfile.writelines(file + "\n")
                elif recursive:
                    root2 = root[n:].replace("\\", "/")
                    do = True
                    if not all_files:
                        for x in root2.split("/"):
                            if x.startswith("."):
                                do = False
                                break
                    if do:
                        if include_dirs:
                            for dir in dirs:
                                if all_files or not dir.startswith("."):
                                    if full_names:
                                        outfile.writelines(path + "/")
                                    outfile.writelines(root2 + "/" + dir + "\n")
                        for file in files:
                            if all_files or not file.startswith("."):
                                if full_names:
                                    outfile.writelines(path + "/")
                                outfile.writelines(root2 + "/" + file + "\n")
        outfile.close()
    }"), FILE)
    python(file = FILE, mustWork = TRUE, quiet = TRUE)
    value <- readLines(outfile, encoding = "UTF-8")
    if (!is.null(pattern))
        value <- value[grepl(pattern, basename(value), ignore.case = ignore.case)]
    return(value)
}


dir2 <- list.files2

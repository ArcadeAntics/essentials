.Rcmd <- function (options = NULL, command = "", args = NULL, ..., name = "",
    dir = R.home("bin"))
{
    command <- c(
        if (.Platform$OS.type == "windows")
            shQuote(file.path(dir, "Rcmd.exe"))
        else c(shQuote(file.path(dir, "R")), shQuote(options), "CMD"),
        shQuote(command),
        shQuote(args)
    )
    command <- paste(command, collapse = " ")
    invisible(.system(command = command, ...))
}


.switch2 <- function (EXPR, TRUE_expr = invisible(), FALSE_expr = invisible(),
    alt_expr = invisible())
{
    if (is.character(EXPR)) {
        switch(EXPR,
        T = ,
        `TRUE` = ,
        True = ,
        true = TRUE_expr,
        F = ,
        `FALSE` = ,
        False = ,
        false = FALSE_expr,
        if (!is.na(EXPR)) alt_expr else if (EXPR) NULL)
    }
    else if (EXPR)
        TRUE_expr
    else FALSE_expr
}


.check_this <- function (
    build_opts = NULL,

    INSTALL = TRUE,
    INSTALL_opts = NULL,

    check = TRUE,
    check_opts = NULL,
    `_R_CHECK_CRAN_INCOMING_` = NULL,

    chdir = FALSE, n = 0L, file = this.dir(verbose = FALSE, n = n + 1L),
    repos = path.join(if (file == ".") "" else file, "..", "PACKAGES"),
    special = !missing(repos))
{
    # check.this               package:essentials                R Documentation
    #
    # Build, Install, and Check a Package Conveniently
    #
    #
    #
    # Description:
    #
    # Performs 'R CMD build', 'R CMD INSTALL', and 'R CMD check' on a source
    # package specified by 'file'.
    #
    #
    #
    # Usage:
    #
    # check.this(
    #
    #     build.vignettes = TRUE, no.build.vignettes = !build.vignettes,
    #     manual = TRUE, no.manual = !manual,
    #     resave.data = FALSE, no_resave_data = FALSE,
    #
    #     build = FALSE,
    #     multiarch = TRUE, no_multiarch = !multiarch,
    #     keep.source = NA,
    #
    #     check = TRUE, as_cran = FALSE,
    #
    #     chdir = FALSE, file = here()
    #
    # )
    #
    #
    #
    # Arguments:
    #
    #
    # build.vignettes, no.build.vignettes, manual, no.manual, resave.data,
    # no_resave_data
    #
    #     further arguments passed to 'R CMD build'
    #
    # build, multiarch, no_multiarch, keep.source
    #
    #     further arguments passed to 'R CMD INSTALL'
    #
    # check
    #
    #     should 'R CMD check' be run?
    #
    # as_cran
    #
    #     further arguments passed to 'R CMD check' (if check is TRUE)
    #
    # chdir
    #
    #     temporarily change the working directory to `file`?
    #
    # n, file
    #
    #     character string; the directory of the package source, by default
    #     the executing script's directory
    #
    # special
    #
    #     move the tarball to the local repository specified by `repos`?
    #
    # repos
    #
    #     character string; directory of the local repository


    make_build_opts <- function(
        ...,
        force = FALSE,
        keep_empty_dirs = FALSE,
        no_build_vignettes = FALSE,
        no_manual = FALSE,
        resave_data = FALSE, no_resave_data = FALSE,
        compact_vignettes = FALSE,
        compression = NULL,
        md5 = FALSE,
        log = FALSE,
        user = NULL)
    {
        dots <- list(...)
        n <- names(dots)
        if (!is.null(n) && length(i <- which(nzchar(n)))) {
            f <- setdiff(names(formals()), "...")
            f <- utils::capture.output(print(f, quote = FALSE, max = 99))
            warning(
                "named argument(s) ",
                paste(dQuote(n[i]), collapse = ", "),
                "\ndo not match names of formals:\n",
                paste(f, collapse = "\n")
            )
            names(dots) <- NULL
        }
        if (!all(vapply(dots, is.character, NA))) {
            stop("non-character argument(s)")
        }
        c(
            if (force) "--force",
            if (keep_empty_dirs) "--keep-empty-dirs",
            if (no_build_vignettes) "--no-build-vignettes",
            if (no_manual) "--no-manual",
            .switch2(resave_data,
            TRUE_expr = "--resave-data",
            FALSE_expr = if (no_resave_data) "--no-resave-data",
            alt_expr = paste0("--resave-data=", match.arg(resave_data, c("no", "best", "gzip")))
            ),
            .switch2(compact_vignettes,
            TRUE_expr = "--compact-vignettes",
            alt_expr = paste0("--compact-vignettes=", match.arg(compact_vignettes, c("no", "qpdf", "gs", "gs+qpdf", "both")))
            ),
            if (!is.null(compression))
                paste0("--compression=", match.arg(compression, c("gzip", "none", "bzip2", "xz"))),
            if (md5) "--md5",
            if (log) "--log",
            if (!is.null(user))
                paste0("--user=", as.scalar.string(user)),
            dots,
            recursive = TRUE,
            use.names = FALSE
        )
    }


    build_opts <- if (is.null(build_opts))
        make_build_opts()
    else do.call("make_build_opts", build_opts, quote = TRUE)


    make_INSTALL_opts <- function(
        ...,
        clean = FALSE,
        preclean = FALSE,
        debug = FALSE,
        library = NULL,
        no_configure = FALSE,
        no_docs = FALSE,
        html = FALSE, no_html = FALSE,
        latex = FALSE,
        example = FALSE,
        fake = FALSE,
        no_lock = FALSE,
        lock = FALSE,
        pkglock = FALSE,
        build = FALSE,
        install_tests = FALSE,
        no_R = FALSE, no_libs = FALSE, no_data = FALSE, no_help = FALSE, no_demo = FALSE, no_exec = FALSE, no_inst = FALSE,
        no_multiarch = FALSE,
        libs_only = FALSE,
        data_compress = NULL,
        resave_data = FALSE,
        compact_docs = FALSE,
        with_keep.source = FALSE, without_keep.source = FALSE,
        with_keep.parse.data = FALSE, without_keep.parse.data = FALSE,
        byte_compile = FALSE, no_byte_compile = FALSE,
        staged_install = FALSE, no_staged_install = FALSE,
        no_test_load = FALSE,
        no_clean_on_error = FALSE,
        merge_multiarch = FALSE,
        use_vanilla = FALSE,
        use_LTO = FALSE, no_use_LTO = FALSE)
    {


        if (special) build <- FALSE


        dots <- list(...)
        n <- names(dots)
        if (!is.null(n) && length(i <- which(nzchar(n)))) {
            f <- setdiff(names(formals()), "...")
            f <- utils::capture.output(print(f, quote = FALSE, max = 99))
            warning(
                "named argument(s) ",
                paste(dQuote(n[i]), collapse = ", "),
                "\ndo not match names of formals:\n",
                paste(f, collapse = "\n")
            )
            names(dots) <- NULL
        }
        if (!all(vapply(dots, is.character, NA))) {
            stop("non-character argument(s)")
        }
        c(
            if (clean) "--clean",
            if (preclean) "--preclean",
            if (debug) "--debug",
            if (!is.null(library))
                paste0("--library=", as.scalar.string(library)),
            if (no_configure) "--no-configure",
            if (no_docs) "--no-docs",
            if (html) "--html" else if (no_html) "--no-html",
            if (latex) "--latex",
            if (example) "--example",
            if (fake) "--fake",
            if (no_lock) "--no-lock" else if (lock) "--lock" else if (pkglock) "--pkglock",
            if (build) "--build",
            if (install_tests) "--install-tests",
            if (no_R) "--no-R",
            if (no_libs) "--no-libs",
            if (no_data) "--no-data",
            if (no_help) "--no-help",
            if (no_demo) "--no-demo",
            if (no_exec) "--no-exec",
            if (no_inst) "--no-inst",
            if (no_multiarch) "--no-multiarch",
            if (libs_only) "--libs-only",
            if (!is.null(data_compress))
                paste0("--data-compress=", match.arg(data_compress, c("gzip", "none", "bzip2", "xz"))),
            if (resave_data) "--resave-data",
            if (compact_docs) "--compact-docs",
            if (with_keep.source) "--with-keep.source" else if (without_keep.source) "--without-keep.source",
            if (with_keep.parse.data) "--with-keep.parse.data" else if (without_keep.parse.data) "--without-keep.parse.data",
            if (byte_compile) "--byte-compile" else if (no_byte_compile) "--no-byte-compile",
            if (staged_install) "--staged-install" else if (no_staged_install) "--no-staged-install",
            if (no_test_load) "--no-test-load",
            if (no_clean_on_error) "--no-clean-on-error",
            if (merge_multiarch) "--merge-multiarch",
            if (use_vanilla) "--use-vanilla",
            if (use_LTO) "--use-LTO" else if (no_use_LTO) "--no-use-LTO",
            dots,
            recursive = TRUE,
            use.names = FALSE
        )
    }


    INSTALL <- if (INSTALL) TRUE else FALSE
    if (INSTALL) {
        INSTALL_opts <- if (is.null(INSTALL_opts))
            make_INSTALL_opts()
        else do.call("make_INSTALL_opts", INSTALL_opts, quote = TRUE)
    }


    make_check_opts <- function(
        ...,
        library = NULL,
        output = NULL,
        no_clean = FALSE,
        no_codoc = FALSE,
        no_examples = FALSE,
        no_install = FALSE,
        no_tests = FALSE,
        no_manual = FALSE,
        no_vignettes = FALSE,
        no_build_vignettes = FALSE,
        ignore_vignettes = FALSE,
        run_dontrun = FALSE,
        run_donttest = FALSE,
        use_gct = FALSE,
        use_valgrind = FALSE,
        timings = FALSE,
        install_args = NULL,
        test_dir = NULL,
        no_stop_on_test_error = FALSE,
        check_subdirs = NULL,
        as_cran = FALSE)
    {
        dots <- list(...)
        n <- names(dots)
        if (!is.null(n) && length(i <- which(nzchar(n)))) {
            f <- setdiff(names(formals()), "...")
            f <- utils::capture.output(print(f, quote = FALSE, max = 99))
            warning(
                "named argument(s) ",
                paste(dQuote(n[i]), collapse = ", "),
                "\ndo not match names of formals:\n",
                paste(f, collapse = "\n")
            )
            names(dots) <- NULL
        }
        if (!all(vapply(dots, is.character, NA))) {
            stop("non-character argument(s)")
        }
        c(
            if (!is.null(library))
                paste0("--library=", as.scalar.string(library)),
            if (!is.null(output))
                paste0("--output=", as.scalar.string(output)),
            if (no_clean) "--no-clean",
            if (no_codoc) "--no-codoc",
            if (no_examples) "--no-examples",
            if (no_install) "--no-install",
            if (no_tests) "--no-tests",
            if (no_manual) "--no-manual",
            if (no_vignettes) "--no-vignettes",
            if (no_build_vignettes) "--no-build-vignettes",
            if (ignore_vignettes) "--ignore-vignettes",
            if (run_dontrun) "--run-dontrun",
            if (run_donttest) "--run-donttest",
            if (use_gct) "--use-gct",
            if (use_valgrind) "--use-valgrind",
            if (timings) "--timings",
            if (!is.null(install_args))
                paste0("--install-args=", install_args),
            if (!is.null(test_dir))
                paste0("--test-dir=", as.scalar.string(test_dir)),
            if (no_stop_on_test_error) "--no-stop-on-test-error",
            if (!is.null(check_subdirs))
                paste0("--check-subdirs=", match.arg(check_subdirs, c("default", "yes", "no"))),
            if (as_cran) "--as-cran",
            dots,
            recursive = TRUE,
            use.names = FALSE
        )
    }


    check <- if (check) TRUE else FALSE
    if (check) {
        `_R_CHECK_CRAN_INCOMING_`
        check_opts <- if (is.null(check_opts))
            make_check_opts()
        else do.call("make_check_opts", check_opts, quote = TRUE)
    }


    # return(list(build_opts, if (INSTALL) INSTALL_opts, if (check) check_opts))


    if (!is.character(file) || length(file) != 1L) {
        stop("invalid 'file'")
    } else if (grepl("^(ftp|ftps|http|https)://", file)) {
        stop("cannot 'check.this' on a URL")
    } else if (chdir && file != ".") {
        owd <- getwd()
        if (is.null(owd))
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(file)
        file <- "."
    }
    ## force any remaining promises before unloading the DLLs
    special
    repos


    desc <- read.dcf(file.path(file, "DESCRIPTION"))
    if (nrow(desc) != 1L)
        stop("bruh wtf are you doing???")
    desc <- structure(c(desc), names = colnames(desc))
    pkgname <- desc[["Package"]]
    version <- desc[["Version"]]
    if (anyNA(c(pkgname, version)) ||
        !grepl(pkgname, pattern = paste0("^(", .standard_regexps()$valid_package_name   , ")$")) ||
        !grepl(version, pattern = paste0("^(", .standard_regexps()$valid_package_version, ")$")))
        stop("invalid package DESCRIPTION file")


    finished <- unloaded <- FALSE
    on.exit({
        if (.Platform$GUI == "RStudio" &&
            exists(".rs.api.restartSession", "tools:rstudio", inherits = FALSE)) {
            command <- if (finished)
                deparse1(call("library", as.symbol(pkgname)),
                    collapse = "\n", width.cutoff = 80L)
            get(".rs.api.restartSession", "tools:rstudio", inherits = FALSE)(command)
        }
        else if (unloaded)
            warning(sprintf("%s %s was unloaded, please restart the R session",
                if (.Platform$OS.type == "windows") "DLL" else "shared object",
                sQuote(pkgname)))
    }, add = TRUE)



    if (.Platform$OS.type == "windows" &&
        isNamespaceLoaded(pkgname))
    {
        libpath <- getNamespaceInfo(pkgname, "path")
        success <- FALSE
        tryCatch({
            DLLs <- getNamespaceInfo(pkgname, "DLLs")
            success <- TRUE
        }, error = function(e) NULL)
        if (success) {
            for (chname in names(DLLs)) {
                library.dynam.unload(chname, libpath)
                unloaded <- TRUE
            }
        }
        DLLs <- getLoadedDLLs()
        DLLs <- DLLs[startsWith(vapply(DLLs, `[[`, "path", FUN.VALUE = ""), paste0(libpath, "/"))]
        for (DLL in DLLs) {
            dyn.unload(DLL[["path"]])
        }
    }


    tar_file <- paste0(pkgname, "_", version, ".tar.gz")


    value <- .Rcmd(command = "build", args = c(build_opts, file), mustWork = TRUE)
    cat("\n")


    if (special) {
        fields <- c("Package", "Version", "Depends", "Suggests",
            "License", "MD5sum", "NeedsCompilation", "Imports",
            "LinkingTo", "Enhances", "OS_type")
        desc <- structure(desc[fields], names = fields)
        if (is.na(desc["NeedsCompilation"]))
            desc["NeedsCompilation"] <- if (dir.exists(file.path(file, "src"))) "yes" else "no"
        desc["MD5sum"] <- tools::md5sum(tar_file)
        desc <- t(desc)


        tar_path <- file.path(repos, "src", "contrib")
        dir.create(tar_path, showWarnings = FALSE, recursive = TRUE)


        PACKAGES_file <- file.path(tar_path, "PACKAGES")
        if (file.exists(PACKAGES_file)) {
            text <- readLines(PACKAGES_file)
            tmpfile <- tempfile("PACKAGES")
            on.exit(unlink(tmpfile), add = TRUE)
            conn <- file(tmpfile, "w")
            tryCatch({
                matchThis <- paste0("Package: ", pkgname)
                if (i <- match(matchThis, text, 0L)) {
                    writeLines(text[seq_len(i - 1L)], conn)
                } else if (i <- match(TRUE, startsWith(text, "Package: ") & text > matchThis, 0L)) {
                    writeLines(text[seq_len(i - 1L)], conn)
                    i <- i - 2L
                } else {
                    i <- length(text)
                    writeLines(c(text, ""), conn)
                }
                write.dcf(desc, conn, indent = 8L, width = 72L)
                j <- which(text == "")
                j <- j[j > i]
                if (length(j) > 0) {
                    j <- j[[1L]]
                    writeLines(text[j:length(text)], conn)
                }
            }, finally = close(conn))
        } else {
            tmpfile <- tempfile("PACKAGES")
            on.exit(unlink(tmpfile), add = TRUE)
            write.dcf(desc, tmpfile, indent = 8L, width = 72L)
        }
        if (!file.rename(tmpfile, PACKAGES_file))
            stop(sprintf("unable to rename file '%s' to '%s'", tmpfile, PACKAGES_file))


        archive_path <- file.path(tar_path, "Archive", pkgname)
        dir.create(archive_path, showWarnings = FALSE, recursive = TRUE)


        to <- file.path(tar_path, tar_file)
        if (!file.rename(tar_file, to))
            stop("failure to rename")


        files <- setdiff(list.files(tar_path), tar_file)
        files <- files[startsWith(files, paste0(pkgname, "_"))]
        new_files <- file.path(archive_path, files)
        files <- file.path(tar_path, files)
        if (!all(file.rename(files, new_files)))
            stop("failure to rename")


        tar_file <- to
    }


    if (INSTALL) {
        value <- .Rcmd(command = "INSTALL", args = c(INSTALL_opts, tar_file), mustWork = TRUE)
        cat("\n")
    }
    finished <- TRUE


    if (check) {
        if (!is.null(`_R_CHECK_CRAN_INCOMING_`)) {
            `_R_CHECK_CRAN_INCOMING_` <- as.logical(`_R_CHECK_CRAN_INCOMING_`)[1L]
            oenv <- Sys.getenv("_R_CHECK_CRAN_INCOMING_", NA)
            if (is.na(oenv))
                on.exit(Sys.unsetenv("_R_CHECK_CRAN_INCOMING_"), add = TRUE)
            else on.exit(Sys.setenv(`_R_CHECK_CRAN_INCOMING_` = oenv), add = TRUE)
            Sys.setenv(`_R_CHECK_CRAN_INCOMING_` = if (is.na(`_R_CHECK_CRAN_INCOMING_`))
                ""
            else if (`_R_CHECK_CRAN_INCOMING_`)
                "TRUE"
            else "FALSE")
        }
        value <- .Rcmd(command = "check", args = c(check_opts, tar_file), mustWork = TRUE)
        cat("\n")
    }


    invisible(value)
}


doc <- function (fun)
{
    r"{
        doc                                                      R Documentation

        Access Function Documentation



        Description:

        Get the documentation string at the beginning of a function's body.



        Usage:

        doc(fun)



        Arguments:

        fun

            the function from which to extract the documentation string.



        Value:

        character string if a documentation string is found, NULL otherwise.



        Example:

        doc(doc)
    }"

    fun <- match.fun(fun)
    x <- body(fun)
    if (!(is.call(x) && is.symbol(x[[1]]) && x[[1]] == quote(`{`)))
        return(NULL)
    if (length(x) < 3)
        return(NULL)
    x <- x[[2]]
    if (is.character(x))
        essentials::dedent(x)
}


.update_DESCRIPTION_Date <- function (file = this.dir(verbose = FALSE, n = n + 1L), n = 0L)
{
    ## change the Date in the DESCRIPTION to the current date
    if (!is.character(file) || length(file) != 1L)
        stop(gettextf("'%s' must be a character string", "file", domain = "R"))
    file <- path.join(file, "DESCRIPTION")
    x <- local({
        conn <- file(file, "rb", encoding = "")
        on.exit(close(conn))
        readLines(conn)
    })
    n <- grep("^Date: ", x)
    if (length(n) > 1L)
        stop(gettextf("in '%s':\n multiple lines that start with \"Date: \"", file))
    if (length(n) < 1L) {}
    else {
        date <- format(Sys.time(), "Date: %Y-%m-%d", "UTC")
        if (x[[n]] != date) {
            x[[n]] <- date
            tmpfile <- tempfile("DESCRIPTION")
            on.exit(unlink(tmpfile))
            local({
                conn <- file(tmpfile, "w", encoding = "")
                on.exit(close(conn))
                writeLines(x, conn, useBytes = TRUE)
            })
            if (!file.rename(tmpfile, file))
                stop(sprintf("unable to rename file '%s' to '%s'",
                    tmpfile, file))
            on.exit()
        }
    }
    invisible()
}

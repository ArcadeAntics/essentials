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


switch2 <- function (EXPR, TRUE.expr = invisible(), FALSE.expr = invisible(),
    alt.expr = invisible())
{
    if (is.character(EXPR)) {
        switch(EXPR, T = , `TRUE` = , True = , true = {
            TRUE.expr
        }, F = , `FALSE` = , False = , false = {
            FALSE.expr
        }, if (!is.na(EXPR))
            alt.expr
        else if (EXPR) {
        })
    }
    else if (EXPR)
        TRUE.expr
    else FALSE.expr
}


.check_this <- function (
    force = FALSE,
    keep.empty.dirs = FALSE,
    build.build.vignettes = TRUE, build.no.build.vignettes = !build.build.vignettes,
    build.manual = TRUE, build.no.manual = !build.manual,
    build.resave.data = FALSE, build.no.resave.data = FALSE,
    compact.vignettes = FALSE,
    compression = NULL,
    md5 = FALSE,
    log = FALSE,

    INSTALL = TRUE,
    INSTALL.clean = FALSE,
    preclean = FALSE,
    debug = FALSE,
    INSTALL.library = NULL,
    configure = TRUE, no.configure = !configure,
    docs = TRUE, no.docs = !docs,
    html = FALSE, no.html = FALSE,
    latex = FALSE,
    example = FALSE,
    fake = FALSE,
    no.lock = FALSE, lock = FALSE,
    pkglock = FALSE,
    build = FALSE,
    install.tests = FALSE,
    no.R = FALSE, no.libs = FALSE, no.data = FALSE, no.help = FALSE, no.demo = FALSE, no.exec = FALSE, no.inst = FALSE,
    multiarch = TRUE, no.multiarch = !multiarch,
    libs.only = FALSE,
    data.compress = NULL,
    INSTALL.resave.data = FALSE,
    compact.docs = FALSE,
    with.keep.source = FALSE, without.keep.source = FALSE, keep.source = NA,
    with.keep.parse.data = FALSE, without.keep.parse.data = FALSE, keep.parse.data = NA,
    byte.compile = FALSE, no.byte.compile = FALSE,
    staged.install = FALSE, no.staged.install = FALSE,
    test.load = TRUE, no.test.load = !test.load,
    clean.on.error = TRUE, no.clean.on.error = !clean.on.error,
    merge.multiarch = FALSE,
    use.vanilla = FALSE,
    use.LTO = FALSE, no.use.LTO = FALSE,

    check = TRUE,
    check.library = NULL,
    output = NULL,
    check.clean = TRUE, check.no.clean = !check.clean,
    codoc = TRUE, no.codoc = !codoc,
    examples = TRUE, no.examples = !examples,
    install = TRUE, no.install = !install,
    tests = TRUE, no.tests = !tests,
    check.manual = TRUE, check.no.manual = !check.manual,
    vignettes = TRUE, no.vignettes = !vignettes,
    check.build.vignettes = TRUE, check.no.build.vignettes = !check.build.vignettes,
    ignore.vignettes = FALSE,
    run.dontrun = FALSE,
    run.donttest = FALSE,
    use.gct = FALSE,
    use.valgrind = FALSE,
    timings = FALSE,
    install.args = NULL,
    test.dir = NULL,
    no.stop.on.test.error = FALSE,
    check.subdirs = NULL,
    as.cran = FALSE,
    `_R_CHECK_CRAN_INCOMING_` = NULL,

    chdir = FALSE, n = 0L, file = this.dir(verbose = FALSE, n = n + 1L),
    special = !missing(repos), repos = path.join(if (file == ".") "" else file, "..", "PACKAGES"))
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
    #     resave.data = FALSE, build.no.resave.data = FALSE,
    #
    #     build = FALSE,
    #     multiarch = TRUE, no.multiarch = !multiarch,
    #     keep.source = NA,
    #
    #     check = TRUE, as.cran = FALSE,
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
    # build.no.resave.data
    #
    #     further arguments passed to 'R CMD build'
    #
    # build, multiarch, no.multiarch, keep.source
    #
    #     further arguments passed to 'R CMD INSTALL'
    #
    # check
    #
    #     should 'R CMD check' be run?
    #
    # as.cran
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


    if (special) build <- FALSE


    build_args <- c(
        if (force) "--force",
        if (keep.empty.dirs) "--keep-empty-dirs",
        if (build.no.build.vignettes) "--no-build-vignettes",
        if (build.no.manual)          "--no-manual",
        switch2(build.resave.data, TRUE.expr = {
            "--resave-data"
        }, FALSE.expr = {
            if (build.no.resave.data)
                "--no-resave-data"
        }, alt.expr = {
            paste0("--resave-data=", match.arg(build.resave.data, c("no", "best", "gzip")))
        }),
        switch2(compact.vignettes, TRUE.expr = {
            "--compact-vignettes"
        }, alt.expr = {
            paste0("--compact-vignettes=", match.arg(compact.vignettes, c("no", "qpdf", "gs", "gs+qpdf", "both")))
        }),
        if (!is.null(compression))
            paste0("--compression=", match.arg(compression, c("gzip", "none", "bzip2", "xz"))),
        if (md5) "--md5",
        if (log) "--log"
    )


    INSTALL <- if (INSTALL) TRUE else FALSE
    if (INSTALL) {
        keep.source
        keep.parse.data
        INSTALL_args <- c(
            if (INSTALL.clean) "--clean",
            if (preclean) "--preclean",
            if (debug) "--debug",
            if (!is.null(INSTALL.library))
                paste0("--library=", as.scalar.string(INSTALL.library)),
            if (no.configure) "--no-configure",
            if (no.docs) "--no-docs",
            {
                if (html)
                    "--html"
                else if (no.html)
                    "--no-html"
            },
            if (latex) "--latex",
            if (example) "--example",
            if (fake) "--fake",
            {
                if (no.lock)
                    "--no-lock"
                else if (lock)
                    "--lock"
                else if (pkglock)
                    "--pkglock"
            },
            if (build) "--build",
            if (install.tests) "--install-tests",
            if (no.R) "--no-R",
            if (no.libs) "--no-libs",
            if (no.data) "--no-data",
            if (no.help) "--no-help",
            if (no.demo) "--no-demo",
            if (no.exec) "--no-exec",
            if (no.inst) "--no-inst",
            if (no.multiarch) "--no-multiarch",
            if (libs.only) "--libs-only",
            if (!is.null(data.compress))
                paste0("--data-compress=", match.arg(data.compress, c("gzip", "none", "bzip2", "xz"))),
            if (INSTALL.resave.data) "--resave-data",
            if (compact.docs) "--compact-docs",
            {
                if (with.keep.source)
                    "--with-keep.source"
                else if (without.keep.source)
                    "--without-keep.source"
                else tryCatch({
                    if (keep.source)
                        "--with-keep.source"
                    else "--without-keep.source"
                }, error = function(c) NULL)
            },
            {
                if (with.keep.parse.data)
                    "--with-keep.parse.data"
                else if (without.keep.parse.data)
                    "--without-keep.parse.data"
                else tryCatch({
                    if (keep.parse.data)
                        "--with-keep.parse.data"
                    else "--without-keep.parse.data"
                }, error = function(c) NULL)
            },
            {
                if (byte.compile)
                    "--byte-compile"
                else if (no.byte.compile)
                    "--no-byte-compile"
            },
            {
                if (staged.install)
                    "--staged-install"
                else if (no.staged.install)
                    "--no-staged-install"
            },
            if (no.test.load) "--no-test-load",
            if (no.clean.on.error) "--no-clean-on-error",
            if (merge.multiarch) "--merge-multiarch",
            if (use.vanilla) "--use-vanilla",
            {
                if (use.LTO)
                    "--use-LTO"
                else if (no.use.LTO)
                    "--no-use-LTO"
            }
        )
    }


    check <- if (check) TRUE else FALSE
    if (check) {
        `_R_CHECK_CRAN_INCOMING_`
        check_args <- c(
            if (!is.null(check.library))
                paste0("--library=", as.scalar.string(check.library)),
            if (!is.null(output))
                paste0("--output=", as.scalar.string(output)),
            if (check.no.clean) "--no-clean",
            if (no.codoc) "--no-codoc",
            if (no.examples) "--no-examples",
            if (no.install) "--no-install",
            if (no.tests) "--no-tests",
            if (check.no.manual) "--no-manual",
            if (no.vignettes) "--no-vignettes",
            if (check.no.build.vignettes) "--no-build-vignettes",
            if (ignore.vignettes) "--ignore-vignettes",
            if (run.dontrun) "--run-dontrun",
            if (run.donttest) "--run-donttest",
            if (use.gct) "--use-gct",
            if (use.valgrind) "--use-valgrind",
            if (timings) "--timings",
            if (!is.null(install.args))
                paste0("--install-args=", install.args),
            if (!is.null(test.dir))
                paste0("--test-dir=", as.scalar.string(test.dir)),
            if (no.stop.on.test.error) "--no-stop-on-test-error",
            if (!is.null(check.subdirs))
                paste0("--check-subdirs=", match.arg(check.subdirs, c("default", "yes", "no"))),
            if (as.cran) "--as-cran"
        )
    }


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


    value <- .Rcmd(command = "build", args = c(build_args, file), mustWork = TRUE)
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
        value <- .Rcmd(command = "INSTALL", args = c(INSTALL_args, tar_file), mustWork = TRUE)
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
        value <- .Rcmd(command = "check", args = c(check_args, tar_file), mustWork = TRUE)
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

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


check.this <- function (
    force = FALSE,
    keep.empty.dirs = FALSE,
    build.build.vignettes = TRUE, build.no.build.vignettes = !build.build.vignettes,
    build.manual = TRUE, build.no.manual = !build.manual,
    build.resave.data = FALSE, build.no.resave.data = FALSE,
    compact.vignettes = FALSE,
    compression = NULL,
    md5 = FALSE,
    log = FALSE,

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

    chdir = FALSE, file = here(), special = FALSE, where = "../PACKAGES")
{
    # Check_This {essentials}                                    R Documentation
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
    # Check_This(
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
    #     should 'R CMD check' be run?
    #
    # as.cran
    #
    #     further arguments passed to 'R CMD check' (if check is TRUE)
    #
    # chdir
    #
    #     temporarily change the working directory to the directory containing
    #     `file`?
    #
    # file
    #
    #     character string; the directory of the package source, by default
    #     the executing script's directory



    # local({
    #     x <- formals(essentials:::check_this)
    #     assign.env <- parent.env(environment())
    #     for (name in names(x)) {
    #         essentials::delayedAssign2(name, x[[name]], assign.env = assign.env, evaluated = TRUE)
    #     }
    # })
    # switch2 <- essentials:::switch2
    # Rcmd <- essentials:::Rcmd
    #
    #
    # file <- "C:/Users/andre/Documents/iris"
    # special <- TRUE
    # check <- FALSE
    # chdir <- TRUE


    if (special) {
        build <- FALSE
        dir <- R.home("bin")
    } else dir <- NULL


    build.args <- c(
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


    keep.source
    keep.parse.data
    INSTALL.args <- c(
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


    check <- if (check) TRUE else FALSE
    if (check)
        check.args <- c(
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


    if (!is.character(file) || length(file) != 1L) {
        stop("invalid 'file'")
    } else if (grepl("^(ftp|ftps|http|https)://", file)) {
        stop("cannot 'Check_This' on a URL")
    } else if (chdir && (path <- file) != ".") {
        file <- "."


        owd <- getwd()
        if (is.null(owd))
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(path)
    }


    packageInfo <- read.dcf(file.path(file, "DESCRIPTION"))
    if (nrow(packageInfo) != 1L)
        stop("bruh wtf are you doing???")
    packageInfo <- structure(c(packageInfo), names = colnames(packageInfo))
    pkgname <- packageInfo[["Package"]]
    version <- packageInfo[["Version"]]
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
            warning(gettextf("%s %s was unloaded, please restart the R session",
                if (.Platform$OS.type == "windows") "DLL" else "shared object",
                sQuote(pkgname)))
    }, add = TRUE)



    if (.Platform$OS.type == "windows" &&
        isNamespaceLoaded(pkgname))
    {
        libpath <- getNamespaceInfo(pkgname, "path")
        for (chname in names(getNamespaceInfo(pkgname, "DLLs"))) {
            library.dynam.unload(chname, libpath)
            unloaded <- TRUE
        }
    }


    tar.file <- paste0(pkgname, "_", version, ".tar.gz")


    value <- if (is.null(dir)) {
        .Rcmd(command = "build", args = c(build.args, file), mustWork = TRUE)
    } else .Rcmd(command = "build", args = c(build.args, file), mustWork = TRUE, dir = dir)
    cat("\n")


    if (special) {
        fields <- c("Package", "Version", "Depends", "Suggests",
            "License", "MD5sum", "NeedsCompilation", "Imports",
            "LinkingTo", "Enhances", "OS_type")
        PACKAGES.info <- structure(packageInfo[fields], names = fields)
        if (is.na(PACKAGES.info["NeedsCompilation"]))
            PACKAGES.info["NeedsCompilation"] <- if (dir.exists(file.path(file, "src"))) "yes" else "no"
        PACKAGES.info["MD5sum"] <- tools::md5sum(tar.file)
        PACKAGES.info <- t(PACKAGES.info)


        tar.path <- file.path(file, where, "src", "contrib")
        dir.create(tar.path, showWarnings = FALSE, recursive = TRUE)


        PACKAGES.file <- file.path(tar.path, "PACKAGES")
        if (file.exists(PACKAGES.file)) {
            text <- readLines(PACKAGES.file)
            con <- file(PACKAGES.file, "w")
            # con <- stdout()
            tryCatch({
                if (i <- match(paste0("Package: ", pkgname), text, 0L)) {
                    writeLines(text[seq_len(i - 1L)], con)
                } else if (i <- match(TRUE, startsWith(text, "Package: ") & substr(text, 10L, 1000000L) > pkgname, 0L)) {
                    writeLines(text[seq_len(i - 1L)], con)
                    i <- i - 2L
                } else {
                    i <- length(text)
                    writeLines(c(text, ""), con)
                }
                write.dcf(PACKAGES.info, con)
                j <- which(text == "")
                j <- j[j > i]
                if (length(j) > 0) {
                    j <- j[[1L]]
                    writeLines(text[j:length(text)], con)
                }
            }, finally = close(con))
        } else write.dcf(PACKAGES.info, PACKAGES.file)


        archive.path <- file.path(tar.path, "Archive", pkgname)
        dir.create(archive.path, showWarnings = FALSE, recursive = TRUE)


        to <- file.path(tar.path, tar.file)
        if (!file.rename(tar.file, to))
            stop("failure to rename")


        files <- setdiff(list.files(tar.path), tar.file)
        files <- files[startsWith(files, paste0(pkgname, "_"))]
        new.files <- file.path(archive.path, files)
        files <- file.path(tar.path, files)
        if (!all(file.rename(files, new.files)))
            stop("failure to rename")


        tar.file <- to
    }


    value <- if (is.null(dir)) {
        .Rcmd(command = "INSTALL", args = c(INSTALL.args, tar.file), mustWork = TRUE)
    } else .Rcmd(command = "INSTALL", args = c(INSTALL.args, tar.file), mustWork = TRUE, dir = dir)
    cat("\n")
    finished <- TRUE


    if (check) {
        value <- if (is.null(dir))
            .Rcmd(command = "check", args = c(check.args, tar.file), mustWork = TRUE)
        else .Rcmd(command = "check", args = c(check.args, tar.file), mustWork = TRUE, dir = dir)
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

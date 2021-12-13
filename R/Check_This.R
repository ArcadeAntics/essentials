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


Check_This <- function (
    build.vignettes = TRUE, no.build.vignettes = !build.vignettes,
    manual = TRUE, no.manual = !manual,
    resave.data = FALSE, no.resave.data = FALSE,

    build = FALSE,
    multiarch = TRUE, no.multiarch = !multiarch,
    keep.source = NA,

    check = TRUE, as.cran = FALSE,

    chdir = FALSE, file = here())
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
    #     resave.data = FALSE, no.resave.data = FALSE,
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
    # no.resave.data
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



    build.args <- c(
        if (no.build.vignettes) "--no-build-vignettes",
        if (no.manual)          "--no-manual",
        switch2(resave.data, TRUE.expr = {
            "--resave-data"
        }, FALSE.expr = {
            if (no.resave.data)
                "--no-resave-data"
        }, alt.expr = {
            paste0("--resave-data=", match.arg(resave.data, c("no", "best", "gzip")))
        })
    )


    keep.source
    INSTALL.args <- c(
        if (build) "--build",
        if (no.multiarch) "--no-multiarch",
        tryCatch({
            if (keep.source)
                "--with-keep.source"
            else "--without-keep.source"
        }, error = function(c) NULL)
    )


    check <- if (check) TRUE else FALSE
    if (check)
        check.args <- c(
            if (as.cran) "--as-cran"
        )


    if (!is.character(file) || length(file) != 1L)
        stop("invalid 'file'")
    else if (grepl("^(ftp|ftps|http|https)://", file))
        stop("cannot 'Check_This' on a URL")
    else if (chdir && (path <- dirname(file)) != ".") {
        # file <- normalizePath(file)
        file <- basename(file)


        owd <- getwd()
        if (is.null(owd))
            stop("cannot 'chdir' as current directory is unknown")
        on.exit(setwd(owd))
        setwd(path)
    }


    packageInfo <- read.dcf(file.path(file, "DESCRIPTION"),
        fields = c("Package", "Version"))
    if (anyNA(packageInfo) ||
        !grepl(packageInfo[[1L, "Package"]], pattern = paste0("^(", .standard_regexps()$valid_package_name   , ")$")) ||
        !grepl(packageInfo[[1L, "Version"]], pattern = paste0("^(", .standard_regexps()$valid_package_version, ")$")))
        stop("invalid package DESCRIPTION file")


    pkgname <- packageInfo[[1L, "Package"]]
    if (isNamespaceLoaded(pkgname)) {
        DLLs <- names(getLoadedDLLs())
        if (pkgname %in% DLLs) {
            libpath <- getNamespaceInfo(pkgname, "path")
            library.dynam.unload(pkgname, libpath)
        }
    }


    file2 <- paste0(file, "_", packageInfo[[1L, "Version"]], ".tar.gz")


    value <- Rcmd(command = "build", args = c(build.args, file),
        mustWork = TRUE)
    cat("\n")


    value <- Rcmd(command = "INSTALL", args = c(INSTALL.args, file2),
        mustWork = TRUE)
    cat("\n")


    if (check) {
        value <- Rcmd(command = "check", args = c(check.args, file2),
            mustWork = TRUE)
        cat("\n")
    }
    if (.Platform$GUI == "RStudio" &&
        exists(".rs.api.restartSession", "tools:rstudio", inherits = FALSE)) {


        get(".rs.api.restartSession", "tools:rstudio", inherits = FALSE)(
            command = deparse1(call("library", as.symbol(pkgname)),
                collapse = "\n", width.cutoff = 80L)
        )
    }
    else invisible(value)
}

.onLoad <- evalq(envir = new.env(), function (libname, pkgname)
{
    # cat("libname = ", sQuote(libname), "\n", sep = "")
    # cat("pkgname = ", sQuote(pkgname), "\n", sep = "")
    nm <- "R_ESSENTIALS_SUPPRESSPACKAGESTARTUPMESSAGES"
    if (Sys.getenv(nm, "FALSE")) {
    } else if (getOption(nm, FALSE)) {
    } else packageStartupMessage(paste(strwrap(sprintf("the string formatting/interpolation for essentials::ArgumentParser was changed with the addition of essentials::f.str\n\nplease change variables from %%(DEFAULT) to %%(DEFAULT)s or %%(METAVARIABLE) to %%(METAVARIABLE)s or any similar such uses\n\nsee ?essentials::f.str for details and examples\n\nturn this message permanently off by setting environment variable or R option \"%s\" to TRUE",
        nm)), collapse = "\n"))
    if (is.na(otoplevel <<- Sys.getenv("R_ESSENTIALS_TOP_LEVEL", NA)))
        Sys.setenv(R_ESSENTIALS_TOP_LEVEL = TRUE)
    else if (otoplevel)
        Sys.setenv(R_ESSENTIALS_TOP_LEVEL = FALSE)
})
evalq(envir = environment(.onLoad), {
    otoplevel <- NULL
})


.onUnload <- evalq(envir = environment(.onLoad), function (libpath)
{
    # cat("libpath = ", sQuote(libpath), "\n", sep = "")
    library.dynam.unload(.packageName, libpath)
    if (is.na(otoplevel))
        Sys.unsetenv("R_ESSENTIALS_TOP_LEVEL")
    else if (otoplevel)
        Sys.setenv(R_ESSENTIALS_TOP_LEVEL = TRUE)
})

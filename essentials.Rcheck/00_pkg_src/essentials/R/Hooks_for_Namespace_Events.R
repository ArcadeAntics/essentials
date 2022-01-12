.onLoad <- evalq(envir = new.env(), function (libname, pkgname)
{
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
    print(libpath)
    library.dynam.unload(.packageName, libpath)
    if (is.na(otoplevel))
        Sys.unsetenv("R_ESSENTIALS_TOP_LEVEL")
    else if (otoplevel)
        Sys.setenv(R_ESSENTIALS_TOP_LEVEL = TRUE)
})

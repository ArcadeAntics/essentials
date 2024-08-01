.getProgressBarMethod <- evalq(envir = new.env(), {
    progressBars <- structure(list(), names = character())
                         function (name = NULL)
{
    name <- match.arg(name, names(progressBars))
    progressBars[[name]]
}
})


setProgressBarMethod <- function (name, definition)
{
    name <- as.character(name)[[1L]]
    definition <- match.fun(definition)
    e <- environment(.getProgressBarMethod)
    e$progressBars[[name]] <- definition
    invisible()
}





.txt_progress_bar_imports_env <- new.env()
evalq(envir = .txt_progress_bar_imports_env, {
    delayedAssign("getTxtProgressBar", utils::getTxtProgressBar)
    delayedAssign("setTxtProgressBar", utils::setTxtProgressBar)
})
lockEnvironment(.txt_progress_bar_imports_env, bindings = TRUE)


setProgressBarMethod("txt",
function (title = NULL, label = NULL, min = 0, max = 1, initial = 0,
    char = "=", width = NA, style = 1, file = "", ...)
{
    e <- new.env(parent = .txt_progress_bar_imports_env)
    evalq(envir = e, {
        get <- function() {
            getTxtProgressBar(pb)
        }
        set <- function(value, title = NULL, label = NULL, ...) {
            setTxtProgressBar(pb, value)
        }
        increment <- function(value = 1, title = NULL, label = NULL, ...) {
            setTxtProgressBar(pb, getTxtProgressBar(pb) + value)
        }
        close <- function() {
            close <- base::close
            close(pb)
        }
    })
    if (!is.null(title))
        cat("   ", title, "\n", file = file, sep = "")
    pb <- utils::txtProgressBar(min = min, max = max, initial = initial,
        char = char, width = width, title = title, label = label,
        style = style, file = file)
    e$pb <- pb
    class(e) <- "progressBar"
    e
}
)





.win_progress_bar_imports_env <- new.env()
evalq(envir = .win_progress_bar_imports_env, {
    delayedAssign("getWinProgressBar", utils::getWinProgressBar)
    delayedAssign("setWinProgressBar", utils::setWinProgressBar)
})
lockEnvironment(.win_progress_bar_imports_env, bindings = TRUE)


setProgressBarMethod("win",
function (title = "R progress bar", label = "", min = 0, max = 1,
    initial = 0, width = 300L, ...)
{
    if (.Platform$OS.type != "windows")
        stop(gettext("'winProgressBar' is a Windows exclusive"))
    e <- new.env(parent = .win_progress_bar_imports_env)
    evalq(envir = e, {
        get <- function() {
            getWinProgressBar(pb)
        }
        set <- function(value, title = NULL, label = NULL, ...) {
            setWinProgressBar(pb, value, title, label)
        }
        increment <- function(value = 1, title = NULL, label = NULL, ...) {
            setWinProgressBar(pb, getWinProgressBar(pb) + value, title, label)
        }
        close <- function() {
            close <- base::close
            close(pb)
        }
    })
    pb <- utils::winProgressBar(title = title, label = label,
        min = min, max = max, initial = initial, width = width)
    e$pb <- pb
    class(e) <- "progressBar"
    e
}
)





.tk_progress_bar_imports_env <- new.env()
evalq(envir = .tk_progress_bar_imports_env, {
    delayedAssign("getTkProgressBar", tcltk::getTkProgressBar)
    delayedAssign("setTkProgressBar", tcltk::setTkProgressBar)
})
lockEnvironment(.tk_progress_bar_imports_env, bindings = TRUE)


setProgressBarMethod("tk",
function(title = "R progress bar", label = "", min = 0, max = 1,
    initial = 0, width = 445L, ...)
{
    e <- new.env(parent = .tk_progress_bar_imports_env)
    evalq(envir = e, {
        get <- function() {
            getTkProgressBar(pb)
        }
        set <- function(value, title = NULL, label = NULL, ...) {
            setTkProgressBar(pb, value, title, label)
        }
        increment <- function(value = 1, title = NULL, label = NULL, ...) {
            setTkProgressBar(pb, getTkProgressBar(pb) + value, title, label)
        }
        close <- function() {
            close <- base::close
            close(pb)
        }
    })
    pb <- tcltk::tkProgressBar(title = title, label = label,
        min = min, max = max, initial = initial, width = width)
    e$pb <- pb
    class(e) <- "progressBar"
    e
}
)





progressBar <- function (name = NULL, ...)
{
    makeProgressBar <- .getProgressBarMethod(name)
    if (!is.function(makeProgressBar)) {
        stop(simpleError(
            gettextf("could not find function \"%s\"", "makeProgressBar", domain = "R"),
            quote(makeProgressBar(...))
        ))
    }
    makeProgressBar(...)
}





close.progressBar <- function (con, ...)
{
    con$close()
}

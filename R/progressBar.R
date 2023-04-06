setProgressBarMethod <- function (name, definition)
{
    name <- as.character(as.symbol(name))
    definition <- match.fun(definition)
    progressBars[[name]] <<- definition
    invisible()
}
evalq(envir = environment(setProgressBarMethod) <- new.env(), {
    progressBars <- list()
})


setProgressBarMethod("tk",
function(title = "R progress bar", label = "", min = 0,
    max = 1, initial = 0, width = 445L, ...)
{
    tcltk::tkProgressBar(title = title, label = label, min = min,
        max = max, initial = initial, width = width)
})


setProgressBarMethod("txt",
function(min = 0, max = 1, initial = 0, char = "=", width = NA,
    title, label, style = 1, file = "", ...)
{
    utils::txtProgressBar(min = min, max = max, initial = initial,
        char = char, width = width, title = title, label = label,
        style = style, file = file)
})


setProgressBarMethod("win",
function(title = "R progress bar", label = "", min = 0,
    max = 1, initial = 0, width = 300L, ...)
{
    if (.Platform$OS.type != "windows")
        stop(gettext("'winProgressBar' is a Windows exclusive"))
    utils::winProgressBar(title = title, label = label, min = min,
        max = max, initial = initial, width = width)
})


progressBar <- function (name = names(progressBars), ...)
progressBars[[match.arg(name)]](...)
environment(progressBar) <- environment(setProgressBarMethod)


getProgress <- function (pb)
UseMethod("getProgress")


getProgress.tkProgressBar <- function (pb)
tcltk::getTkProgressBar(pb)


getProgress.txtProgressBar <- function (pb)
utils::getTxtProgressBar(pb)


getProgress.winProgressBar <- function (pb)
utils::getWinProgressBar(pb)


setProgress <- function (pb, ...)
UseMethod("setProgress")


setProgress.tkProgressBar <- function (pb, value, title = NULL, label = NULL, ...)
tcltk::setTkProgressBar(pb = pb, value = value, title = title, label = label)


setProgress.txtProgressBar <- function (pb, value, ...)
utils::setTxtProgressBar(pb = pb, value = value)


setProgress.winProgressBar <- function (pb, value, title = NULL, label = NULL, ...)
utils::setWinProgressBar(pb = pb, value = value, title = title, label = label)


increment <- function (x, ...)
UseMethod("increment")


increment.default <- function (x, ...)
setProgress(pb = x, value = getProgress(x) + 1)


increment.tkProgressBar <- function (x, title = NULL, label = NULL, ...)
tcltk::setTkProgressBar(pb = x, value = tcltk::getTkProgressBar(x) + 1, title = title, label = label)


increment.txtProgressBar <- function (x, ...)
utils::setTxtProgressBar(pb = x, value = utils::getTxtProgressBar(x) + 1)


increment.winProgressBar <- function (x, title = NULL, label = NULL, ...)
utils::setWinProgressBar(pb = x, value = utils::getWinProgressBar(x) + 1, title = title, label = label)


decrement <- function (x, ...)
UseMethod("decrement")


decrement.default <- function (x, ...)
setProgress(pb = x, value = getProgress(x) - 1)


decrement.tkProgressBar <- function (x, ...)
tcltk::setTkProgressBar(pb = x, value = tcltk::getTkProgressBar(x) - 1)


decrement.txtProgressBar <- function (x, ...)
utils::setTxtProgressBar(pb = x, value = utils::getTxtProgressBar(x) - 1)


decrement.winProgressBar <- function (x, ...)
utils::setWinProgressBar(pb = x, value = utils::getWinProgressBar(x) - 1)

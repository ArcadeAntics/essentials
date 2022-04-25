.shPrompt <- evalq(envir = new.env(), function (type, arg.name, option.name, env.name)
{
    match.type <- function(type, ...) {
        if (is.character(type)) {
            if (length(type) > 1L)
                warning("first element used of ", ...)
            type <- pmatch(tolower(type[1L]), table)
            if (!is.na(type)) types[[type]]
        }
    }
    type <- match.type(type, "'", arg.name, "' argument")
    if (is.null(type)) {
        type <- match.type(getOption(option.name), "'", option.name, "' option")
        if (is.null(type)) {
            type <- match.type(Sys.getenv(env.name), "'", env.name, "' environment variable")
            if (is.null(type)) {
                if (.Platform$OS.type == "windows") {  # on Windows
                    type <- switch(basename(Sys.getenv("COMSPEC")),
                        cmd.exe = "cmd",
                        powershell.exe = "powershell",
                        "windows"
                    )
                }
                else if (on.macOS) {                   # on macOS (see ?capabilities)
                    type <- "macOS"
                }
                else type <- "ubuntu"                  # on any other Linux flavour
            }
        }
    }


    wd <- getwd()
    switch(type, windows = , cmd = {            # Windows command prompt


        if (!is.null(wd)) {
            if (.Platform$OS.type == "windows")
                wd <- chartr("/", "\\", wd)
        }
        else wd <- "NULL"
        paste0(wd, ">")


    }, powershell = {


        if (!is.null(wd)) {
            if (.Platform$OS.type == "windows")
                wd <- chartr("/", "\\", wd)
            if (grepl("^[/\\\\]{2}", wd))  # network file
                wd <- paste0("Microsoft.Powershell.Core\\FileSystem::", wd)
        }
        else wd <- "NULL"
        paste0("PS ", wd, "> ")


    }, macOS = {                                # macOS command prompt


        if (!is.null(wd)) {
            wd <- path.contract(wd)
            if (wd != "~")
                wd <- basename(wd)
        }
        else wd <- "NULL"
        sys.info <- Sys.info()
        nodename <- if (on.macOS)
            sub("\\.(lan|local)$", "", sys.info[["nodename"]])
        else sys.info[["nodename"]]
        paste0(nodename, ":", wd, " ", sys.info[["effective_user"]], "$ ")


    }, bash = , ubuntu = , unix = {             # bash command prompt


        if (!is.null(wd)) {
            wd <- path.contract(wd)
            if (.Platform$OS.type == "windows") {
                wd <- chartr("\\", "/", wd)
                if (grepl(pattern <- paste0("^", normalizePath("/", "/", FALSE)),
                    wd, ignore.case = TRUE))
                    wd <- sub(pattern, "/", wd, ignore.case = TRUE)
            }
        }
        else wd <- "NULL"
        sys.info <- Sys.info()
        nodename <- if (on.macOS)
            sub("\\.(lan|local)$", "", sys.info[["nodename"]])
        else sys.info[["nodename"]]
        x <- c("\033[38;5;70m", "\033[1m",
        #       1~~~~~~~~~~~~    2~~~~~~

            sys.info[["effective_user"]], "@", nodename,

            "\033[22m", "\033[39m",
        #    3~~~~~~~    4~~~~~~~

            ":",

            "\033[38;5;67m", "\033[1m",
        #    5~~~~~~~~~~~~    6~~~~~~

            wd,

            "\033[22m", "\033[39m",
        #    7~~~~~~~    8~~~~~~~

            "$ ")

        # 1
        #     start using colour green, other acceptable colours include:
        #     \033[38;5;112m
        #     \033[38;5;34m
        # 2
        #     start using boldface
        # 3
        #     stop using boldface
        # 4
        #     stop using colour green
        #
        # 5
        #     start using colour blue, other acceptable colours include:
        #     \033[38;5;68m
        #     \033[38;5;25m
        #     \033[38;5;24m
        # 6
        #     start using boldface
        # 7
        #     stop using boldface
        # 8
        #     stop using colour blue


        if (!supports.8.bit.color())
            paste(x[-c(1L, 2L, 6L, 7L, 9L, 10L, 12L, 13L)], collapse = "")
        else paste(x, collapse = "")


    }, stop("invalid 'type'; should never happen, please report!"))
})
evalq(envir = environment(.shPrompt), {
    types <- c("windows", "cmd", "powershell", "macOS", "bash", "ubuntu", "unix")
    table <- tolower(types)
    delayedAssign("on.macOS", capabilities("aqua"))
})


shPrompt <- function (type = NULL)
.shPrompt(type = type, arg.name = "type", option.name = "essentials::shPrompt(type)",
    env.name = "R_ESSENTIALS_SH_PROMPT_TYPE")

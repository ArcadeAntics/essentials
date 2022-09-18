# # library(this.path)
# # suppressPackageStartupMessages(library(essentials))
#
#
#
#
#
# strequal <- function (x, y)
# {
#     x <- as.scalar.character(x)
#     y <- as.scalar.character(y)
#     if (is.na(x)) {
#         is.na(y)
#     }
#     else if (is.na(y))
#         FALSE
#     else x == y
# }
#
#
# strcaseequal <- function (x, y)
# {
#     x <- as.scalar.character(x)
#     y <- as.scalar.character(y)
#     if (is.na(x)) {
#         is.na(y)
#     }
#     else if (is.na(y))
#         FALSE
#     else tolower(x) == tolower(y)
# }
#
#
# jswitch <- function (...)
# {
#     EXPR <- as.scalar(..1)
#     if (!is.character(EXPR)) {
#         iEXPR <- as.integer(EXPR)
#         if (EXPR == iEXPR)
#             EXPR <- iEXPR
#         else stop("invalid EXPR, not an integer or string")
#     }
#     if (...length() <= 1L)
#         return(invisible())
#     cases <- as.list(substitute(list(...)))[-(1:2)]
#     tmp <- cases[[length(cases)]]
#     if (has.default <- is.call(tmp) && length(tmp) == 3L && is.symbol(tmp[[1L]]) && tmp[[1L]] == ":=" && is.symbol(tmp[[2L]]) && tmp[[2L]] == "default") {
#         default <- cases[[length(cases)]][[3L]]
#         cases <- cases[-length(cases)]
#     }
#     cases <- lapply(cases, function(e) {
#         if (!(is.call(e) &&
#               is.symbol(e[[1L]])))
#             stop("invalid case call ", capture.output(e))
#         if (e[[1L]] != ":=") {
#             case <- e
#             has.expr <- FALSE
#         }
#         else {
#             if (length(e) != 3L)
#                 stop("invalid case call ", capture.output(e))
#             case <- e[[2L]]
#             expr <- e[[3L]]
#             has.expr <- TRUE
#         }
#         if (!(is.call(case) && is.symbol(case[[1L]])))
#             stop("invalid case call ", capture.output(case))
#         call("if",
#
#
#              call("||", as.symbol("cont"),
#                 if (case[[1L]] == "case" && length(case) == 2L)
#                     call("strequal", EXPR, case[[2L]])
#                 else if (case[[1L]] == "icase" && length(case) == 2L)
#                     call("strcaseequal", EXPR, case[[2L]])
#                 else if (case[[1L]] == "recase" && length(case) >= 2L)
#                     as.call(c(as.symbol("grepl"), x = EXPR, as.list(case)[-1L]))
#                 else stop("invalid case call ", capture.output(case))),
#
#
#             as.call(c(as.symbol("{"), quote(cont <- TRUE), if (has.expr) expr)))
#     })
#     expr <- call("{",
#         quote(cont <- FALSE),
#         quote(used_next <- TRUE),
#         quote(skip <- TRUE),
#         call("repeat", as.call(c(as.symbol("{"),
#             quote(indx <- indx + 1L),
#             quote(if (indx > 0L) {
#                 if (used_next)
#                     stop("can't use next within jswitch", call. = FALSE)
#                 else break
#             }),
#             cases,
#             if (has.default) list(default),
#             quote(used_next <- FALSE)))))
#     # return(expr)
#     eval(expr, parent.frame())
# }
#
#
#
#
#
# # jswitch("this string contains TEST3 somewhere inside",
# # case("test1") := {
# #     writeLines("\nin \"test1\"\n")
# # },
# # icase("test2") := {
# #     writeLines("\nin \"test2\"\n")
# # },
# # recase("test3", ignore.case = TRUE) := {
# #     writeLines("\nin \"test3\"\n")
# # },
# # default := {
# #     writeLines("\nin default\n")
# # })
#
#
#
#
#
# # jswitch(fileArgs()[[1L]],
# # case("foo"),
# # case("bar") := {
# #     writeLines("\n\"foo\" or \"bar\" (case sensitive)\n")
# #     break
# # },
# # icase("pi") := {
# #     writeLines("\n\"pi\" or \"Pi\" or \"pI\" or \"PI\" (case insensitive)\n")
# #     break
# # },
# # recase("^D") := {
# #     writeLines("\nSomething that starts with \"D\" (case sensitive)\n")
# #     break
# # },
# # recase("^E", ignore.case = TRUE) := {
# #     writeLines("\nSomething that starts with \"E\" (case insensitive)\n")
# #     break
# # },
# # case("1") := {
# #     writeLines("\n\"1\"\n")
# #     # break omitted on purpose
# # },
# # case("2") := {
# #     writeLines("\n\"2\" (or \"1\")\n")
# #     break
# # },
# # default := {
# #     writeLines("\ndefault\n")
# #     next
# #     break
# # })
#
#
#
#
#
# if (FALSE) {
#     Rscript(file = this.path(), args = "foo")
#     Rscript(file = this.path(), args = "FOO")
#     Rscript(file = this.path(), args = "bar")
#     Rscript(file = this.path(), args = "BAR")
#     Rscript(file = this.path(), args = "pi")
#     Rscript(file = this.path(), args = "Pi")
#     Rscript(file = this.path(), args = "pI")
#     Rscript(file = this.path(), args = "PI")
#     Rscript(file = this.path(), args = "Dentist")
#     Rscript(file = this.path(), args = "dentist")
#     Rscript(file = this.path(), args = "Entymology")
#     Rscript(file = this.path(), args = "entymology")
#     Rscript(file = this.path(), args = "1")
#     Rscript(file = this.path(), args = "2")
#     Rscript(file = this.path(), args = "none of the above")
# }


intequal <- function (e1, e2)
.Call(C_intequal, e1, e2)


strequal <- function (e1, e2)
.Call(C_strequal, e1, e2)


strcaseequal <- function (e1, e2)
.Call(C_strcaseequal, e1, e2)


jswitch <- function (...)
.External2(C_jswitch)

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
.External2(C_intequal, e1, e2)


strequal <- function (e1, e2)
.External2(C_strequal, e1, e2)


strcaseequal <- function (e1, e2)
.External2(C_strcaseequal, e1, e2)


jswitch <- function (...)
.External2(C_jswitch)

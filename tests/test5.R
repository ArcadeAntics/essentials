# parser <- essentials::ArgumentParser(
#     description = essentials::dedent("
#         Process some integers
#     "))
# parser$add.argument("integers", action = "append", nargs = "+",
#     type = "integer", metavariable = "N",
#     help = "An integer for the accumulator")
# parser$add.argument("--sum", action = "store_const",
#     default = base::max, constant = base::sum,
#     help = "Sum the integers (default: find the max)",
#     destination = "accumulate")
# pargs <- parser$parse.args()
# print(pargs$accumulate(pargs$integers))

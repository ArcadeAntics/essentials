parser <- essentials::ArgumentParser(
    description = "A description for this script",
    epilogue = "   bonkque ---- testing ----------- without wrapping", wrap.epilogue = FALSE
)


# parser$add.argument("testing", help = "testing positional arguments")
# parser$add.argument("testing2", help = "testing positional arguments again")


parser$add.argument("-a", action = "store_true", help = "Help message for argument '%(SHORTFLAG)' (default: %(DEFAULT))")
parser$add.argument("--beta", action = "store_false", help = "Help message for argument '%(LONGFLAG)'")
parser$add.argument("-d", "--delta", action = "count", help = "Help message for argument '%(LONGFLAG)'")


parser$add.argument("-f", help = "Take input from '%(METAVARIABLE)'", metavariable = "FILE")
parser$add.argument("--encoding", help = "Specify encoding to be used for stdin", metavariable = "enc")
parser$add.argument("-n", "--number", action = "append", type = "numeric", help = "\n  A bunch of\nnumbers to sum \n  ", metavariable = "N")


parser$add.argument("-e", action = "append", type = "expression", help = "Any syntactically valid R expression", metavariable = "EXPR")


parser$add.argument(
    "--with-keep.source",
    action = "store_true",
    help = "Use 'keep.source' for R code",
    destination = "keep.source"
)
parser$add.argument(
    "--without-keep.source",
    action = "store_false",
    help = "Do not use 'keep.source' for R code",
    destination = "keep.source"
)


parser$print.help()
parser$parse.args()

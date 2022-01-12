parser <- essentials::ArgumentParser()


parser$add.version(exit = utils::packageVersion("essentials"))
parser$add.argument("--alpha")
parser$add.skip()
parser$add.argument("--beta")


pargs <- parser$parse.args(
    c("--alpha", "testing", "--args", "--beta", "more")
)

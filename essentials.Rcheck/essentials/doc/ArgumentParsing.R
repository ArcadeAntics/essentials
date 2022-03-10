### R code from vignette source 'ArgumentParsing.Rnw'

###################################################
### code chunk number 1: ArgumentParsing.Rnw:21-23
###################################################
parser <- essentials::ArgumentParser(
    description = "Produce a plot")


###################################################
### code chunk number 2: ArgumentParsing.Rnw:28-31
###################################################
parser$add.argument("x", nargs = "+", type = "numeric",
    help = "Numbers to plot, provided to argument 'x'",
    metavariable = "N")


###################################################
### code chunk number 3: ArgumentParsing.Rnw:38-40
###################################################
pargs <- parser$parse.args(
    args = c("1", "2", "4", "7", "10", "3.1415926535897931"))


###################################################
### code chunk number 4: pargsx (eval = FALSE)
###################################################
## graphics::plot(x = pargs$x)


###################################################
### code chunk number 5: ArgumentParsing.Rnw:51-52
###################################################
graphics::plot(x = pargs$x)


###################################################
### code chunk number 6: ArgumentParsing.Rnw:61-64
###################################################
parser <- essentials::ArgumentParser()
parser$add.argument("pos-name1", "pos.name2")
# one or more variable names


###################################################
### code chunk number 7: ArgumentParsing.Rnw:69-71
###################################################
parser$add.argument("-a", "--flag_name")
# one of more variable names starting with one or two hyphens


###################################################
### code chunk number 8: ArgumentParsing.Rnw:76-81
###################################################
# any of these are acceptable
parser$parse.args(c("value1", "-a", "value2"))
parser$parse.args(c("value1", "-a=value2"))
parser$parse.args(c("value1", "--flag_name=value2"))
parser$parse.args(c("value1", "--flag_name", "value2"))


###################################################
### code chunk number 9: ArgumentParsing.Rnw:95-107
###################################################
parser <- essentials::ArgumentParser()
parser$add.argument("-a", action = "store")
parser$add.argument("-b", action = "append")
parser$add.argument("-c", action = "store_const",
    default = "not provided", constant = "provided")
parser$add.argument("-d", action = "store_false")
parser$add.argument("-e", action = "store_true")
parser$add.argument("-f", action = "count")
parser$parse.args(
    c("-a", "1value", "-b", "1st value", "-b", "2nd value", "-ff"))
parser$parse.args(
    c("-c", "-d", "-e", "-fff"))


###################################################
### code chunk number 10: ArgumentParsing.Rnw:118-126
###################################################
parser <- essentials::ArgumentParser()
parser$add.argument("--arg1", choices = c("choice1", "option2", "string3"))
parser$add.argument("--arg2", choices = 1:10, type = "integer")
parser$parse.args(c("--arg1=option2", "--arg2=7"))
parser$parse.args("--arg1=s")  # strings may be abbreviated
tryCatch({
    parser$parse.args(c("--arg2", "-8"))
}, condition = print)



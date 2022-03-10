`%until%` <- essentials::`%until%`
`%while%` <- essentials::`%while%`


# do while/until loops should respect 'return' statements
# just like for, while, and repeat loops do
stopifnot(
    (function() {
        do(return(TRUE)) %until% (TRUE)
        return(FALSE)
    })(),
    (function() {
        do(return(TRUE)) %while% (FALSE)
        return(FALSE)
    })()
)





# in a previous version, this would have failed because control would be
# transferred back to 'expr' instead of to 'cond', creating an infinite loop
#
# in this version, it should eval 'next', transfer control to 'cond' and then
# finish the loop
essentials::Rscript(
    options = c("--default-packages=essentials", "--vanilla"),
    exprs = "do(next) %until% (TRUE)",
    timeout = 1, mustWork = TRUE, quiet = TRUE
)

pkgname <- "essentials"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "essentials-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('essentials')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ASCII")
### * ASCII

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ASCII
### Title: ASCII Characters
### Aliases: ASCII

### ** Examples

ASCII()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ASCII", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Args")
### * Args

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Args
### Title: Extract Arguments Supplied to a Script / / From an Object of
###   Class "ParsedArgs"
### Aliases: Args

### ** Examples

essentials:::write.code(file = FILE <- tempfile(), withAutoprint({


    # the regular command-line arguments
    commandArgs()


    # the script arguments
    essentials::Args()


}, verbose = FALSE))


essentials::Rscript("--default-packages=NULL", FILE, args = pi)
essentials::withArgs(source(FILE, verbose = FALSE), pi)


unlink(FILE)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Args", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ArgumentParser")
### * ArgumentParser

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ArgumentParser
### Title: Argument Parser
### Aliases: ArgumentParser ArgumentParser-class
###   essentials_ArgumentParser-class

### ** Examples

x <- essentials::ArgumentParser()
x$print.help()


y <- essentials::ArgumentParser(description = "A description for the program",
    epilogue = c("  --- Final Message ---   ",
        "A final message for the program, do not wrap this message"),
    wrap.epilogue = FALSE)
y$print.help()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ArgumentParser", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Commands")
### * Commands

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Commands
### Title: Extract Commands From an Object of Class "ParsedArgs"
### Aliases: Commands

### ** Examples

parser <- essentials::ArgumentParser()
`parser CMD1` <- parser$add.parser("CMD1")
`parser CMD2` <- parser$add.parser("CMD2")
`parser CMD1 a` <- `parser CMD1`$add.parser(c("a", "b"))
`parser CMD1 c` <- `parser CMD1`$add.parser(c("c", "d"))
`parser CMD2 e` <- `parser CMD2`$add.parser(c("e", "f"))
`parser CMD2 g` <- `parser CMD2`$add.parser(c("g", "h"))


essentials::Commands(parser$parse.args())
essentials::Commands(parser$parse.args(c("CMD1")))
essentials::Commands(parser$parse.args(c("CMD2")))
essentials::Commands(parser$parse.args(c("CMD1", "a")))
essentials::Commands(parser$parse.args(c("CMD1", "b")))
essentials::Commands(parser$parse.args(c("CMD1", "c")))
essentials::Commands(parser$parse.args(c("CMD1", "d")))
essentials::Commands(parser$parse.args(c("CMD2", "e")))
essentials::Commands(parser$parse.args(c("CMD2", "f")))
essentials::Commands(parser$parse.args(c("CMD2", "g")))
essentials::Commands(parser$parse.args(c("CMD2", "h")))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Commands", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("GeneralizedExtremeValue")
### * GeneralizedExtremeValue

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: GeneralizedExtremeValue
### Title: The Generalized Extreme Value Distribution
### Aliases: GeneralizedExtremeValue dgev pgev qgev rgev

### ** Examples

shapes <- expression(-1/2, 0, +1/2)
legend.text <- as.expression(lapply(shapes, function(shape) {
    call("==", as.symbol("xi"), shape)
}))
shapes <- vapply(shapes, base::eval, 0)
cols <- c("green3", "red", "blue")
x <- seq.int(-4, 4, length.out = 1001)


# we use plapply here instead of lapply because
# plapply allows us to name the looping arguments
y <- essentials::plapply(
    list(shape = shapes),
    essentials::dgev,
    x = x
)
graphics::par(mar = c(4.9, 4.5, 2.1, 0.4))
graphics::plot(
    xlim = range(x), ylim = range(y),
    panel.first = graphics::grid(col = "gray69"),
    x = NA_real_, y = NA_real_,
    xlab = "x", ylab = ~f(list(x, mu, sigma, xi)),
    main = "Probability density function",
    bty = "L"
)
for (i in seq_along(y)) {
    graphics::lines(x, y[[i]], col = cols[[i]], lwd = 2)
}
graphics::legend(
    x = "topleft",
    legend = legend.text,
    col = cols,
    lwd = 2,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ list(mu == 0, sigma == 1), adj = 1)





y <- essentials::plapply(
    list(shape = shapes),
    essentials::pgev,
    q = x
)
graphics::plot(
    xlim = range(x), ylim = range(y),
    panel.first = graphics::grid(col = "gray69"),
    x = NA_real_, y = NA_real_,
    xlab = "x", ylab = ~F(list(x, mu, sigma, xi)),
    main = "Cumulative probability function",
    bty = "L"
)
for (i in seq_along(y)) {
    graphics::lines(x, y[[i]], col = cols[[i]], lwd = 2)
}
graphics::legend(
    x = "topleft",
    legend = legend.text,
    col = cols,
    lwd = 2,
    bty = "n"
)
graphics::title(sub = ~"All with" ~ list(mu == 0, sigma == 1), adj = 1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("GeneralizedExtremeValue", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("IDW")
### * IDW

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: InverseDistanceWeighting
### Title: Inverse Distance Weighting
### Aliases: InverseDistanceWeighting IDW

### ** Examples

x0 <- c(0, 1, 4, 5)
u0 <- c(1, 2, 2, 1)
x <- seq.int(-4, 9, length.out = 1001)
u <- IDW(x0, u0, x)
graphics::plot(
    panel.first = graphics::grid(col = "gray69"),
    x, u, type = "l", col = "blue", lwd = 2
)
graphics::points(x0, u0, pch = 16, cex = 1.5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IDW", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Missing")
### * Missing

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Missing
### Title: Does an Argument have a Value?
### Aliases: Missing

### ** Examples

parser <- essentials::ArgumentParser()
parser$add.argument("--arg1")
parser$add.argument("--arg2", default = "def")
pargs <- parser$parse.args()


list(
    arg1 = essentials::Missing(pargs, arg1),
    arg2 = essentials::Missing(pargs, "arg2")
)


# with R >= 4.1.0, use the forward pipe operator `|>` to
# make calls to `Missing` more intuitive:
# list(
#     arg1 = pargs |> essentials::Missing(arg1),
#     arg2 = pargs |> essentials::Missing("arg2")
# )



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Missing", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("R")
### * R

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: R
### Title: Start a New R Session
### Aliases: R Rcmd Rscript Rterm

### ** Examples

# if you're on Windows, you should notice that the quoting rules for 'Rterm' are
# far more intuituve than for 'R'
#
# if you're under a Unix-alike, you should notice that 'Rterm' and 'R' do the
# same thing


essentials::R      (exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rterm  (exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}")
essentials::Rscript(exprs = r"{cat(commandArgs(), sep = "\n")}", quiet = TRUE)


essentials::Rcmd(command = "INSTALL", args = "--help")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("R", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Runge-KuttaMethods")
### * Runge-KuttaMethods

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Runge-Kutta Methods
### Title: Runge-Kutta Methods
### Aliases: 'Runge-Kutta Methods' EulerMethod RK1 ImprovedEulerMethod RK2
###   RungeKuttaMethod RK4

### ** Examples

# Consider a particle moving in a bowl
# We'll use cylindrical coordinates r, theta, and z
#     r     = radial position (distance from z-axis)
#     theta = angular position
#     z     = height
# The height of the bowl is z = r
# The independent variable (in this case) is time (denoted t)
# After some calculations, the differential equations obtained are
#     d/dt(r^2 * d/dt(theta)) = 0
#     d^2/dt^2(r) = r/2 * (d/dt(theta))^2 - g/2
# where g is the acceleration due to gravity
# This gives us three equations as follows
#     d/dt(r)     = f[r]     = rdot
#     d/dt(rdot)  = f[rdot]  = (r * (d/dt(theta))^2 - g)/2
#     d/dt(theta) = f[theta] = r0^2 * thetadot0 / r^2
# where
#     rdot      = d/dt(r)
#     r0        = initial value of radial  position
#     rdot0     = initial value of radial  velocity
#     theta0    = initial value of angular position
#     thetadot0 = initial value of angular velocity
# With all of this information, we can define
#     a sequence of time values
#     our initial conditions
#     derivatives of the dependents with respect to the independent


g <- 1
r0 <- 1           # 1 away from the center
rdot0 <- 0        # not moving radially
theta0 <- 0       # in the positive x-direction
thetadot0 <- 0.5  # spinning counter-clockwise with angular speed 0.5


k <- r0^2 * thetadot0
thetadot <- function(r) k/r^2


independent <- list(t = seq.int(0, 21.6, 0.001))
initialConditions <- c(r = r0, rdot = rdot0, theta = theta0)
fun <- function(independent, dependents) {
    r <- dependents[1L]
    thetadot <- k/r^2
    c(dependents[2L], (r * thetadot^2 - g)/2, thetadot)
}


# finally, simply call
value <- essentials::EulerMethod(independent, initialConditions, fun)


x <- value$r * cos(value$theta)
y <- value$r * sin(value$theta)
xylim <- c(-1, 1) * max(abs(c(x, y)))


graphics::par(mar = c(5.1, 4.1, 0.4, 0.4))
graphics::plot(
    xlim = xylim, ylim = xylim, asp = 1,
    panel.first = grid(col = "gray69"),
    x = x, y = y,
    col = grDevices::hcl.colors(length(x)),
    pch = 16, cex = 0.5,
    bty = "n",
    xlab = "x", ylab = "y"
)
graphics::title(
    sub = bquote(list(
        r[0] == .(r0),
        dot(r)[0] == .(rdot0),
        theta[0] == .(theta0),
        dot(theta)[0] == .(thetadot0))
    ),
    adj = 1
)
## Don't show: 
remove(g, r0, rdot0, theta0, thetadot0, k, independent, initialConditions, fun, value, x, y, xylim)
## End(Don't show)









# Consider an animal population defined by
#     d/dt(P) = k * P * (M - P) - h
# where
#     t = time
#     P = population as a function of time
#     k = how fast the population increases
#     M = carrying capacity of the population within their environment
#     h = amount harvested / / hunted
#
#
# The solution to this equation is
#     P(t) = (M1 * (P0 - M2) - M2 * (P0 - M1) * exp(-k * Delta * t)) /
#         ((P0 - M2) - (P0 - M1) * exp(-k * Delta * t))
# where
#     P0    = initial population
#     Delta = sqrt(M^2 - 4 * h/k)
#     M1    = (M + Delta)/2
#     M2    = (M - Delta)/2
# We will now compare the exact solution stated above
# to the numerical solution from the Euler method


k <- 1
M <- 4
h <- 3


Delta <- sqrt(M^2 - 4 * h/k)
M1    <- (M + Delta)/2
M2    <- (M - Delta)/2


exact <- function (P0, col)
{
    t <- seq.int(0, 5, length.out = 101)
    P <- (M1 * (P0 - M2) - M2 * (P0 - M1) * exp(-k * Delta * t)) /
        ((P0 - M2) - (P0 - M1) * exp(-k * Delta * t))
    graphics::lines(t, P, col = col, lwd = 2)
    invisible()
}
euler <- function (P0, col)
{
    t <- seq.int(0, 5, 0.5)
    initialConditions <- c(P = P0)
    fun <- function(t, P) k * P * (M - P) - h
    P <- essentials::EulerMethod(t, initialConditions, fun)$P
    graphics::lines(t, P, col = col, lwd = 2)
    invisible()
}


P0 <- c(1.5, 2, 3.5)  # different values of initial population to plot
exact_colours <- c("red"    , "green"    , "blue")
euler_colours <- c("maroon4", "darkgreen", "navy")


graphics::par(mar = c(5.1, 4.1, 0.4, 0.4))
graphics::plot(
    xlim = c(0, 5), ylim = c(1.5, 3.5),
    panel.first = grid(col = "gray69"),
    x = NA_real_, y = NA_real_,
    bty = "n",
    xlab = "time", ylab = "population"
)
graphics::title(
    sub = bquote(list(k == .(k), M == .(M), h == .(h))),
    adj = 1
)
for (i in seq_along(P0)) {
    exact(P0[i], exact_colours[i])
    euler(P0[i], euler_colours[i])
}
graphics::legend("bottomright",
    legend = as.expression(essentials::plapply(
        list(
            rep(c("Exact", "Euler"), each = length(P0)),
            rep(P0, 2)
        ),
        function(name, P0) bquote(list(.(name), P[0] == .(P0)))
    )),
    fill   = c(exact_colours, euler_colours),
    ncol   = 2,
    bty = "n")## Don't show: 
remove(k, M, h, Delta, M1, M2, exact, euler, P0, exact_colours, euler_colours, i)
## End(Don't show)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Runge-KuttaMethods", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("as.colorRampPalette")
### * as.colorRampPalette

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.colorRampPalette
### Title: Color Interpolation
### Aliases: as.colorRampPalette

### ** Examples

crp <- as.colorRampPalette(1:9)

show.colors(crp(20))

show.colors(crp(20, end = 19/20))

show.colors(crp(20, alpha = 0.75))

show.colors(crp(20, rev = TRUE))

show.colors(crp(20, end = 19/20, alpha = 0.75, rev = TRUE))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.colorRampPalette", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("as.scalar")
### * as.scalar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: as.scalar
### Title: Scalars
### Aliases: scalar as.scalar as.scalar.logical as.scalar.integer
###   as.scalar.real as.scalar.double as.scalar.numeric as.scalar.complex
###   as.scalar.number as.scalar.string as.scalar.character as.scalar.raw

### ** Examples

## if the type converting from and converting to are identical, as.scalar is a
## much shorter way of writing what you intend.
as.scalar(c(TRUE, FALSE, NA))
as.scalar(1:100)
as.scalar(1:10 + 0.5)
as.scalar(exp((0+1i) * 6 * (-4:4)))
as.scalar(letters)


## if the type converting from and converting to are not identical, it is better
## to specify the type converting to.
as.scalar.logical(c(TRUE, FALSE, NA))
as.scalar.integer(c(TRUE, FALSE, NA))
as.scalar.numeric(c(TRUE, FALSE, NA))
as.scalar.complex(c(TRUE, FALSE, NA))
as.scalar.character(c(TRUE, FALSE, NA))

as.scalar(TRUE, "character")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("as.scalar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("asWindowsbasename")
### * asWindowsbasename

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: asWindowsbasename
### Title: Create a Basename Valid in Windows
### Aliases: asWindowsbasename

### ** Examples

asWindowsbasename(c(
    "  test  ",
    "  testing?.",
    "already valid name"
))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("asWindowsbasename", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("aslength1")
### * aslength1

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: aslength1
### Title: Subset the First Element of a Vector
### Aliases: aslength1

### ** Examples

aslength1(1)
aslength1(1:10)
try(aslength1(integer(0)))

print(system.file("R", "aslength1.Rd", package = "essentials"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("aslength1", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("color.with.alpha")
### * color.with.alpha

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: color.with.alpha
### Title: Color Opacity
### Aliases: color.with.alpha as.hex.code

### ** Examples

example.colors <- c("red", "#FFA50099", "yellow", "green", "blue", "purple")
show.colors(example.colors)
show.colors(color.with.alpha(example.colors, alpha = NULL))  # remove all opacity
show.colors(color.with.alpha(example.colors, alpha = 0.25))  # one quarter opaque
show.colors(color.with.alpha(example.colors, alpha = 0.5))   # half opaque
show.colors(color.with.alpha(example.colors, alpha = 0.75))  # three quarters opaque
show.colors(color.with.alpha(example.colors, alpha = 1))     # fully opaque, same as NULL


example.colors2 <- 1:8
show.colors(example.colors2)
show.colors(color.with.alpha(example.colors2, alpha = 0.5))
show.colors(color.with.alpha(example.colors2, alpha = 0.75))


example.colors3 <- grDevices::col2rgb(gg.colors(10))


## show.colors would not be able to show example.colors3 because it's a matrix
## use as.hex.codes to decode the values to their hex codes, then display
show.colors(as.hex.code(example.colors3))
show.colors(color.with.alpha(example.colors3, alpha = 0.5))
show.colors(color.with.alpha(example.colors3, alpha = 0.75))


example.colors4 <- data.frame(
    red   = c( 75,  66,  24,   0,   0,   0,   0, 108, 187, 253)/255,
    green = c(  0,  44,  80, 112, 142, 168, 190, 208, 221, 227)/255,
    blue  = c( 85, 112, 134, 148, 152, 144, 125,  94,  56,  51)/255
)
show.colors(as.hex.code(example.colors4))
show.colors(color.with.alpha(example.colors4, alpha = 0.5))
show.colors(color.with.alpha(example.colors4, alpha = 0.75))

# alpha can be a vector, here we provide 10 different alpha values
show.colors(color.with.alpha(example.colors4,
    alpha = seq.int(0.5, 1, length.out = 10)))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("color.with.alpha", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dedent")
### * dedent

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dedent
### Title: Remove Common Leading Whitespace
### Aliases: dedent

### ** Examples

cat(dedent("
    here is a multi-line string that appears in the source code. we wish to
    remove the common indent from each line, and dedent should do this for us!
    ---- hopefully this works ----
"), sep = "\n")


# for me, a tab prints as eight spaces when preceded by a newline
# but since tab isn't always eight spaces, we treat this as unsolveable :(
# there is no common leading whitespace since "        " != "\t"
cat(dedent("
        another multi-line string, this time with no common leading whitespace
\tanother multi-line string, this time with no common leading whitespace
"), sep = "\n")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dedent", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("delayedAssign2")
### * delayedAssign2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: delayedAssign2
### Title: Delay Evaluation
### Aliases: delayedAssign2

### ** Examples

# this is a simplified version of what ArgumentParser does with its arguments.
# ArgumentParser hides many details that should be shown here


# make a list like ArgumentParser
args <- list()
add.argument <- function (name, default)
{
    args[[length(args) + 1]] <<-
        list(name = name, default = substitute(default))
    invisible()
}


# these would normally be added by 'ArgumentParser()$add.argument'
add.argument("--alpha", TRUE     )
add.argument("--beta" , `--alpha`)
add.argument("--gamma", `--alpha`)


# this would normally be the environment
# returned by 'ArgumentParser()$parse.args'
value <- new.env()


for (n in seq_along(args)) {


    # we have 'evaluated = TRUE' here because we don't want 'args[[n]]$default'
    # to be the expression of the promise, but whatever 'args[[n]]$default'
    # evaluates to
    delayedAssign2(args[[n]]$name, args[[n]]$default,
        eval.env = value, assign.env = value,
        evaluated = TRUE)


    # this part would normally be more complex to deal with things like
    # multiple names for 1 argument, 'type', 'choices',  and, of course,
    # providing arguments, but i only want to demonstrate 'delayedAssign2' here
}


# then we force evaluate each argument
for (n in seq_along(args)) {
    get(args[[n]]$name, envir = value, inherits = FALSE)
}


# these are all TRUE, could be all NA or FALSE if argument '--alpha=NA' or
# '--alpha=FALSE' was provided to the ArgumentParser
print(as.list(value, all.names = TRUE))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("delayedAssign2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("do.while")
### * do.while

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: do.while
### Title: Do While/Until Loops in R
### Aliases: do.while do.until %while% %until%

### ** Examples

# Suppose you want a unique name for a temporary file (we'll ignore that
# `tempfile` exists for now). We can use a do while loop to create a new
# random name until a unique name is found.
do ({
    value <- sprintf("%x", sample.int(2147483647L, 1L))
    print(value)
}) %while% (file.exists(value))


## note that the following is equivalent:
# do ({
#     value <- sprintf("%x", sample.int(2147483647L, 1L))
#     print(value)
# }) %until% (!file.exists(value))


# Suppose you want a random number that is greater than one million (we'll ignore
# that `stats::runif` exists for now).
do ({
    value <- sample.int(1.01e+06, 1L)
    print(value)
}) %until% (value > 1e+06)


## note that the following is equivalent:
# do ({
#     value <- sample.int(1.01e+06, 1L)
#     print(value)
# }) %while% (value <= 1e+06)


# Finally suppose you wanted to ask the user for input, but wanted to make sure
# it was valid. Here, we'll say the input is valid if it is all numeric
# characters (ignore leading and trailing whitespace).
# Let's put a limit on it too, only ask a certain amount of times
count <- 0L
do ({
    value <- readline("Enter an integer: ")
    value <- gsub("^\\s+|\\s$", "", value)
    valid <- grepl("^[[:digit:]]+$", value)
    count <- count + 1L
}) %until% (count >= 5L || valid)
print(list(value = value, valid = valid, count = count))


## note that the following is equivalent:
# do ({
#     value <- readline("Enter an integer: ")
#     value <- gsub("^\\s+|\\s$", "", value)
#     valid <- grepl("^[[:digit:]]+$", value)
#     count <- count + 1L
# }) %while% (count < 5L && !valid)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("do.while", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("envvars")
### * envvars

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: envvars
### Title: Environment Variables
### Aliases: envvar envvars getEnvvar

### ** Examples

oenv <- envvars(); utils::str(oenv)  # oenv is a named list

getEnvvar("PATH") == envvars()$PATH  # the latter is slower, needs more memory

# change the language, and save the previous value
old.env <- envvars(LANGUAGE = "nn")
old.env              # previous value
envvars("LANGUAGE")  # current value


# restore LANGUAGE back to its previous value,
# or remove it if it previously did not exist
envvars(old.env)


envvars(oenv)  # reset (all) initial environment variables
envvars("LANGAUGE")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("envvars", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("essentials-package")
### * essentials-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: essentials-package
### Title: Essential Functions not Included in Base R
### Aliases: essentials-package essentials
### Keywords: package

### ** Examples

as.numbers("4")
as.numbers("4+0i")  # imaginary component is removed
as.numbers("4+1i")

is.numbers(4L)
is.numbers(4)
is.numbers(4+1i)

as.scalar(1:100)
as.scalar(as.list(1:100))  # coerced to NA_character_ since argument isn't atomic

aslength1(1:100)  # identical to as.scalar(1:100)
aslength1(as.list(1:100))  # returns a list of length 1



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("essentials-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("file.open")
### * file.open

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: file.open
### Title: Open a File or URL
### Aliases: file.open

### ** Examples

## Not run: 
##D file.open(tempdir())
##D file.open("https://cran.r-project.org/doc/manuals/R-exts.html#Documenting-packages")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("file.open", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("flat.list")
### * flat.list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: flat.list
### Title: Combine Values into a Flat List (a List with no List Elements)
### Aliases: flat.list

### ** Examples

x <- list(a = 1:5, list(list(b = 6:10), c = 11:15), list(d = exp(-4)))
print(x)
flat.list(x)
flat.list(x, e = "testing")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("flat.list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hcl.colors2")
### * hcl.colors2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hcl.colors2
### Title: Color Palettes
### Aliases: hcl.colors2 inferno.colors plasma.colors viridis.colors
###   gg.colors show.colors

### ** Examples

show.colors(hcl.colors2(256, palette = "Spectral"))


show.colors(inferno.colors(256))
show.colors(plasma.colors(256))
show.colors(viridis.colors(256, end = 0.9))


show.colors(gg.colors(256))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hcl.colors2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("hypot")
### * hypot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hypot
### Title: Hypotenuse
### Aliases: hypot phypot

### ** Examples

## when a side is infinite, the hypotenuse is Inf
hypot(Inf, NaN)   # Inf
hypot(-Inf, NaN)  # Inf (applies to negative infinity too)

## when a side is NA or NaN, the hypotenuse is NaN
hypot(NaN, 0)     # NaN
hypot(NA , 0)     # NaN

## numbers whose squares would overflow normally are handled well
hypot(.Machine$double.xmax, 5     )
hypot(1e+300              , 1e+300)


## hypotenuse
hypot(3, 4)      # 5
hypot(3+4i)      # 5 (works for complex numbers as well)

## 3-dimensional "hypotenuse"
hypot(3, 4, 12)  # 13

## n-dimensional "hypotenuse"
hypot(1:100)


x <- seq.int(-3, 3, length.out = 101)
y <- 1
(h <- phypot(x, 1))  # parallel hypotenuse
graphics::plot(
    panel.first = graphics::grid(col = "gray69"),
    x = x, y = h, type = "l",
    main = "Distance from" ~ (list(0, 0)) ~ "to" ~ (list(x, 1))
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hypot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("legend.dimensions")
### * legend.dimensions

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: legend.dimensions
### Title: Size of a Legend
### Aliases: legend.dimensions

### ** Examples
## Don't show: 
omar <- graphics::par("mar")
## End(Don't show)
# the expression that produces the desired legend
expr <- ~graphics::legend(


    # the top-left corner of the legend will appear in the
    # top-right corner of the plot
    x = essentials::fix.xlog(graphics::par("usr")[2L]),
    y = essentials::fix.ylog(graphics::par("usr")[4L]),


    legend = letters[1:5], fill = 1:5,
    xpd = TRUE
)


# we'll start by drawing a plot with a legend without
# adjusting the margins. the margins will likely look too
# small or too large
graphics::plot(1:5) ; essentials::add.legend(expr)






# now, we'll adjust the margins using 'legend.dimensions'
# capture the dimensions of the resultant legend
ld <- essentials::legend.dimensions(expr)


essentials::adj.margins(ld)
graphics::plot(1:5) ; essentials::add.legend(expr)





# now, we'll adjust the legend such that it is centered
# vertically
essentials::location(expr) <- essentials::location(ld, adj = 0.5)
graphics::plot(1:5) ; essentials::add.legend(expr)





# you do not need to use 'legend.dimensions' explicitly, you
# could supply 'expr' directly to 'adj.margins' and
# 'location'
essentials::adj.margins(expr)
essentials::location(expr) <- essentials::location(expr, adj = 0.75)
graphics::plot(1:5) ; essentials::add.legend(expr)
## Don't show: 
graphics::par(mar = omar)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("legend.dimensions", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("list.files2")
### * list.files2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: list.files2
### Title: List the Files in a Directory/Folder
### Aliases: list.files2 dir2

### ** Examples

## Not run: 
##D FILE <- paste0(tempfile(tmpdir = tempfile("dir")), "_testing_\u{03B4}.txt")
##D dir.create(dirname(FILE))
##D invisible(file.create(FILE))
##D 
##D 
##D x <- list.files (dirname(FILE), full.names = TRUE)
##D y <- list.files2(dirname(FILE), full.names = TRUE)
##D `names<-`(file.exists(x), x)
##D `names<-`(file.exists(y), y)
##D 
##D 
##D unlink(dirname(FILE), recursive = TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("list.files2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("listify")
### * listify

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: listify
### Title: Create a List from an Object
### Aliases: listify

### ** Examples

listify(5)        # when 'x' is not a list, returns 'list(x)'
listify(list(5))  # when 'x' is a list, returns 'x'


## when 'x' is an S4 data.frame, returns 'x'
listify(methods::new("data.frame"))


## when 'x' is an unmodified S3 data.frame, returns 'list(x)'
listify(data.frame())


## S3 data.frame 'x' could be modified such that 'listify' returns 'x'
listify(structure(data.frame(), class = c("data.frame", "list")))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("listify", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mfor")
### * mfor

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mfor
### Title: Multivariate For Loop
### Aliases: mfor

### ** Examples

# when exactly one variable is specified,
# the behaviour is similar to 'for'
mfor(i, 1:5, print(1:i))


# 'mfor' works on classed objects, 'for' does not
mfor(date, Sys.time() + 0:9, print(date))


# sequences are recycled as necessary,
# with a warning for fractional recycling
mfor(i, j, k, list(1:4, 6:10, 11:15), {
    print(c(i = i, j = j, k = k))
})


# mfor works well with data frames
mfor(          col              , cex, main               ,
    data.frame(palette.colors(3), 1:3, paste("title", 1:3)),
    graphics::plot(x = 1:5, col = col, cex = cex, main = main,
        pch = 16)
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mfor", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("numbers")
### * numbers

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: numbers
### Title: Number Vectors
### Aliases: numbers as.numbers is.numbers as.numbers.default

### ** Examples

x <- 1:5
names(x) <- c("a", "b", "c", "d", "e")
as.numbers(x)  # vector converted from integer to double, names removed


x <- x + 0i  # x is now a complex vector
as.numbers(x)  # vector of type double since all numbers were purely real


## vector of type complex, despite being purely real
as.numbers(x, strict = FALSE)


x <- x + 1i
## vector remains of type complex since numbers are not purely real
as.numbers(x)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("numbers", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("path.contract")
### * path.contract

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: path.contract
### Title: Contract File Paths
### Aliases: path.contract

### ** Examples

stopifnot(path.contract(path.expand(x <- c("~", "~/foo"))) == x)
# Note that this is not necessarily true the other way around (in Windows)
# simply because the path separator may have changed


tilde <- path.expand("~")
if (tilde == "~") {
    cat("the home directory is unknown or none is specified\n")
} else {
    paths <- file.path(c(tilde, toupper(tilde), tolower(tilde)), "foo")
    print(cbind(
        Path = paths,
        `Contracted Path` = path.contract(paths),
        `Contracted Path (ignoring case)` = path.contract(paths, ignore.case = TRUE),
        `Contracted Path (with case)` = path.contract(paths, ignore.case = FALSE)
    ), quote = FALSE)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("path.contract", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plapply")
### * plapply

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plapply
### Title: Apply a Function to Multiple List or Vector Arguments
### Aliases: plapply psapply pvapply

### ** Examples

plapply(list(
    col = c("red", "green", "blue"),
    cex = c(1, 1.5, 2),
    main = c("title 1", "title 2", "title 3")
), graphics::plot, x = 1:5, pch = 16)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plapply", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("progressBar")
### * progressBar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: progressBar
### Title: Progress Bars
### Aliases: progressBar setProgressBarMethod getProgress setProgress
###   increment decrement

### ** Examples

# if we use 'tryCatch', we can make use 'finally' to guarantee the progress bar
# is closed, regardless of signalling an error or user interupt. not entirely
# useful in this example, but it can be in longer, more complicated situations
tryCatch({
    pb <- progressBar(if (.Platform$OS.type == "windows")
        "win"
    else "txt", max = 20, style = 3)
    for (i in 1:20) {
        Sys.sleep(0.05)
        increment(pb)
    }
}, finally = close(pb))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("progressBar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("python")
### * python

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: python
### Title: Start a New Python Session
### Aliases: python

### ** Examples

## Not run: 
##D essentials::python(command = essentials::dedent(r"{
##D     print("Hello World!")
##D     print("An example python script...")
##D }"))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("python", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("readArgs")
### * readArgs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: readArgs
### Title: Read/Write Command-Line Arguments from/to a File
### Aliases: readArgs writeArgs

### ** Examples

x <- letters; essentials::writeArgs(x, file = "")


comment(x) <- essentials::dedent("
    adding a comment
    to our arguments
"); essentials::writeArgs(x, file = "")


x <- list(


    local({
        x <- c("\xC5", "\xC9", "\xD8", "\xEC", "\xFC")
        Encoding(x) <- "latin1"
        comment(x) <- "accented characters"
        x
    }),


    local({
        x <- c("\u{03C3}", "\u{03B4}")
        comment(x) <- 'greek letters (default encoding "UTF-8")'
        x
    }),


    local({
        x <- "fa\xE7ile"
        Encoding(x) <- "latin1"  # x is intended to be in latin1
        comment(x) <- essentials::dedent(r"{
            another non-ASCII character ("unknown" in UTF-8 locale,
            "latin1" in IS08859-1, ...)
        }")
        x
    }),


    local({
        x <- c("\u{7B90}", "\u{5316}", "\u{5B57}")
        comment(x) <- 'chinese characters (default encoding "UTF-8")'
        x
    }),


    local({
        x <- c("\U{0001D11E}", "\U{0001D4D7}")
        comment(x) <- "rarer characters outside the usual 16^4 range"
        x
    }),


    local({
        x <- essentials::dedent(r"{
            this would be 'rather' annoying to quote for a `shell` on $Unix$,
            and even "more" so on Windows because of the \"double\" quotes!
        }")
        comment(x) <- "all ASCII characters, annoying to quote, hard to read"
        x
    })
)
comment(x) <- essentials::dedent("
    these are some unusual characters
    but should still behave correctly
")


# the same arguments in different formats
essentials::writeArgs(x, "", name = "Rargs")
essentials::writeArgs(x, "", name = "pyargs")
essentials::writeArgs(x, "", name = "csv")
essentials::writeArgs(x, "", name = "tsv")
essentials::writeArgs(x, "", name = NULL)


FILE <- essentials::writeArgs(x, at = FALSE)
y <- essentials::readArgs(FILE)


# for the purpose of comparison, we need 'x' to be a character vector
z <- essentials:::asArgs(x)


# let's check that the arguments in 'x' match the
# arguments written to and read back from 'FILE' (in 'y')
#
# we don't use 'identical(x, y)' because 'x' has
# attributes (a comment) while 'y' does not
if (length(z) != length(y)) {
    cat(gettextf("Catastrophic failure, wrote %d arguments, read %d\n",
        length(z), length(y)),
        file = stderr())
    stop("Please submit a bug report using ",
        "utils::bug.report(package = \"essentials\")")
} else if (any(i <- z != y)) {
    cat(ngettext(sum(i),
        "The following argument was written or read incorrectly!\n",
        "The following arguments were written or read incorrectly!\n"),
        file = stderr())
    print(z[i])
    cat("\nIncorrectly written or read as:\n", file = stderr())
    print(y[i])
    stop("Please submit a bug report using ",
        "utils::bug.report(package = \"essentials\")")
} else cat("Yay! The arguments were written and read correctly!\n")





# Using writeArgs and withArgs, Rscript
cat(
    essentials::dedent(r"{
        withAutoprint({
            parser <- essentials::ArgumentParser()
            parser$add.argument("args", nargs = "*")
            pargs <- parser$parse.args()
            print(pargs$args)
        })
    }"),
    file = script <- tempfile(), sep = "\n"
)
essentials::withArgs(
    source(script, local = TRUE, echo = FALSE),
    paste0("@", FILE)
)
essentials::Rscript("--default-packages=NULL", script, args = paste0("@", FILE))
## Don't show: 
unlink(script)
## End(Don't show)




# reading/writing from/to a compressed file
FILE2 <- essentials::writeArgs(x, at = FALSE, fileext = ".Rargs.gz")
stopifnot(identical(
    essentials::readArgs(FILE),
    essentials::readArgs(FILE2)
))


# miniscule difference, more desirable with more arguments
c(file.size.original   = file.size(FILE),
  file.size.compressed = file.size(FILE2))
## Don't show: 
unlink(FILE); unlink(FILE2)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("readArgs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setReadWriteArgsMethod")
### * setReadWriteArgsMethod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setReadWriteArgsMethod
### Title: Create and Save a Method for Reading/Writing Command-Line
###   Arguments from/to a File
### Aliases: has.ext scan2 format4scan setReadWriteArgsMethod

### ** Examples

# suppose you wanted to define your own method for
# reading/writing command-line arguments to a file. we'll
# say the file extension will be ".myargs". with this, we
# start by making 'condition'
condition <- function(file) {
    essentials::has.ext(file, ".myargs",
        compression = TRUE, fixed = TRUE)
}


# next, we will make a reading function. this will typically
# be some variation of 'scan2', but feel free to use
# anything else that works. for this example, we'll use
# "-" as the delimiter, "`" as the quoting character, and
# "/" as the comment character
read <- function(file) {
    essentials::scan2(file = file, sep = "-",
        quote = "`", comment.char = "/")
}


# next, we will make a writing function. this will typically
# be some variation of 'format4scan', but feel free to use
# anything else that works
write <- function(x, comments = TRUE,
    nlines.between.comment.and.args = 0,
    nlines.between.args = 2) {
    essentials::format4scan(x, sep = "-", quote = "`",
        comment.char = if (comments) "/" else "",
        nlines.between.comment.and.args = nlines.between.comment.and.args,
        nlines.between.args = nlines.between.args)
}


# now, combine it all together
essentials::setReadWriteArgsMethod(
    name      = "myargs",
    condition = condition,
    read      = read,
    write     = write
)


# try writing arguments with this new format
x <- letters
comment(x) <- "testing comments"
essentials::writeArgs(x, "", name = "myargs")


# confirm that writing and reading returns the same set of
# arguments
FILE <- essentials::writeArgs(x, fileext = ".myargs", at = FALSE)
y <- essentials::readArgs(FILE)
stopifnot(length(x) == length(y), x == y)
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setReadWriteArgsMethod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shEncode")
### * shEncode

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shEncode
### Title: Quote Strings for Use in OS Shells
### Aliases: shEncode commandEncode commandQuote

### ** Examples

fun <- function(string) {
    cat(c(
        "string  ", string                                        , "\n",
        "sh      ", essentials::shEncode(string, type = "sh"     ), "\n",
        "Rscript ", essentials::shEncode(string, type = "Rscript"), "\n",
        "R       ", essentials::shEncode(string, type = "R"      ), "\n"
    ), sep = "")
}

fun("abc$def`gh`i\\j")

fun("testing \\\"this\\\"")

fun("\"testing\" $this$ 'out'")


## Not run: 
##D essentials:::.system(paste(c(
##D     "perl",
##D     "-e",
##D     essentials::shEncode(r"{print "test \"this\" out\n";}",
##D         windows.type = "perl")
##D ), collapse = " "))
## End(Not run)


## Not run: 
##D essentials:::.system(paste(c(
##D     "python",
##D     "-c",
##D     essentials::shEncode(r"{print("test \"this\" out")}",
##D         windows.type = "python")
##D ), collapse = " "))
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shEncode", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shPrompt")
### * shPrompt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shPrompt
### Title: Replicate the Command-Line / / Terminal Prompts Seen on a Few
###   Common Shells
### Aliases: shPrompt

### ** Examples

cat(
    essentials::shPrompt("cmd"       ),
    essentials::shPrompt("powershell"),
    essentials::shPrompt("macOS"     ),
    essentials::shPrompt("bash"      ),
    sep = "\n"
)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shPrompt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("strip")
### * strip

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: strip
### Title: Remove Leading and Trailing White Spaces
### Aliases: strip

### ** Examples

x <- c(
    "  the quick brown fox jumps over a lazy dog  ",
    "  the quick brown fox jumps over a lazy dog\t\n"
)
strip(x)  # the leading and trailing tab, newline, and space are removed


## x is intended to be in encoding latin1
x <- "fa\xE7ile"
Encoding(x) <- "latin1"
y <- strip(x)


## since 'x' has no leading or trailing white space, 'strip(x)' retains the
## encoding of 'x'
Encoding(y)
y



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("strip", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("toProv")
### * toProv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: toProv
### Title: Convert the Provinces and Territories of Canada to their Names
###   or Postal Abbreviations
### Aliases: toProv toProv2 toProvince

### ** Examples

x <- c(
    "Ontario"                  , "ON", "0N",


    # alternate forms
    "Quebec"                   , "QC", "Qu\u{00E9}bec",


    # case insensitive
    "Nova Scotia"              , "NS", "nova scotia",
    "New Brunswick"            , "NB",
    "Manitoba"                 , "MB",
    "British Columbia"         , "BC",
    "Prince Edward Island"     , "PE", "PEI",


    # partial matching
    "Saskatchewan"             , "SK", "Sask",
    "Alberta"                  , "AB", "AL",
    "Newfoundland and Labrador", "NL", "Newfoundland & Labrador", "NF",
    "Northwest Territories"    , "NT",
    "Yukon"                    , "YT", "Yukon Territory", "YK",
    "Nunavut"                  , "NU"
)
cbind(Original = x, `Postal Abbr` = toProv(x), Name = toProvince(x))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("toProv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tryExcept")
### * tryExcept

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tryExcept
### Title: Condition Handling and Recovery
### Aliases: tryExcept

### ** Examples

# the following example won't work using 'utils::example' because it signals
# errors, so copy and paste this code into the R Console


## Not run: 
##D tryCatch({
##D     stop("error in 'expr'")
##D }, finally = {
##D     stop("tryCatch will not reach the second expression in 'finally'")
##D     stop("but tryExcept will")
##D })
##D 
##D 
##D essentials::tryExcept({
##D     stop("error in 'expr'")
##D }, finally = {
##D     stop("tryCatch will not reach the second expression in 'finally'")
##D     stop("but tryExcept will")
##D })
##D 
##D 
##D essentials::tryExcept({
##D     stop("error in 'expr'")
##D }, finally = {
##D     cat("this environment = "); print(environment())
##D     stop("err1")
##D     stop("err2")
##D     print(5 + 6)
##D     stop("err3")
##D 
##D 
##D     # just checking that the arguments aren't evaluated in the wrong frame
##D     # (why would they be though??)
##D     expr
##D     finally
##D 
##D 
##D     # testing that code with a source reference will be evaluated properly
##D     print(function(x) {
##D         x  # testing comments only appearing in source reference
##D     })
##D 
##D 
##D     stop("err4")
##D     print(6 + 7)
##D 
##D 
##D })
##D 
##D 
##D essentials::tryExcept({
##D     cat("this one should behave the same as tryCatch",
##D         "because 'finally' is not a compound expression",
##D         "(is not of class \"{\")",
##D         sep = "\n")
##D     stop("error in 'expr'")
##D }, finally =
##D 
##D 
##D     # checking again that 'finally' is evaluated in the correct environment
##D     cat("this environment =", utils::capture.output(environment()), "\n"))
##D 
##D 
##D essentials::tryExcept({
##D 
##D 
##D     cat("Here is a situation in which you might actually use this.",
##D         "Suppose you're changing a bunch of settings and options that you want",
##D         "to reset when 'tryExcept' finishes. For example:",
##D         "* changing the working directory",
##D         "* changing options stored in `options()`",
##D         "* changing graphical parameters stored in `graphics::par()`",
##D         "* shutting down a graphics device with `dev.off()`",
##D         "* changing the current graphics device with `dev.set()`",
##D         "* changing the state of `grDevices::devAskNewPage()`",
##D         "* deleting a temporary file created with `tempfile()`",
##D         "    or downloaded with `utils::download.file()`",
##D         "* closing a connection",
##D         "* changing the random number generator state",
##D         "    with `RNGkind()` or `set.seed()`",
##D         "* any other type of cleaning process",
##D         sep = "\n")
##D     owd <- getwd()
##D     oopt <- options(max.print = 10, digits = 17)
##D     odev <- grDevices::dev.cur()
##D     if (names(odev) != "null device")
##D         oldask <- grDevices::devAskNewPage(ask = FALSE)
##D     FILE <- tempfile()
##D     setwd(dirname(FILE))
##D     con <- file(FILE, "w")
##D     if (has.Random.seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE))
##D         oldSeed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
##D     else oldRNG <- RNGkind()
##D     RNGkind("default", "default", "default")
##D     set.seed(1)
##D 
##D 
##D     cat("\nYou've setup your settings and options above",
##D         "so now we do something with it", sep = "\n")
##D 
##D 
##D     cat("\nOnly 10 of these will print\n")
##D     print(1:100)
##D 
##D 
##D     cat("\nWe're plotting an image which we will remove afterwards\n")
##D     plot(1:10)
##D     cdev <- grDevices::dev.cur()
##D     Sys.sleep(2)
##D 
##D 
##D }, finally = {
##D 
##D 
##D     cat("\nNow clean up the everything, but unlike 'tryCatch', run all",
##D         "cleaning steps even if one signals an error", sep = "\n")
##D 
##D 
##D     grDevices::dev.off(cdev)
##D     close(con)
##D     file.remove(FILE)
##D 
##D 
##D     if (has.Random.seed)
##D         assign(".Random.seed", oldSeed, envir = globalenv(), inherits = FALSE)
##D     else RNGkind(oldRNG[1L], oldRNG[2L], oldRNG[3L])
##D 
##D 
##D     if (names(odev) != "null device") {
##D         grDevices::dev.set(oldask)
##D         grDevices::devAskNewPage(oldask)
##D     }
##D 
##D 
##D     setwd(owd)
##D     options(oopt)
##D })
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tryExcept", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("withArgs")
### * withArgs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: withArgs
### Title: Source R Code, Providing Arguments to the Script
### Aliases: withArgs

### ** Examples

essentials:::write.code(file = FILE <- tempfile(), {
    withAutoprint({


        this.path::this.path()
        essentials::Args()


    }, verbose = FALSE)
})


# wrap your source call with a call to `withArgs`
essentials::withArgs(
    source(FILE, local = TRUE, verbose = FALSE),
    letters, pi, exp(1)
)
essentials::withArgs(
    sys.source(FILE, environment()),
    letters, pi + 1i * exp(1)
)


# with R >= 4.1.0, use the forward pipe operator `|>` to
# make calls to `withArgs` more intuitive:
# source(FILE, local = TRUE, verbose = FALSE) |> essentials::withArgs(
#     letters, pi, exp(1)
# )
# sys.source(FILE, environment()) |> essentials::withArgs(
#     letters, pi + 1i * exp(1)
# )
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("withArgs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wrapper")
### * wrapper

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wrapper
### Title: Wrapper Functions
### Aliases: wrapper

### ** Examples

# I don't particularly like that the function data.frame has
# the formal argument "check.names" default to TRUE. Here, we
# will use 'wrapper' to make a wrapper for data.frame that has
# "check.names" set to FALSE


# we want the function body to look like this
wrapper(data.frame)


## make the function with the appropriate function body
data.frame2 <- function() NULL
body(data.frame2) <- wrapper(data.frame)


## add the function formals, changing "check.names" from TRUE to FALSE
formals(data.frame2) <- formals(data.frame)
formals(data.frame2)$check.names <- FALSE


print(data.frame2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wrapper", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("zplapply")
### * zplapply

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: .plapply
### Title: Apply a Function to Multiple List or Vector Arguments
### Aliases: .plapply .psapply .pvapply

### ** Examples

# you should see here that plapply will not evaluate its
# optional arguments to FUN (because they are not used in
# this example)
#
# but .plapply will evaluate its optional arguments, even
# though they are not used in this example
invisible(essentials:: plapply(NA, function(...) {
    print(substitute(list(...)))
},      k = cat("evaluated optional arguments to FUN\n") ))
invisible(essentials::.plapply(NA, function(...) {
    print(substitute(list(...)))
}, list(k = cat("evaluated optional arguments to FUN\n"))))


# also, plapply will only evaluate optional arguments as
# requested
invisible(essentials:: plapply(NA, function(x, ...) ..1,
         cat("evaluated first optional argument to FUN\n"),
         cat("evaluated second optional argument to FUN\n") ))
invisible(essentials::.plapply(NA, function(x, ...) ..1,
    list(cat("evaluated first optional argument to FUN\n"),
         cat("evaluated second optional argument to FUN\n"))))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("zplapply", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

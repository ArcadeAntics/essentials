\name{Runge-Kutta Methods}
\alias{Runge-Kutta Methods}
\alias{EulerMethod}
\alias{RK1}
\alias{ImprovedEulerMethod}
\alias{RK2}
\alias{RungeKuttaMethod}
\alias{RK4}
\title{Runge-Kutta Methods}
\description{
  Performs the Runge-Kutta method on a collection of dependent variables
}
\usage{
EulerMethod(independent, initialConditions, fun)
RK1(independent, initialConditions, fun)

ImprovedEulerMethod(independent, initialConditions, fun)
RK2(independent, initialConditions, fun)

RungeKuttaMethod(independent, initialConditions, fun)
RK4(independent, initialConditions, fun)
}
\arguments{
  \item{independent}{list or numeric; if a list, the first element will be used.

    A numeric sequence of independent variable values.

    The advantage of a list is explained in the \bold{Value} section.}

  \item{initialConditions}{numeric or complex; initial values for a series of
    dependent variables.}

  \item{fun}{function; accepts at least two arguments, the first an independent
    variable value, the second a series of dependent variable values.}
}
\details{
  Consider a problem given as:

  \deqn{\frac{dx}{dq} = f(q, x), x(q[0]) = x[0]}{dx/dq = f(q, x), x(q0) = x0}

  Here x is an unknown function (scalar or vector) of independent quantity q
  that we wish to approximate. We know that dx/dq, the rate at which x changes,
  is a function of q and x itself. At the initial quantity q0, the
  corresponding value of x is x0. To use any of the Runge-Kutta methods,
  provide a series of independent quantity values as "independent" (the first
  of which should be q0), the initial quantities of x, x0, as
  "initialConditions", and the function f(q, x) as "fun"
}
\value{
  The advantage of a list is that a name may be provided to the independent
  variable. For instance, you could say:

  \code{EulerMethod(independent = seq(0, 1, 0.001), ...)}

  or you could say:

  \code{EulerMethod(independent = list(t = seq(0, 1, 0.001)), ...)}

  This way the return value will name the independent variable
}
\examples{
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
\dontshow{remove(g, r0, rdot0, theta0, thetadot0, k, independent, initialConditions, fun, value, x, y, xylim)}









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
    bty = "n")\dontshow{remove(k, M, h, Delta, M1, M2, exact, euler, P0, exact_colours, euler_colours, i)}
}

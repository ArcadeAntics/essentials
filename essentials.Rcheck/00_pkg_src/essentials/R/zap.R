zap <- function (x)
x[0L, , drop = FALSE]


zap.table <- function (...)
zap(read.table(..., nrows = 1L))


zap.csv <- function (...)
zap(read.csv(..., nrows = 1L))


zap.csv2 <- function (...)
zap(read.csv2(..., nrows = 1L))


zap.delim <- function (...)
zap(read.delim(..., nrows = 1L))


zap.delim2 <- function (...)
zap(read.delim2(..., nrows = 1L))

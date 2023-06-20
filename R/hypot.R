phypot <- function (..., na.rm = FALSE)
.External2(.C_phypot, na.rm)


hypot <- function (..., na.rm = FALSE)
.External2(.C_hypot, na.rm)

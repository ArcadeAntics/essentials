.is_mfor_done <- function (ptr)
.External2(.C_ismfordone, ptr)


mfor <- function (...)
.External2(.C_mfor)

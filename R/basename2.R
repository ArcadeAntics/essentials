# basename2 <- function (path)
# {
#     if (typeof(path) != "character")
#         stop(gettext("a character vector argument expected", domain = "R"))
#     path <- path.expand(path)
#     if (os.windows) {
#         # is the root of a network share??
#         #
#         # two slashes, >= 1 non-slashes, >= 1 slashes, >= 1 non-slashes, >= 0 slashes
#         #              host name                       share name
#         pattern <- "^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+[/\\\\]*$"
#         if (all(i <- grepl(pattern, path)))
#             character(length(path))
#         else if (any(i)) {
#             value <- character(length(path))
#             # value[i] <- ""
#             value[!i] <- basename(path[!i])
#             value
#         }
#         else basename(path)
#     }
#     else {
#         pattern <- "^(//[^/]+/+[^/]+)/*$"
#         if (all(i <- grepl(pattern, path)))
#             character(length(path))
#         else if (any(i)) {
#             value <- character(length(path))
#             # value[i] <- ""
#             value[!i] <- basename(path[!i])
#             value
#         }
#         else basename(path)
#     }
# }
#
#
# dirname2 <- function (path)
# {
#     if (typeof(path) != "character")
#         stop(gettext("a character vector argument expected", domain = "R"))
#     path <- path.expand(path)
#     if (os.windows) {
#         # is the root of a network share??
#         #
#         # two slashes, >= 1 non-slashes, >= 1 slashes, >= 1 non-slashes, >= 0 slashes
#         #              host name                       share name
#         pattern <- "^([/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+)([/\\\\]+([^/\\\\]+[/\\\\]*)?)?$"
#         if (all(i <- grepl(pattern, path))) {
#             value <- sub(pattern, "\\1/", path)
#             value <- chartr("\\", "/", value)
#             if (any(j <- startsWith(value, "//")))  # preserve network shares
#                 substr(value[j], 1L, 2L) <- "\\\\"
#             value
#         }
#         else if (any(i)) {
#             value <- character(length(path))
#             value[i] <- {
#                 tmp <- sub(pattern, "\\1/", path[i])
#                 tmp <- chartr("\\", "/", tmp)
#                 if (any(j <- startsWith(tmp, "//")))  # preserve network shares
#                     substr(tmp[j], 1L, 2L) <- "\\\\"
#                 tmp
#             }
#             value[!i] <- dirname(path[!i])
#             value
#         }
#         else dirname(path)
#     }
#     else {
#         if (any(startsWith(path, "//"))) {
#             pattern <- "^(//[^/+/+[^/]+)(/+([^/]+/*)?)?$"
#             if (all(i <- grepl(pattern, path)))
#                 sub(pattern, "\\1/", path)
#             else if (any(i)) {
#                 value <- character(length(path))
#                 value[i] <- sub(pattern, "\\1/", path[i])
#                 value[!i] <- dirname(path[!i])
#                 value
#             }
#             else dirname(path)
#         }
#         else dirname(path)
#     }
# }
#
#
# path <- c("//host-name/share-name/path/to/file", "//host-name/share-name/path/to",
#     "//host-name/share-name/path", "//host-name/share-name/",
#     "//host-name/share-name")
# cbind(path, basename = basename(path), basename2 = basename2(path))
# cbind(path, dirname = dirname(path), dirname2 = dirname2(path))
# cbind(path, path.join(dirname2(path), basename2(path)))

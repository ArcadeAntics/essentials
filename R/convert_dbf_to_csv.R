delayedAssign(".convert_dbf_to_csv.py" , system.file(package = .packageName, "python", "convert_dbf_to_csv.py" , mustWork = TRUE))


convert_dbf_to_csv <- function (input = NULL, output = NULL, names = NULL)
{
    invisible(python(
        file = .convert_dbf_to_csv.py,
        args = c(
            paste0("--input=" , if (is.null(input )) "." else input ),
            paste0("--output=", if (is.null(output)) "." else output),
            if (length(names)) paste0(names, .Platform$path.sep, collapse = "")
        ),
        mustWork = TRUE,
        quiet = TRUE
    ))
}

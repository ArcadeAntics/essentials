\name{list.files2}
\alias{list.files2}
\alias{dir2}
\alias{list.dirs2}
\title{List the Files in a Directory/Folder}
\description{
  On Windows, when the C runtime is not \code{"ucrt"}, \code{list.files2} and
  \code{list.dirs2} list all the files or directories in UTF-8 rather than the
  native encoding. It uses \CRANpkg{jsonlite} and
  \href{https://www.python.org/downloads/}{python}, so make sure you have them
  installed.

  Otherwise, \code{list.files2} and \code{list.dirs2} will just call
  \code{\link[base]{list.files}}.
}
\usage{
list.files2(path = ".", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

       dir2(path = ".", pattern = NULL, all.files = FALSE,
            full.names = FALSE, recursive = FALSE,
            ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

list.dirs2(path = ".", full.names = TRUE, recursive = TRUE)
}
\arguments{
  \item{path, pattern, all.files, full.names, recursive, ignore.case, include.dirs, no..}{
    See \code{\link[base]{list.files}}.}
}
\value{
  A character vector containing the names of the files in the specified
  directories (empty if there were no files). If a path does not exist or is
  not a directory or is unreadable it is skipped.

  \code{list.dirs2} implicitly has \code{all.files = TRUE}, and if
  \code{recursive = TRUE}, the answer includes \code{path} itself (provided it
  is a readable directory).

  \code{dir2} is an alias for \code{list.files2}.
}
\seealso{
  \code{\link[base]{list.files}}
}
\examples{
list.files2(R.home())
## Only files starting with a-l or r
## Note that a-l is locale-dependent, but using case-insensitive
## matching makes it unambiguous in English locales
dir2("../..", pattern = "^[a-lr]", full.names = TRUE, ignore.case = TRUE)

list.dirs2(R.home("doc"))
list.dirs2(R.home("doc"), full.names = FALSE)


## in Windows without Universal C Runtime, 'list.files' returns
## a mis-translated filename whereas 'list.files2' does not

dir.create(DIR <- tempfile("dir"))
invisible(file.create(file.path(DIR, "testing_\\u{03B4}.txt")))


file.exists(list.files (DIR, full.names = TRUE))
file.exists(list.files2(DIR, full.names = TRUE))


unlink(DIR, recursive = TRUE)
}

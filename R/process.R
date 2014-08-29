#' Process website files
#'
#' Process a file from the source directory into the build directory according to its role, as determined by \code{\link{file_role}}
#'
#' @inheritParams get_build_path
#' @param ... further arguments passed to or from other methods.
#'
#' @export
process <- function(path, ...) {
  UseMethod("process")
}

#' @importFrom stringr str_replace
#' @export
#' @rdname process
process.deleted <- function(path, ...) {
  dest <- get_build_path(path)
  message("  remove ", dest)
  status <- try(file.remove(dest))
  return(status)
}

#' @importFrom stringr str_replace
#' @export
#' @rdname process
process.other <- function(path, ...) {
  dest <- get_build_path(path)
  message("     add ", dest)
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  status <- try(file.copy(path, dest, overwrite = TRUE))
  return(status)
}

#' @export
#' @rdname process
process.code <- function(path, ...) {
  message("  source ", path)
  wd <- getwd()
  on.exit(setwd(wd))
  dir <- dirname(path)
  setwd(dir)
  try(source(basename(path)))
  return(NULL)
}

#' @export
#' @rdname process
process.template <- function(path, ...) {
  # get destination directory
  destDir <- dirname(get_build_path(path))
  dir.create(destDir, showWarnings = FALSE, recursive = TRUE)
  # TODO does not work with relative paths
  
  # get layout template
  baseDir <- str_split_fixed(path, "/source/", n=2)[,1]
  layout <- str_c(baseDir, "/source/layouts/main.brew")
  # TODO allow several layouts
    
  # message("  render ", path , " within ", layout)
  message("  render ", path)

  wd <- getwd()
  on.exit(setwd(wd))
  setwd(dirname(path))
  status <- try(render(file=basename(path), layout=layout, dest=destDir))
  return(status)
}

#' Change the path of a file from source to build
#'
#' @param path path to the file to process, in the source directory
#'
#' @keywords internal
get_build_path <- function(path) {
  dest <- str_replace(path, fixed("/source/"), "/build/")
  return(dest)
}
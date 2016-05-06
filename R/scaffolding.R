#' @import stringr

# Get absolute path to source directory
source_dir <- function(dir) {
  dir <- normalizePath(dir)
  stringr::str_c(dir, "/source")
}

# Get absolute path to build directory
build_dir <- function(dir) {
  dir <- normalizePath(dir)
  stringr::str_c(dir, "/build")
}

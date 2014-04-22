#' Remove the build directory
#'
#' @param dir directory containing the website (contains "source" and "build")
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @importFrom stringr str_c
clean <- function(dir=getwd(), ...) {

  # remove final slash, perform path expansion
  dir <- normalizePath(dir)
  buildDir <- str_c(dir, "/build")

  system(str_c("rm -Rf ", buildDir))
  # TODO make that system agnostic using file.remove
}

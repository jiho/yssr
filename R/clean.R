#' Remove the build directory
#'
#' @param dir directory containing the website (contains "source" and "build")
#'
#' @export
#' @importFrom stringr str_c
clean <- function(dir=getwd()) {

  # remove final slash, perform path expansion
  dir <- normalizePath(dir)
  buildDir <- str_c(dir, "/build")

  f <- list.files(buildDir, recursive=TRUE, full.names=TRUE, all.files=TRUE)
  d <- rev(list.dirs(buildDir, recursive=TRUE, full.names=TRUE))
  # NB: rev to put containing directory last

  all <- c(f, d)

  status <- file.remove(all)
  
  nonRemovedFiles <- all[ ! status ]
  if ( length(nonRemovedFiles) > 0 ) {
    stop("Cannot remove files :\n  ", str_c(nonRemovedFiles, collapse="\n  "))
  }

  return(invisible(TRUE))
}

#' Serve the local website
#'
#' @param dir directory containing the website (contains "source" and "build")
#'
#' @export
serve <- function(dir=getwd()) {
  # remove final slash, perform path expansion
  dir <- normalizePath(dir)
  buildDir <- str_c(dir, "/build")
  
  status <- system(str_c("cd \"", buildDir, "\"; open http://0.0.0.0:8000; python -m SimpleHTTPServer"))
  # TODO check for python and fall back on opening files directory.
  # TODO open is OS X only

  return(invisible(status))
}

# TODO implement watching and re-rendering on the fly
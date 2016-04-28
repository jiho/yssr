#' Read site configuration
#'
#' Source the \code{config.R} file at the root of the site, when it exists, which contains options.
#'
#' @param dir to site directory, by default the current directory
#'
#' @return A list, in which various options are defined
#'
#' @import stringr
#' @export
read_config <- function(dir=getwd()) {
  # get configuration file
  dir <- normalizePath(dir)
  config_file <- stringr::str_c(dir, "/config.R")

  # source its content in a separate environment
  conf <- new.env()
  if (file.exists(config_file)) {
    source(config_file, local=conf)
  }
  # TODO investigate if sourcing can be a security risk here
  
  # convert to list for convenience
  conf <- as.list(conf)
  return(conf)
}

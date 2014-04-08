#' List files and directories
#'
#' Displays the content of the provided directory as an HTML list of links. Can display only certain files (e.g. those with a specific extension or prefix) using pattern matching. This function is somewhat equivalent to the PHP and Shell functions of the same name.
#' 
#' @param dir path to the directory of interest
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned (as in \code{\link{list.files}} on which scandir is based)
#' @param type of list to produce: unordered (\code{ul}) or ordered(\code{ol})
#' @param ... passed to \code{\link{list.files}}
#'
#' @importFrom stringr str_c
#' @export
scandir <- function(dir, pattern=NULL, type=c("ul", "ol"), ...) {
  # list all files matching pattern in directory
  files <- list.files(dir, pattern=pattern, ...)

  # convert all file names into a link to this file name
  files <- llply(files, function(f, d) {
    as.character(a(f, href=str_c(d, "/", f)))
  }, d=dir)

  # display a list of links
  display_as_list(files, type=type)
}

#' List relevant files of a site
#'
#' List all files then exclude some file type unwanted in the output and reorder files to be processed in a specified order
#'
#' @param dir directory containing the website (contains "source")
#' @param exclude character string (regular expression) designating files to exclude from being copied in the destination directory. By default, exclude xls, xlsx and csv files which are usually used to contain data from which the website is generated. Set it to NULL to exclude nothing
#'
#' @importFrom stringr str_detect str_c
#' @export
# TODO make it internal, make it relative to current directory, make exclude NULL by default
list_site_files <- function(dir=getwd(), exclude=glob2rx("*.xls|xlsx|csv")) {
  
  # get absolute path to source directory
  dir <- normalizePath(dir)
  sourceDir <- str_c(dir, "/source")
  
  # list all files, recursively, including hidden files
  paths <- list.files(sourceDir, recursive=TRUE, full.names=TRUE, all.files=TRUE)

  # remove current R working directory from the paths, making them relative (hence shorter)
  # paths <- str_replace(paths, fixed(str_c(getwd(), "/")), "")

  # exclude file according to the exclude argument
  if ( ! is.null(exclude) ) {
    excluded <- str_detect(paths, pattern=exclude)
    paths <- paths[!excluded]
  }
  
  # exclude layouts
  paths <- paths[!str_detect(paths, pattern=glob2rx("*/source/layouts/*"))]
  
  # order files in the order they will need to be processed
  # - other content files
  # - code files
  # - template files
  roles <- file_role(paths)
  paths <- paths[order(roles)]
  

  return(paths)
}

#' Get modification date and size of a file
#'
#' @param ... passed to \code{\link[base]{file.info}}, i.e. character vectors containing file paths.
#'
#' @importFrom dplyr select
get_info <- function(...) {
  # get file info
  info <- file.info(...)
  # TODO use hash

  # keep only what's important
  info$path <- row.names(info)
  # row.names(info) <- NULL
  info <- info[,c("path", "mtime", "size")]
  
  return(info)
}

#' Compare two directory states
#'
#' @param old previous state
#' @param new current state
#' @return list containing number of changes \code{n}, and files which have been \code{added}, \code{deleted}, \code{modified} and overall \code{changed}
#'
#' @importFrom dplyr setdiff
compare_state <- function(old, new) {
  deleted_or_modified <- dplyr::setdiff(old, new)$path
  added_or_modified <- dplyr::setdiff(new, old)$path
  
  same <- dplyr::intersect(old, new)$path
  modified <- dplyr::intersect(deleted_or_modified, added_or_modified)

  deleted <- setdiff(deleted_or_modified, modified)
  added <- setdiff(added_or_modified, modified)

  changed <- c(added, deleted, modified)
  n <- length(changed)

  list(n = n, added = added, deleted = deleted, modified = modified, changed = changed)
}

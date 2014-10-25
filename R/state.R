list_source_files
sort_site_files
file_role
file_status
site_state


library(stringr)
library(plyr)

## List source files --------

# get absolute path to source directory
dir <- normalizePath(dir)
sourceDir <- str_c(dir, "/source")

# list all files, recursively, including hidden files
paths <- list.files(sourceDir, recursive=TRUE, all.files=TRUE)

files <- data.frame(path=paths)


# get modification date and size
infos <- file.info(str_c(sourceDir, paths, sep="/"))
files$mtime <- infos$mtime
files$size <- infos$size


#' Guess the role of files from their extension
#' 
#' @param name vector of file names/paths
#'
#' @importFrom tools file_ext
#' @importFrom stringr str_replace
file_role <- function(name) {
  # get lowercase extension
  ext <- file_ext(name)
  ext <- tolower(ext)

  # lowercase version of file extensions for all possible roles
  knownTypes <- list(
    code = c("r"),
    template = c("brew", "markdown", "md", "rhtml", "rmd")
  )
  # make it into a single vector
  knownTypes <- unlist(knownTypes)
  # remove numbers added to names by unlist
  names(knownTypes) <- str_replace(names(knownTypes), "[0-9]", "")

  # find the type of each file
  role <- names(knownTypes)[match(ext, knownTypes)]

  # unknown types have role "other"
  role[is.na(role)] <- "other"

  return(role)
}

files$role <- file_role(paths)



#' Compare two directory states
#'
#' @param old previous state
#' @param new current state
#' @return list containing number of changes \code{n}, and files which have been \code{added}, \code{deleted}, \code{modified} and overall \code{changed}
#'
#' @importFrom dplyr setdiff
file_status <- function(old, new, exclude) {
  deleted_or_modified <- dplyr::setdiff(old, new)$path
  added_or_modified <- dplyr::setdiff(new, old)$path
  
  # identical <- dplyr::intersect(old, new)$path
  modified <- dplyr::intersect(deleted_or_modified, added_or_modified)

  deleted <- setdiff(deleted_or_modified, modified)
  added <- setdiff(added_or_modified, modified)

  # prepare output
  rownames(old) <- old$path
  rownames(new) <- new$path
  
  new$status <- "identical"
  new[added,"status"] <- "added"
  new[modified,"status"] <- "modified"

  old$status <- "deleted"
  new <- rbind(new, old[deleted,])

  return(new)
}





## Determine file status

# excluded




exclude <- NULL

# exclude file according to the exclude argument
if ( ! is.null(exclude) ) {
  excluded <- str_detect(paths, pattern=exclude)
  paths <- paths[!excluded]
}

## Foo ------

# TODO re-run R files every time
# need a real dependency graph to make sure to rerun the files that depend on data

# detect non-existing files (deleted)
roles[!file.exists(path)] <- "deleted"
# TODO exclude code files
# TODO feed the name of the destination! file (foo.brew will give foo.html and this is what needs to be removed)

# make it into an ordered factor to be able to sort in an order that follows yssr's logic: first other, then code, then templates
roles <- factor(roles, levels=c("deleted", "other", "code", "template"))




# order files in the order they will need to be processed
# - other content files
# - code files
# - template files
roles <- file_role(paths)
paths <- paths[order(roles)]
# TODO make this into its own function




#' List sources files of a site
#'
#' List files, possibly excluding some file type unwanted in the output
#'
#' @param dir directory containing the website (contains "source")
#' @param exclude character string (regular expression) designating files to exclude from being copied in the destination directory. By default, exclude xls, xlsx and csv files which are usually used to contain data from which the website is generated. Set it to NULL to exclude nothing
#'
#' @importFrom stringr str_detect str_c
#' @export
# TODO make it internal, make it relative to current directory, make exclude NULL by default
list_source_files <- function(dir=getwd(), exclude=glob2rx("*.xls|xlsx|csv")) {
  
  # get absolute path to source directory
  dir <- normalizePath(dir)
  sourceDir <- str_c(dir, "/source")
  
  # list all files, recursively, including hidden files
  paths <- list.files(sourceDir, recursive=TRUE, full.names=TRUE, all.files=TRUE)
  paths <- str_replace

  # remove current R working directory from the paths, making them relative (hence shorter)
  # paths <- str_replace(paths, fixed(str_c(getwd(), "/")), "")

  # exclude file according to the exclude argument
  if ( ! is.null(exclude) ) {
    excluded <- str_detect(paths, pattern=exclude)
    paths <- paths[!excluded]
  }
  
  # order files in the order they will need to be processed
  # - other content files
  # - code files
  # - template files
  roles <- file_role(paths)
  paths <- paths[order(roles)]
  # TODO make this into its own function
  

  return(paths)
}



file_role <- function(path) {
  # TODO rename that into status, include it into the state.R file (which probably needs to be renamed) and make it more general
  
  # get lowercase extension
  ext <- file_ext(path)
  ext <- tolower(ext)
  
  # lowercase version of file extensions for all possible roles
  knownTypes <- list(
    code = c("r"),
    template = c("brew", "markdown", "md", "rhtml", "rmd")
  )
  # make it into a single vector
  knownTypes <- unlist(knownTypes)
  # remove numbers added to names by unlist
  names(knownTypes) <- str_replace(names(knownTypes), "[0-9]", "")
  
  # find the type of each file
  roles <- names(knownTypes)[match(ext, knownTypes)]
  
  # unknown types have role "other"
  roles[is.na(roles)] <- "other"
  
  # detect non-existing files (deleted)
  roles[!file.exists(path)] <- "deleted"
  # TODO exclude code files
  # TODO feed the name of the destination! file (foo.brew will give foo.html and this is what needs to be removed)
  
  # make it into an ordered factor to be able to sort in an order that follows yssr's logic: first other, then code, then templates
  roles <- factor(roles, levels=c("deleted", "other", "code", "template"))
   
  return(roles)
}




# exclude layouts
paths <- paths[!str_detect(paths, pattern=glob2rx("*/source/layouts/*"))]





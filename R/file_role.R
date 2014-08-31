#' Guess the role of files from their extension
#' 
#' @param path vector of paths to files
#'
#' @importFrom tools file_ext
#' @importFrom stringr str_replace
file_role <- function(path) {
  
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
  
  # make it into an ordered factor to be able to sort in an order that follows yssr's logic: first other, then code, then templates
  roles <- factor(roles, levels=c("deleted", "other", "code", "template"))
   
  return(roles)
}

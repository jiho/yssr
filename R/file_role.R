#' Guess the role of files from their path and extension
#'
#' @param path path to \emph{one} file, relative to the root of the site.
#'
#' @return A string with the role of the file
#'
#' @examples
#' file_role("code.R")
#' file_role("code.r")
#' file_role("my_page.Rmd")
#' file_role("my_page.md")
#' file_role("my_page.html")
#' file_role("assets/style.css")
#' file_role("assets/logo.png")
#' file_role("templates/main.html")
#' 
#' @import stringr
file_role <- function(path) {
  # markers of known roles: extensions, path elements, etc.
  # NB: keep in order in which files must be processed (code first, then rest)
  markers <- c(
    code="\\.r$",
    code_markup="\\.rmd$",
    markup="\\.markdown$",
    markup="\\.md$",
    markup="\\.text$",
    template="source\\/templates\\/"
  )
  
  # detect if the file matches a given marker (i.e. role)
  matches <- stringr::str_detect(path, stringr::regex(markers, ignore_case=TRUE))
  # when it does, assign it, otherwise give it role "other"
  if (any(matches)) {
    role <- names(markers)[matches][1]
  } else {
    role <- "other"
  }
  
  # # force order of roles (for later processing)
  # role <- factor(role, levels=c(unique(names(markers)), "other"))

  return(role)
}



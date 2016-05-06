# Detect the presence of at least one of several patterns in a string
#
# @param string vector of character strings.
# @param pattern vector of \emph{globbing} patterns. Strings matching one or more patterns are detected.
#
# @return A logical vector, of the same length as \code{string}, containing TRUE when the corresponding string matches at least one of the patterns
#
# @examples
# str_detect_any("foo", c("fo*", "*o*"))
# str_detect_any(c("foo", "bar"), c("fo*", "*o*"))
# str_detect_any(c("foo", "bar"), c("fo*", "*o*", "*r"))
# str_detect_any(c("foo", "bar", "bob"), "*o*")
#
#' @import plyr
#' @import stringr
str_detect_any <- function(string, pattern, ...) {
  matches <- plyr::laply(string, stringr::str_detect, pattern=glob2rx(pattern), .drop=F, ...)
  matches <- plyr::aaply(matches, 1, any, ...)
  return(matches)
}


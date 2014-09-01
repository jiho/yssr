#' Capitalise string
#'
#' Turn the first letter of a string into its capital version
#'
#' @param x a vector of character strings
#'
#' @importFrom stringr str_sub str_c
#' @export
#' @seealso \code{\link[base]{toupper}}, \code{\link{capitalise_words}}
#' @examples
#' capitalise("the quick red fox jumps over the lazy brown dog")
capitalise <- function(x) {
  first <- str_sub(x, 1, 1)
  rest <- str_sub(x, 2)
  x <- str_c(toupper(first), rest)
  return(x)
}
#' @export
#' @rdname capitalise
capitalize <- capitalise

#' Capitalise words in a string
#'
#' Turn the first letter of each word in a string into its capital version
#'
#' @param x a vector of character strings
#' @param sep separator between words. Defaults to single space.
#'
#' @importFrom stringr str_split str_c
#' @importFrom plyr llply
#' @export
#' @seealso \code{\link[base]{toupper}}, \code{\link{capitalise}}
#' @examples
#' capitalise_words("the quick red fox jumps over the lazy brown dog")
capitalise_words <- function(x, sep=fixed(" ")) {
  x <- str_split(x, pattern=sep)
  x <- llply(x, capitalise)
  x <- laply(x, str_c, collapse=sep)
  return(x)
}
#' @export
#' @rdname capitalise_words
capitalize_words <- capitalise


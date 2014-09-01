# Import all HTML tag functions from shiny
#' @importFrom shiny HTML
#' @importFrom shiny tag tags
#' @importFrom shiny p h1 h2 h3 h4 h5 h6 a br div span pre code img em
NULL

#' Create HTML links
#'
#' Create \code{a} tag(s). The function is vectorised over the inputs.
#'
#' @param text vector of character strings containing the text of the links; what is displayed
#' @param url vector of character strings containing the url to which the links point
#' @param ... passed to \code{link[shiny]{a}}
#'
#' @export
link <- function(url, text=url, ...) {
  if (length(url) != length(text)) {
    stop("Need as many text descriptions as urls")
  }
  for (i in seq(along=url)) {
    url[i] <- as.character(a(text[i], href=url[i], ...))
  }
  return(url)
}


#' Create mailto links
#'
#' Create \code{a} tag(s) with \code{href=mailto}. The function is vectorised over the inputs.
#'
#' @param address vector of character strings containing email addresses
#' @param text vector of character strings containing text of for the links. By default, the email address
#' @param obscure wether to obscure the email address with \code{\link{obscure_email}}
#' @param ... passed to \code{link[shiny]{a}}
#'
#' @export
#' @seealso \code{\link{obscure_email}}
mailto <- function(address, text=NULL, obscure=TRUE, ...) {
  if ( obscure ) {
    address <- obscure_email(address)
  }
  if ( is.null(text) ) {
    text <- address
  } else {
    if (length(address) != length(text)) {
      stop("Need as many text descriptions as email addresses")
    }
  }
  for (i in seq(along=address)) {
    text[i] <- as.character(a(text[i], href=str_c("mailto:",address[i]), ...))
  }
  return(text)
}


#' Toggle a logical value into an attribute for an HTML tag
#'
#' Attributes are set in HTML tags by setting them to NA. This is not very human-readable. It is more understandable to set them to TRUE to set the attribute and to FALSE not to set it.
#'
#' @param x logical value
#'
#' @export
#' @examples
#' library("shiny")
#' tags$input(name="Foo", required=toggle_attribute(FALSE))
#' tags$input(name="Foo", required=NULL)
#' tags$input(name="Foo", required=toggle_attribute(TRUE))
#' tags$input(name="Foo", required=NA)
toggle_attribute <- function(x) {
  if (!is.logical(x)) {
    stop("Attributes to be toggled need to be logical")
  }
  if (x) {
    out <- NA
  } else {
    out <- NULL
  }
  return(out)
}

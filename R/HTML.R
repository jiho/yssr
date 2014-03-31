# Import all HTML generating functions from shiny
#' importFrom shiny tag tags
#' importFrom shiny p h1 h2 h3 h4 h5 h6 a br div span pre code img string em hr tags


#' Create an HTML link
#'
#' @param text text of the link, what is displayed
#' @param url url to which the link points
#'
#' @export
link <- function(text, url) {
  a(text, href=url)
}


#' Create an HTML unordered list
#'
#' @param x an R object (a vector or a list), each element of which will be wrapped in \code{<li>} tags and inserted into a \code{<ul>} tag. If x is a list of lists, it will result in nested \code{<ul>} lists
#' @param ... passed to \code{\link{shiny::tags}$ul}
#'
#' @examples
#' ul(c("a", "b", "c"))
#' ul(list("a", list("a.1", "a.2"), "b"))
#'
#' @export
ul <- function(x, ...) {
  UseMethod("ul")
}

# method for vectors
#' @export
ul.default <- function(x, ...) {
  tags$ul(
    llply(x, tags$li),
    ...
  )
}

# method for lists, possibly nested
#' @export
ul.list <- function(x, ...) {
  tags$ul(
    llply(x, function(X) {
      if (is.list(X)) {
        ul.list(X)
      } else {
        tags$li(X)
      }
    }),
    ...
  )
}

#' Create an HTML ordered list
#'
#' @param x an R object (a vector or a list), each element of which will be wrapped in \code{<li>} tags and inserted into a \code{<ol>} tag. If x is a list of lists, it will result in nested \code{<ol>} lists
#' @param ... passed to \code{\link{shiny::tags}$ol}
#'
#' @examples
#' ol(c("a", "b", "c"))
#' ol(list("a", list("a.1", "a.2"), "b"))
#'
#' @export
ol <- function(x, ...) {
  UseMethod("ol")
}

# method for vectors
#' @export
ol.default <- function(x, ...) {
  tags$ol(
    llply(x, tags$li)
  )
}

# method for lists, possibly nested
#' @export
ol.list <- function(x, ...) {
  tags$ol(
    llply(x, function(X) {
      if (is.list(X)) {
        ol.list(X)
      } else {
        tags$li(X)
      }
    })
  )
}

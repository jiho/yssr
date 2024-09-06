#' Create an HTML list
#'
#' @param x an R object (a vector or a list), each element of which will be wrapped in \code{<li>} tags and inserted into a \code{<ul>} or \code{<ol>} tag. If x is a list of lists, it will result in nested \code{<ul>}/\code{<ol>} lists
#' @param type the type of HTML list : unordered or ordered
#' @param ... passed to \code{shiny::tags}
#'
#' @examples
#' display_as_list(c("a", "b", "c"))
#' display_as_list(c("a", "b", "c"), type="ol")
#' display_as_list(list("a", list("a.1", "a.2"), "b"))
#' display_as_list(list("a", list("<a href='foo.html'>a.1</a>", "a.2"), "b"))
#'
#' @export
display_as_list <- function(x, type=c("ul", "ol"), ...) {
  UseMethod("display_as_list")
}

# method for vectors
#' @importFrom plyr llply
#' @export
display_as_list.default <- function(x, type=c("ul", "ol"), ...) {
  type <- match.arg(type)

  out <- tags[[type]](
    llply(x, function(X) tags$li(HTML(X))),
    ...
  )

  return(as.character(out))
}

# method for lists, possibly nested
#' @importFrom plyr llply
#' @export
display_as_list.list <- function(x, type=c("ul", "ol"), ...) {
  type <- match.arg(type)

  out <- tags[[type]](
    llply(x, function(X, type) {
      if (is.list(X)) {
        HTML(display_as_list(X, type=type))
      } else {
        tags$li(HTML(X))
      }
    }, type=type),
    ...
  )

  return(as.character(out))
}

#' @rdname display_as_list
#' @export
display_as_ul <- function(x, ...) {
  display_as_list(x, type="ul", ...)
}

#' @rdname display_as_list
#' @export
display_as_ol <- function(x, ...) {
  display_as_list(x, type="ol", ...)
}

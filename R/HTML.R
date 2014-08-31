# Import all HTML tag functions from shiny
#' @importFrom shiny HTML
#' @importFrom shiny tag tags
#' @importFrom shiny p h1 h2 h3 h4 h5 h6 a br div span pre code img em
NULL

#' Create an HTML link
#'
#' @param text text of the link, what is displayed
#' @param url url to which the link points
#'
#' @export
link <- function(text, url) {
  out <- a(text, href=url)
  return(as.character(out))
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
    llply(x, function(X) {
      if (is.list(X)) {
        display_as_list.list(X)
      } else {
        tags$li(HTML(X))
      }
    }),
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


#' Create an HTML table
#'
#' @param x a data.frame, 2D matrix, table, etc. to be displayed as an HTML table
#' @param digits number of digits to display for numeric columns; default is all digits
#' @param ... passed to \code{shiny::tags$table}
#'
#' @examples
#' d <- data.frame(a=1:3, b=runif(3))
#' display_as_table(d)
#' display_as_table(d, digits=3)
#'
#' @export
#' @importFrom plyr llply alply
display_as_table <- function(x, digits=NULL, ...) {
  if ( is.null(nrow(x)) ) {
    stop("x is not a table")
  }

  if ( nrow(x) == 0 ) {

    warning("nothing to display")

  } else {

    # round numeric columns if necessary
    if ( ! is.null(digits) ) {
      for (j in 1:ncol(x)) {
        if ( is.numeric(x[,j]) ) {
          x[,j] <- round(x[,j], digits=digits)
        }
      }
    }

    # convert factors to characters for representation
    for (j in 1:ncol(x)) {
      if ( is.factor(x[,j]) ) {
        x[,j] <- as.character(x[,j])
      }
    }

    out <- tags$table(
      # header
      if (length(colnames(x)) != 0) {
        tags$thead(
          tags$tr(
            llply(colnames(x), function(X) tags$th(HTML(X)))
          )
        )
      }
      ,
      # content
      tags$tbody(
        alply(x, 1, function(row) {
          tags$tr(
            llply(row,  function(X) tags$td(HTML(X)))
          )
        })
      )
      ,
      ...
    )
  }

  return(as.character(out))
}

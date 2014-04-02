# Import all HTML generating functions from shiny
#' @name HTML
#'
#' @importFrom shiny tag tags
#' @importFrom shiny p h1 h2 h3 h4 h5 h6 a br div span pre code img em tags
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
#' @importFrom plyr llply
ul.default <- function(x, ...) {
  out <- tags$ul(
    llply(x, tags$li),
    ...
  )

  return(as.character(out))
}

# method for lists, possibly nested
#' @export
#' @importFrom plyr llply
ul.list <- function(x, ...) {
  out <- tags$ul(
    llply(x, function(X) {
      if (is.list(X)) {
        ul.list(X)
      } else {
        tags$li(X)
      }
    }),
    ...
  )

  return(as.character(out))
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
#' @importFrom plyr llply
ol.default <- function(x, ...) {
  out <- tags$ol(
    llply(x, tags$li)
  )

  return(as.character(out))
}

# method for lists, possibly nested
#' @export
#' @importFrom plyr llply
ol.list <- function(x, ...) {
  out <- tags$ol(
    llply(x, function(X) {
      if (is.list(X)) {
        ol.list(X)
      } else {
        tags$li(X)
      }
    })
  )

  return(as.character(out))
}

#' Display an HTML table
#'
#' @param x a data.frame, 2D matrix, table, etc. to be displayed as an HTML table
#' @param ... passed to \code{\link{shiny::tags}$table}
#' @export
#' @importFrom plyr llply alply
display_table <- function(x, ...) {
  if ( is.null(nrow(x)) ) {
    stop("x is not a table")
  }
  if ( nrow(x) == 0 ) {
    warning("nothing to display")
  } else {
    out <- tags$table(
      # header
      if (length(colnames(x)) != 0) {
        tags$tr(
          llply(colnames(x), tags$th)
        )
      }
      ,
      # content
      alply(x, 1, function(row) {
        tags$tr(
          llply(row, tags$td)
        )
      }),
      ...
    )
  }

  return(as.character(out))
}

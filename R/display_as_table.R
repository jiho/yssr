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

    warning("Nothing to display")
    out <- ""

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

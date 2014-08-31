#' Capitalise string
#'
#' Turn the first letter of a string into its capital version
#'
#' @param x a vector of character strings
#'
#' @importFrom stringr str_sub str_c
#' @export
capitalise <- function(x) {
  first <- str_sub(x, 1, 1)
  rest <- str_sub(x, 2, nchar(x))
  str_c(toupper(first), rest)
}
capitalize <- capitalise

#' Bootstrap form elements
#'
#' @param action action link for the form
#' @param method post or get
#' @param accept-charset Unicode by default
#' @param ... passed to the corresponding \code{\link[shiny]{tags}} function
#'
#' @seealso \code{\link[shiny]{tags}}
#' @export
form_bootstrap <- function(action, ..., method="post", `accept-charset`="utf-8") {
  method <- match.arg(method, choices=c("post","get"))
  as.character(tags$form(action=action, method=method, `accept-charset`=`accept-charset`, enctype="multipart/form-data", ...))
}
form_bs <- form_bootstrap

#' @param name name of the element, used to process form data in php
#' @param label label of the element, visible on the page; by default, a capitalised version of the name
#' @param type of input. Is checked against valid HTML5 input types
#' @param help help text, placed under the element
#' @param required boolean, wether the input is required or not
#' @export
#' @rdname form_bootstrap
input_bs <- function(name, label=capitalise(name), type="text", help=NULL, required=FALSE, ...) {
  type <- match.arg(type, choices=c("text", "password", "datetime", "datetime-local", "date", "month", "time", "week", "number", "email", "url", "search", "tel", "color", "file"))
  required <- toggle_attribute(required)
  out <- div(class="form-group",
    tags$label("for"=name, class="control-label", label),
    tags$input(name=name, id=name, type=type, class="form-control", required=required, ...),
    if (!is.null(help)) {
      p(class="help-block", help)
    }
  )
  return(as.character(out))
}

#' @importFrom shiny radioButtons
#' @param choices a vector of possible values; if named, the content is the value tag and the names is displayed
#' @export
#' @rdname form_bootstrap
radio_bs <- function(name, choices, label=capitalise(name), ...) {
  out <- div(class="form-group",
    radioButtons(inputId=name, choices=choices, label=label, ...)
    # TODO improve, this is not exactly following bootstrap's syntax
  )
  return(as.character(out))
}

#' @importFrom shiny checkboxGroupInput
#' @export
#' @rdname form_bootstrap
checkbox_bs <- function(name, choices, label=capitalise(name), ...) {
  out <- div(class="form-group",
    checkboxGroupInput(inputId=name, choices=choices, label=label, ...)
    # TODO improve, this is not exactly following bootstrap's syntax
  )
  return(as.character(out))
}

#' @importFrom shiny selectInput
#' @export
#' @rdname form_bootstrap
select_bs <- function(name, choices, label=capitalise(name), ...) {
  out <- div(class="form-group",
    selectInput(inputId=name, choices=choices, label=label, ...)
  )
  return(as.character(out))
}

#' @param class class of the submit button
#' @export
#' @rdname form_bootstrap
submit_bs <- function(label="Submit", class="btn btn-default", ...) {
  tags$button(type="submit", class=class, label, ...)  
}

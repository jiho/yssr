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
#' @export
#' @rdname form_bootstrap
label_bs <- function(name, label) {
  if (label == "") {
    out <- NULL
  } else {
    out <- tags$label("for"=name, class="control-label", label)
  }
  return(out)
}

#' @param type of input. Is checked against valid HTML5 input types
#' @param help help text, placed under the element
#' @param required boolean, wether the input is required or not
#' @export
#' @rdname form_bootstrap
input_bs <- function(name, label=capitalise(name), type="text", help=NULL, required=FALSE, ...) {
  type <- match.arg(type, choices=c("text", "password", "datetime", "datetime-local", "date", "month", "time", "week", "number", "email", "url", "search", "tel", "color", "file"))
  required <- toggle_attribute(required)
  out <- div(class="form-group",
    label_bs(name, label),
    tags$input(name=name, id=name, type=type, class="form-control", required=required, ...),
    if (!is.null(help)) {
      p(class="help-block", help)
    }
  )
  return(out)
}

#' @param rows number of rows shown by default in multiline text areas
#' @export
#' @rdname form_bootstrap
textarea_bs <- function(name, label=capitalise(name), rows=3, help=NULL, required=FALSE, ...) {
  required <- toggle_attribute(required)
  out <- div(class="form-group",
    label_bs(name, label),
    tags$textarea(name=name, id=name, class="form-control", required=required, rows=rows, ...),
    if (!is.null(help)) {
      p(class="help-block", help)
    }
  )
  return(out)
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
  return(out)
}

#' @importFrom shiny checkboxGroupInput
#' @export
#' @rdname form_bootstrap
checkbox_bs <- function(name, choices, label=capitalise(name), ...) {
  out <- div(class="form-group",
    checkboxGroupInput(inputId=name, choices=choices, label=label, ...)
    # TODO improve, this is not exactly following bootstrap's syntax
  )
  return(out)
}

#' @param selected element of the select list to be selected in `select_bs()`
#' @param multiple boolean, allow multiple choices in `select_bs()`
#' @export
#' @rdname form_bootstrap
select_bs <- function(name, choices, label=capitalise(name), selected = NULL, multiple = FALSE, ...) {
  # resolve names
  choices <- shiny:::choicesWithNames(choices)

  # default value if it's not specified
  if (is.null(selected)) {
    if (!multiple) selected <- shiny:::firstChoice(choices)
  } else selected <- shiny:::validateSelected(selected, choices, name)

  # create select tag and add options
  selectTag <- tags$select(id = name, name = name, shiny:::selectOptions(choices, selected), class = "form-control", ...)
  if (multiple) {
    selectTag$attribs$multiple <- "multiple"
  }
  # browser()

  # return label and select tag
  out <- div(class="form-group", shiny:::shinyInputLabel(name, label), selectTag)

  return(out)
}

#' @param class class of the submit button
#' @export
#' @rdname form_bootstrap
submit_bs <- function(label="Submit", class="btn btn-default", ...) {
  tags$button(type="submit", class=class, label, ...)
}

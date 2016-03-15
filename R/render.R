#' Render a template file inside a layout
#'
#' @param file name of a file containing content to render through \code{\link{render_content}} and the insert into a layout template. The extension of the file determines which rendering function is used. Allowed extensions are
#' \describe{
#' \item{".brew"}{brew template, rendered by \code{\link[brew]{brew}}}
#' \item{".rmd"}{R markdown template, rendered by \code{\link[knitr]{knit2html}}}
#' \item{".rhtml"}{R html template, rendered by \code{\link[knitr]{knit}}}
#' \item{".md" or ".markdown"}{markdown template, rendered by \code{\link[markdown]{markdownToHTML}}}
#' }
#' NB: the case of the extension does not matter, e.g. \code{"Rmd"} and \code{"rmd"} are equivalent.
#' NB: to insert raw HTML or PHP into the layout template, just give the file an extension \code{".brew"}.
#' @param layout path to layout template file
#' @param dest path to destination directory
#' @param ... passed to \code{\link{render_content}}
#'
#' @export
#' @importFrom stringr str_c str_replace str_split_fixed
#' @importFrom tools file_ext
#' @importFrom brew brew
render <- function(file, layout, dest, ...) {

  # read content of the file in a character string
  yield <- scan(file, what="character", sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
  yield <- str_c(yield, collapse="\n")

  # get file extension to call the appropriate rendering function
  ext <- file_ext(file)
  class(yield) <- c(tolower(ext), "character")
  yield <- render_content(yield, ...)
  # NB: needs to be named yield for brew() to work below
  # TODO define other environment variables that the template can use

  # prepare destination name
  destExt <- ifelse(str_detect(yield, fixed("<?php")), "php", "html")
  destFile <- str_c(dest, "/", str_replace(file, str_c(ext, "$"), destExt))

  # insert the yield in the layout template
  brew(file=layout, output=destFile)
  # TODO allow something other than brew here

  return(invisible(destFile))
}

#' Render markup from a templating engine into HTML
#'
#' @param text character string with markup to feed to the appropriate conversion engine.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
render_content <- function(text, ...) {
  UseMethod("render_content")
}

#' @importFrom brew brew
#' @export
#' @rdname render_content
render_content.brew <- function(text, ...) {
  out <- capture.output(brew(text=text, ...))
  out <- str_c(out, collapse="\n")
  return(out)
}

#' @importFrom knitr knit2html
#' @param options options passed to \code{\link[markdown]{markdownToHTML}}
#' @export
#' @rdname render_content
render_content.rmd <- function(text, options=c("fragment_only", "smartypants", "base64_images"), ...) {
  out <- knit2html(text=text, fragment.only=TRUE, quiet=TRUE, options=options, ...)
  return(out)
}

# #' @importFrom markdown markdownToHTML
#' @export
#' @rdname render_content
render_content.md <- function(text, options=c("fragment_only", "smartypants", "base64_images"), ...) {
  # out <- markdownToHTML(text=text, fragment.only=TRUE, options=options, ...)
  tmp <- tempfile()
  cat(text, file=tmp)
  out <- rmarkdown::pandoc_convert(input=tmp, to="html")
  # out <- system(paste0("echo ", text, " | pandoc --to html"), intern=T)
  return(out)
}
#' @export
#' @rdname render_content
render_content.markdown <- render_content.md

#' @importFrom knitr knit
#' @export
#' @rdname render_content
render_content.rhtml <- function(text, ...) {
  out <- knit(text=text, quiet=TRUE, ...)
  return(out)
}

#' @export
#' @rdname render_content
render_content.default <- function(text, ...) {
  return(text)
}

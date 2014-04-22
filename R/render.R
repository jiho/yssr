#' Render markup from a templating engine into HTML
#'
#' @param text character string with markup to feed to the appropriate conversion engine. Allowed markup are:
#' \itemize{
#' \item \code{\link{brew}}, with extension \code{.brew}
#' \item \code{R markdown}, with extensions \code{.Rmd} or \code{.rmd}
#' \item \code{markdown}, with extensions \code{.md} or \code{.markdown}
#' \item \code{R HTML} for \code{\link{knitr}}, with extensions \code{.Rhtml} or \code{.rhtml}
#' }
#' @param ... further arguments passed to or from other methods.
#'
#' @export
render_content <- function(text, ...) {
  UseMethod("render_content")
}

#' @importFrom brew brew
#' @export
# TODO check why the methods do not show up in the help
render_content.brew <- function(text, ...) {
  out <- capture.output(brew(text=text, ...))
  out <- str_c(out, collapse="\n")
  return(out)
}

#' @importFrom knitr knit2html
#' @importFrom plyr laply
#' @importFrom stringr str_c
#' @export
render_content.rmd <- function(text, options=c("fragment_only", "smartypants", "base64_images"), ...) {
  out <- knit2html(text=text, fragment.only=TRUE, quiet=TRUE, options=options, ...)
  return(out)
}

#' @importFrom markdown markdownToHTML
#' @export
render_content.md <- function(text, options=c("fragment_only", "smartypants", "base64_images"), ...) {
  out <- markdownToHTML(text=text, fragment.only=TRUE, options=options, ...)
  return(out)
}
#' @export
render_content.markdown <- render_content.md

#' @importFrom knitr knit
#' @export
render_content.rhtml <- function(text, ...) {
  out <- knit(text=text, quiet=TRUE, ...)
  return(out)
}

#' @export
render_content.html <- function(text, ...) {
  return(text)
}


#' Render a file into a full HTML page
#'
#' @param file path a file containing content to render through render_content()
#' @param layout path to a layout template within which the content is to be rendered; should contain \code{yield} somewhere
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @importFrom stringr str_c
#' @importFrom tools file_ext
render_file <- function(file, layout, ...) {

  # check content file and template existence
  if ( ! file.exists(file) ) {
    stop("Cannot find file ", file)
  }
  if ( ! file.exists(layout) ) {
    stop("Cannot find template file ", layout)
  }

  # read content file content in a character string
  yield <- scan(file, what="character", sep="\n", quiet=TRUE, blank.lines.skip=FALSE)
  yield <- str_c(yield, collapse="\n")

  # get file extension and call the appropriate rendering function
  ext <- file_ext(file)
  class(yield) <- c(tolower(ext), "character")
  yield <- render_content(yield)
  # TODO define other environment variables that the template can use

  # render the content of the yield within the template
  dest <- str_replace(file, fixed(ext), "html")
  brew(file=layout, output=dest)
  # TODO allow something else than brew at this point

  return(invisible(dest))
}


#' Render the full website
#'
#' @param dir directory containing the website (contains "source")
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @importFrom stringr str_c str_replace str_detect fixed
#' @importFrom plyr laply l_ply
render <- function(dir=getwd(), ...) {

  # remove final slash, perform path expansion
  dir <- normalizePath(dir)
  sourceDir <- str_c(dir, "/source")

  # record current state of the source directory (to detect new files after running the code and templating)
  filesBefore <- list.files(sourceDir, recursive=TRUE, full.names=TRUE, all.files=TRUE)


  ## 1. Run code

  # list all R source files
  codeFiles <- list.files(sourceDir, pattern=c("(R|r)$"), recursive=TRUE, full.names=TRUE)

  # run code in each file, from the directory in which each file is
  message("Running R files")
  l_ply(codeFiles, function(file) {
    wd <- getwd()
    on.exit(setwd(wd))
    cat(str_replace(file, fixed(str_c(sourceDir, "/")), "  "), "\n")
    setwd(dirname(file))
    source(file)
  })


  ## 2. Render content from templates

  # list files written in a templating language
  templatedFiles <- list.files(sourceDir, pattern=c("(Rmd|rmd|brew|rhtml|Rhtml)$"), recursive=TRUE, full.names=TRUE)
  # remove layout templates
  isLayout <- str_detect(templatedFiles, fixed("source/layouts/"))
  templatedFiles <- templatedFiles[ ! isLayout ]

  # # list html files and detect wether they are fully formed or need to be integrated in a layout template
  # htmlFiles <- list.files(sourceDir, pattern=c("(html)$"), recursive=TRUE, full.names=TRUE)
  # isFullyFormed <- laply(htmlFiles, function(file) {
  #   firstLine <- scan()
  # })
  #
  # contentFiles <- c(templatedFiles, htmlFiles)
  # TODO Sort out how to use html files in a layout since those would need be overwritten with the current way of doing things
  contentFiles <- templatedFiles

  # render all content files
  message("Rendering")
  renderedFiles <- laply(contentFiles, function(file) {
    wd <- getwd()
    on.exit(setwd(wd))
    cat(str_replace(file, fixed(str_c(sourceDir, "/")), "  "), "\n")
    setwd(dirname(file))
    render_file(
      file=file,
      layout=str_c(sourceDir, "/layouts/main.brew")
    )
  })


  ## 3. Move the content to the destination directory

  # detect new files
  filesAfter <- list.files(sourceDir, recursive=TRUE, full.names=TRUE, all.files=TRUE)
  generatedFiles <- filesAfter[ ! filesAfter %in% filesBefore ]

  # list all files to move; do not move code and template files
  filesToMove <- filesAfter[ ! filesAfter %in% c(codeFiles, templatedFiles) ]
  # TODO do not move layout files either

  # prepare destination directories
  filesDestinations <- str_replace(filesToMove, fixed("/source/"), "/build/")
  allDirs <- unique(dirname(filesDestinations))
  l_ply(allDirs, dir.create, showWarnings=FALSE, recursive=TRUE)

  # copy all files
  message("Moving files")
  status <- file.copy(filesToMove, filesDestinations, overwrite=TRUE)

  # remove files generated by code or rendered
  file.remove(generatedFiles)

  return(invisible(status))
}


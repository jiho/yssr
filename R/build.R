#' Build the website
#'
#' @param dir directory containing the website (contains "source")
# @param exclude character string (regular expression) designating files to exclude from being copied in the destination directory. By default exclude xls, xlsx and csv files which are usually used to contain data from which the website is generated
# TODO add exclude here
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#' @importFrom stringr str_c str_replace str_detect fixed
#' @importFrom plyr laply l_ply
build <- function(dir=getwd(), ...) {

  # TODO use setwd() here and then work with relative paths

  # get the initial state of the website source
  initState <- get_info(list_site_files(dir=dir, ...))
  if ( nrow(initState) == 0 ) {
    stop("Cannot find source directory or source directory empty")
  }
  
  # initialise the list of files to be processed

  # get previous state of the source directory
  dir <- normalizePath(dir)
  stateFile <- str_c(dir, "/build/.yssrstate")
  if ( file.exists(stateFile) ) {
    state <- dget(stateFile)
  } else {
    # if it does not exist, make all files seem changed (by setting their size to something impossible)
    state <- initState
    state$size <- -1
  }
  
  # set the files that changed to be processed
  changes <- compare_state(state, initState)
  files <- changes$changed

  if ( length(files) == 0) {
    # message("Everything up to date")

  } else {

    # the current state becomes the old state
    old <- initState

    while ( length(files) > 0 ) {

      # select the first file
      file <- files[1]

      # process the file according to its role
      # - remove "deleted" files from destination
      # - copy "other" files to destination
      # - run code files in place
      # - render template files to destination
      type <- file_role(file)
      class(file) <- as.character(type)
      process(file)

      # get the new state of the site (files can have been created/modified)
      new <- get_info(list_site_files(dir=dir, ...))

      # detect changes and add changed files to be (re)processed
      changes <- compare_state(old=old, new=new)
      files <- unique(c(files, changes$changed))
      # # warn for un-expected behaviour
      # if ( length(changes$deleted) ) {
      #   warning("Files ", str_c(changes$deleted, collapse=", "), " where deleted")
      # }
      # if ( length(changes$modified) ) {
      #   warning("Files ", str_c(changes$modified, collapse=", "), " where modified")
      # }

      # pop the processed files from the stack
      files <- files[-1]

      # update the state
      old <- new
    }

    # cleanup files created since the start of the function
    changes <- compare_state(old=initState, new=new)
    if ( length(changes$added) > 0 ) {
      message("   clean (", length(changes$added), " files created by scripts in the source directory)")
      file.remove(changes$added)
    }

    # save state
    new <- get_info(list_site_files(dir=dir, ...))
    dput(new, file=stateFile)
  }

  return(invisible(NULL))
}

#' Watch the website source for changes and process files accordingly
#'
#' @inheritParams build
#' @param interval interval, in s, at which to watch for changes
#' @export
watch <- function(dir=getwd(), interval=1, ...) {
  message("Watching website \"", dir, "\". Press Ctrl+C to stop.")
  while ( TRUE ) {
    build(dir=dir, ...)
    Sys.sleep(interval)
  }
}

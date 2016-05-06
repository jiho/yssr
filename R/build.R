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

  # get the current state of the website source
  init_state <- state(dir)
  current_state <- init_state
  
  system("touch test/source/phones.csv")
  
  # get previous state of the source directory
  old_state <- get_state(dir)
  browser()
  
  # set the files that changed to be processed
  changes <- compare_state(old_state, current_state)


  while ( nrow(changes) > 0 ) {
    # process all files in order
    for (i in 1:nrow(changes)) {
      # process the file according to its role
      # - remove "deleted" files from destination
      # - copy "other" files to destination
      # - run code files in place
      # - render template files to destination
      file <- changes$path[i]
      class(file) <- file_role(file)
      process(file)
    }

    # the previously current state becomes the old state
    old_state <- current_state
    # recompute the new state
    current_state <- state(dir)
    # and compute changes
    changes <- compare_state(old_state, current_state)
    # when there are still changes, re-run the loop
    
    # TODO break after 100 runs of the loop and error out
  }

  # cleanup files created since the start of the function
  changes <- compare_state(init_state, current_state)
  added <- dplyr::filter(changes, status == "added")
  n_added <- nrow(added)
  if ( n_added > 0 ) {
    message("   clean (", n_added, " files created in the source directory)")
    file.remove(n_added$path)
  }

  # save state
  current_state <- state(dir)
  save_state(current_state)
  # message("Everything up to date")

  return(invisible(current_state))
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

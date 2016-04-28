# Compute the state of the directory dir
#
# List all files, exclude some, get the modification time and size of the remaining ones.
#
# @param dir base directory of the site
# @param exclude vector of \emph{globbing} patterns to exclude
# 
# @return A data.frame with columns path, mtime and size
state <- function(dir, exclude=c("*/.git*", "*/.svn*", "*.DS_Store*", "*._*", "Thumbs.db")) {
  source_dir <- source_dir(dir)
 
  # list all files, recursively, including hidden files
  path <- list.files(source_dir, recursive=TRUE, all.files=TRUE, full=TRUE)
  if ( length(path) == 0 ) {
    stop("Cannot find source directory or source directory empty")
  }

  # exclude some files: version control, Mac OS stuff, Windows stuff, etc.
  path <- path[! str_detect_any(path, pattern=exclude)]
  # TODO exclude files based on user configuration

  # get modification date and size
  mtime <- file.mtime(path)
  size <- file.size(path)

  state <- data.frame(path, mtime, size)
  return(state)
}

# Save the state as a text file to the disk
#' @import stringr
put_state <- function(x, dir) {
  dput(x, file=stringr::str_c(build_dir(dir), "/.yssrstate"))
}

# Get the state saved to disk
#' @import stringr
get_state <- function(dir) {
  dget(file=stringr::str_c(build_dir(dir), "/.yssrstate"))
}

# Compare states and compute which files need processing
#
# @param previous_state, current_state two `state` data.frames as computed by \code{\link{state}}
#
# @return A state-like data.frame with only the files to process
compare_state <- function(previous_state, current_state) {
  # find which files changed in anyway
  deleted_modified <- dplyr::setdiff(previous_state, current_state)$path
  deleted <- dplyr::setdiff(previous_state$path, current_state$path)
  added <- dplyr::setdiff(current_state$path, previous_state$path)
  modified <- dplyr::setdiff(deleted_modified, deleted)
  
  # make a list of files and their status
  # NB: we merge previous_state and current_state to track deleted files
  files <- data.frame(path=unique(c(previous_state$path, current_state$path)))
  # by default files are identical
  files$status <- "identical"
  # except when found not to be
  row.names(files) <- files$path
  files[deleted, "status"] <- "deleted"
  files[added, "status"] <- "added"
  files[modified, "status"] <- "modified"
  row.names(files) <- NULL

  # determine file role from extension, path, etc.
  files$role <- sapply(files$path, file_role)

  # track dependencies between files
  deps <- plyr::aaply(files, 1, function(x) {
    # only code files can depend on other files
    if (x$role %in% c("code", "code_markup")) {
      # track mention of other files in this file's content
      content <- readr::read_file(x$path)
      deps <- stringr::str_detect(content, stringr::fixed(basename(files$path)))
      # TODO identically named files in various levels of the hierarchy cause problem here
      #      to solve it we should detect the path relative to the current file, for each file => HARD
    } else {
      deps <- rep(FALSE, times <- nrow(files))
    }
    return(deps)
  }, .expand=F)
  
  # all files depend on templates
  deps[,files$role == "template"] <- TRUE
  # TODO check whether we need to avoid having template depend on themselves

  # no file can depend on a deleted file
  deps[,files$status == "deleted"] <- FALSE

  # TODO check for circular dependencies
  
  # process all non-identical files
  changed <- files$status != "identical"
  # or files that depend on changed files
  depends_on_changed <- apply(t(deps) & changed, 2, any)
  
  files_to_process <- files[changed | depends_on_changed,]

  return(files_to_process)
}


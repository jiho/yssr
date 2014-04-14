#' Initialise an yssr website
#'
#' @param dir name/path to a directory to hold the website. Will be created if it does not exists
#' 
#' @importFrom stringr str_c
#' @export
init <- function(dir=getwd(), ...) {

  # remove final slash, perform path expansion
  dir <- normalizePath(dir, mustWork=FALSE)
  sourceDir <- str_c(dir, "/source")
  
  # prepare structure
  dir.create(str_c(sourceDir, "/layouts"), recursive=TRUE)  
  
  # add a layout template
  cat("<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>YSSR</title>
  </head>
  <body>
    <%= yield %>
  </body>
</html>
", file=str_c(sourceDir, "/layouts/main.brew"))

  # add content
  cat("Hello World!
  
This is an R site

```{r}
2 + 2
```
", file=str_c(sourceDir, "/index.Rmd"))

  setwd(dir)

  message("All set. Moving to directory:\n  ", dir, "\nNow call render()")

  return(invisible(dir))
}
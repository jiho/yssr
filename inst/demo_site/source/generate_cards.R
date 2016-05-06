d <- read.csv("phones.csv")

dir.create("cards", showWarnings=FALSE)

library("plyr")
library("stringr")

a_ply(d, 1, function(x) {
  cat(
    "# ", x$Name, "\n\n",
    "*", x$Title, "*\n\n",
    "Phone: ", x$Phone,
    sep="", file=str_c("cards/", x$index, ".md")
  )
})

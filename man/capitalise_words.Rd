% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/capitalise.R
\name{capitalise_words}
\alias{capitalise_words}
\alias{capitalize_words}
\title{Capitalise words in a string}
\usage{
capitalise_words(x, sep = fixed(" "))

capitalize_words(x)
}
\arguments{
\item{x}{a vector of character strings}

\item{sep}{separator between words. Defaults to single space.}
}
\description{
Turn the first letter of each word in a string into its capital version
}
\examples{
capitalise_words("the quick red fox jumps over the lazy brown dog")
}
\seealso{
\code{\link[base]{toupper}}, \code{\link{capitalise}}
}

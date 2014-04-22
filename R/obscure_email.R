#' Obscure email address
#'
#' Obscure email address by converting characters to their unicode hex code. This procedure will indeed fool many, if not most, address-harvesting bots, but it definitely won’t fool all of them.
#' 
#' @param email character string which is an email address
#'
#' @importFrom stringr str_c
#' @export
obscure_email <- function(email) {
  
  # convert each character of the email adress into its unicode integer
  unicode_characters <- utf8ToInt(email)
  # convert integers into hex mode
  hex_characters <- as.hexmode(unicode_characters)
  # make those html entities
  entities <- str_c("&#x", hex_characters, ";")
  # reassemble the address, now encoded
  out <- str_c(entities, collapse="")

  return(out)
}

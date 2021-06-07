
#' Say "Bonjour 2 times !" with a name.
#'
#' @param nom Name of the person to say " Bonjour !" (character).
#' @param verbose Print in console (boolean).
#'
#' @return A character string "Bonjour! Name Bonjour! Name"
#' @export
#'
#' @examples
bonjour2 <- function(
  nom = "Maria",
  verbose = TRUE # never use T or F
){
  sentence <- paste("Bonjour!", nom, ". Bonjour!", nom)
  if(verbose)
    cat(sentence)
  return(sentence)
}

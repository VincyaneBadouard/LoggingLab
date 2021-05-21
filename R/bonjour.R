#' Say "Bonjour !" with a name
#'
#' @param nom (character) Name of the person to say bonjour.
#' @param verbose (boolean) Print in console
#'
#' @return a character string ""Bonjour! name"
#' @export
#'
#' @examples
#' bonjour()
bonjour <- function(
  nom = "Maria",
  verbose = TRUE # never use T or F
){
  sentence <- paste("Bonjour!", nom)
  if(verbose)
    cat(sentence)
  return(sentence)
}

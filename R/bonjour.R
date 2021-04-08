#' Say "Bonjour !" with a name
#'
#' @param nom (character) Name of the person to say bonjour.
#' @param verbose (boolean) Print in console
#'
#' @return
#' @export
#'
#' @examples
#' bonjour()
bonjour <- function(
  nom = "Maria",
<<<<<<< HEAD
  verbose = TRUE # never use T or F
=======
  verbose = T
>>>>>>> c69e0d5da9136d607b27dd63284c2fdd0e4d6e04
){
  sentence <- paste("Bonjour!", nom)
  if(verbose)
    cat(sentence)
  return(sentence)
}

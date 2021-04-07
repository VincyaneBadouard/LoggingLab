#' bonjour
#'
#' Description.
#'
#' @param nom char. Name of the person to say boonjour.
#' @param verbose bool. Print in console
#'
#' @return string
#'
#' @export
#'
#' @examples
#' bonjour()
#'
bonjour <- function(
    nom = "Maria",
    verbose = T
    ){
    sentence <- paste("Bonjour!", nom)
    if(verbose)
      cat(sentence)
    return(sentence)
  }

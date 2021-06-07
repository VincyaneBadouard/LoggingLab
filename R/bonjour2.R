#' Title
#'
#' @param nom
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
bonjour2 <- function (
  nom="Maria", verbose = TRUE)
{
  sentence <- paste ("Bonjour", nom)
  if(verbose)
    cat(sentence)
  return(sentence)
}

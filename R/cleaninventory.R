#' Select trees according to the protocol
#'
#'@description select trees according to the protocol: alive, unique, at the right place & of the required minimum protocol diameter
#'
#' @param inventory (dataframe) Your inventory data or the "Paracou6_2016" test data#'
#' @return cleaninventory (dataframe).Stop the function if the tree identifiers (idTree) are not unique, or if there are different plots in the inventory.
#' @export
#'
#' @examples
#'
#' data(Paracou6_2016)
#' cleaninventory(inventory = Paracou6_2016)
#'
cleaninventory <- function(
  inventory
){

  # # to test inside the function
  # rm(list = ls())
  # data(Paracou6_2016)
  # inventory <- Paracou6_2016
  # rm(Paracou6_2016)

if (!("DBH" %in% names(inventory))) {add_column(inventory, DBH = NA) #if DBH doesn't exist create it
    inventory$DBH = inventory$CircCorr/pi} # and compute it

  inventory <- inventory %>%
    filter(CodeAlive == "TRUE") %>% #only alive trees
    filter(DBH >= 10) # DBH >= 10, Circ = perimeter of the circle =! diameter !

  if (any(duplicated(inventory$idTree)))
    stop ("Tree identifiers (idTree) are not unique.") # stop function if the tree identifiers (idTree) are not unique

  # length(unique(inventory$Plot)) == 1
  if (!all(inventory$Plot == inventory$Plot[1])) #all the Plot values are equal?
    stop ("Your inventory concerns different plots (Plot). Our function simulates logging at the plot level.")

  if (!length(unique(inventory$Plot))== 1) #all is the same year?
    stop ("Your inventory concerns different years (CensusYear). Our function simulates logging at 1 year scale.")

  # Remove out-plot trees (A faire)
}


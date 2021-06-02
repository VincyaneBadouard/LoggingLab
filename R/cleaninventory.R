#' Select trees according to the protocol
#'
#'@description Select trees according to the protocol: alive, unique, at the right place & of the required minimum protocol diameter
#'
#' @param inventory Your inventory data or the "Paracou6_2016" test data (data.frame)
#' @return inventory (data.frame).Stop the function if the tree identifiers (idTree) are not unique, or if there are different plots in the inventory.
#' @export
#' @importFrom tibble add_column
#' @importFrom dplyr filter
#' @importFrom dplyr mutate

#'
#' @examples
#'
#' data(Paracou6_2016)
#' cleaninventory(inventory = Paracou6_2016)
#'
cleaninventory <- function(
  inventory
){

  # Argument check
  if (!inherits(inventory, "data.frame"))
    stop("inventory must be a data.frame")

  if (!("DBH" %in% names(inventory))) {
    inventory <- mutate(inventory, DBH = ifelse(is.na(CircCorr), Circ/pi, CircCorr/pi))
  } #if DBH (cm) doesn't exist create it

  inventory <- inventory %>%
    filter(CodeAlive == "TRUE") %>% #only alive trees
    filter(DBH >= 10) # DBH >= 10, Circ = perimeter of the circle? =! diameter !

  GoodData <- TRUE # usefull boolean for later

  GeneralStop <- "" # empty character string to stock all the warnings

  if (any(duplicated(inventory$idTree))) {
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop, "Tree identifiers (idTree) are not unique.")# stop function if the tree identifiers (idTree) are not unique
  }

  # length(unique(inventory$Plot)) == 1
  if (!length(unique(inventory$Plot))== 1){ #all the Plot values are equal?
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop,"Your inventory concerns different plots (Plot). Our function simulates logging at the plot level.")
  }

  if (!length(unique(inventory$CensusYear))== 1) {#all is the same year?
    GoodData <- FALSE
    GeneralStop <- paste (GeneralStop,"Your inventory concerns different years (CensusYear). Our function simulates logging at 1 year scale.")
  }

  if (!GoodData)# inverse value of the object
    stop(paste ("Your inventory does not comply", GeneralStop))


  return(inventory)

  # Remove out-plot trees (A faire)
}


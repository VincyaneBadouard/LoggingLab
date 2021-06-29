#' futurereserve
#'
#' @param inventory (data.frame)
#' @param speciescriteria (data.frame)
#' @param otherloggingparameters (list)
#'
#' @return
#' @export
#'
#' @examples
#' inventory <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#' diversification = TRUE)$inventory
#'
#' HVinit <- harvestable(ONFGuyafortaxojoin(addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))),
#' diversification = TRUE)$HVinit
#'
#' inventory <- selected(inventory, type = "manual", fuel = "0", diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
#' otherloggingparameters = loggingparameters(), VO = 80, HVinit = HVinit)$inventory
#'
#' futurereserve(inventory)
#'
futurereserve <- function(
  inventory,
  speciescriteria = SpeciesCriteria,
  otherloggingparameters = loggingparameters()

){

  # Arguments check

  # if(!any(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
  #   stop("The 'inventory' and 'speciescriteria' arguments of the 'futurereserve' function must be data.frame")
  #
  # if(!inherits(otherloggingparameters, "list"))
  #   stop("The 'otherloggingparameters' argument of the 'futurereserve' function must be a list")


  #Future: select essence and diameters

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(Commercial == "1" &
                                    (DBH >= otherloggingparameters$FutureTreesMinDiameter & DBH < MinFD),
                                  "future", LoggingStatus))

  #Reserve
  # Randomly select the reserved trees (among the futures, as many as the number of trees exploited):

  StemNbr <- sum(as.numeric(inventory$Selected == "1"), na.rm = TRUE)#29 selected ind
  ReserveRows <- sample(which(inventory$LoggingStatus == "future"), size = StemNbr, replace = F)

  inventory$LoggingStatus[ReserveRows] <-"reserve"

  return(inventory)

}

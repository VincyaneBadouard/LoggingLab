#' .futurereserve
#'
#' @param inventory (data.frame)
#' @param speciescriteria (data.frame)
#' @param otherloggingparameters (list)
#'
#' @return
#' @export
#'
#' @examples
#'
.futurereserve <- function(
  inventory,
  speciescriteria = SpeciesCriteria,
  otherloggingparameters = loggingparameters()

){
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

}

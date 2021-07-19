#' timberharvestedvolume
#'
#' @param inventory (data.frame)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param otherloggingparameters (list)
#'
#' @return A list with the logged volume (LoggedVolume)
#' and when fuel = "2", the logged volume without the hollow trees (NoHollowLoggedVolume),
#' NoHollowLoggedVolume = NULL when fuel = "0" or "1
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' inventory <- addtreedim(cleaninventory(inventorycheckformat(Paracou6_2016)))
#'
#' inventory <- treeselection(inventory, SpeciesCriteria,
#' type ="manual", fuel = "2", objective = 20, diversification = TRUE, specieslax = FALSE,
#' objectivelax = FALSE, otherloggingparameters = loggingparameters())$inventory
#'
#' inventory <- secondtrailsopening()$inventory
#'
#' inventory <- treefelling()$inventory
#'
#' inventory <- adjustedsecondtrails()$inventory
#'
#' timberharvestedvolume(inventory, fuel = "2", otherloggingparameters = loggingparameters())
#' }
#'
timberharvestedvolume <- function(
  inventory,
  fuel,
  otherloggingparameters = loggingparameters()
){

  LoggedTable <- inventory %>%
    filter(Logged == "1")

  if (fuel !="2") {                    # no hollow trees exploitation

    LoggedVolume <- sum(LoggedTable$TreeHarvestableVolume)

    outputs <- list(LoggedVolume = LoggedVolume,
                    NoHollowLoggedVolume = NULL
    )

  }

  if (fuel =="2") {                   # with hollow trees exploitation

    HollowTable <- inventory %>%
      filter(ProbedHollow == "1")

    NoHollowLoggedVolume <- sum(LoggedTable$TreeHarvestableVolume)
    LoggedVolume <- sum(NoHollowLoggedVolume +
                          1-(otherloggingparameters$TreeHollowPartForFuel)*(HollowTable$TreeHarvestableVolume))

    outputs <- list(LoggedVolume = LoggedVolume,
                    NoHollowLoggedVolume = NoHollowLoggedVolume
    )
  }


  return(outputs)
}

#' timberharvestedvolume
#'
#' @param inventory (data.frame)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param advancedloggingparameters (list)
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
#' data(Paracou6_2016)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016))
#'
#' inventory <- treeselection(inventory, SpeciesCriteria,
#' scenario ="manual", fuel = "2", objective = 20, diversification = TRUE, specieslax = FALSE,
#' objectivelax = FALSE, advancedloggingparameters = loggingparameters())$inventory
#'
#' inventory <- secondtrailsopening()$inventory
#'
#' inventory <- treefelling()$inventory
#'
#' inventory <- adjustedsecondtrails()$inventory
#'
#' timberharvestedvolume(inventory, fuel = "2", advancedloggingparameters = loggingparameters())
#' }
#'
timberharvestedvolume <- function(
  inventory,
  fuel,
  advancedloggingparameters = loggingparameters()
){

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  LoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  # Arguments check

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
                          1-(advancedloggingparameters$TreeHollowPartForFuel)*(HollowTable$TreeHarvestableVolume))

    outputs <- list(LoggedVolume = LoggedVolume,
                    NoHollowLoggedVolume = NoHollowLoggedVolume
    )
  }


  return(outputs)
}

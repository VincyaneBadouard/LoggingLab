#' Future & reserve trees designation
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param speciescriteria Table of species exploitability criteria : species
#'   names, economic interest level, minimum and maximum felling diameter, in
#'   the same format as \code{\link{SpeciesCriteria}} (data.frame)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return input inventory with selected future and reserve trees
#' (LoggingStatus = "future"/"reserve"),
#' for your logging criteria (\code{\link{SpeciesCriteria}})
#'
#' @details **Future** trees are all trees satisfying the following conditions:
#'  - species of 1st economic rank
#'  - DBH between 35cm ('FutureTreesMinDiameter') and the species MinFD
#'  or UpMinFD if it has been raised for its species.
#'  - in the prospection units (harvestable areas)
#'
#'  **Reserve** trees are randomly chosen among future trees so that
#'  the number of reserve trees is equal to the number of harvested trees.
#'
#' @seealso  \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'   \code{\link{loggingparameters}}
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' data(Paracou6_2016)
#' data(ParamCrownDiameterAllometry)
#' data(ForestZoneVolumeParametersTable) # The volume parameters data in the global environment
#' data(SpeciesCriteria)
#' data(DTMParacou)
#' data(HarvestableAreaOutputsCable)
#'
#' scenario <- "RIL2"
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- commercialcriteriajoin(inventory, SpeciesCriteria)
#'
#' harvestableOutputs <- harvestable(inventory,
#'  topography = DTMParacou,
#'  diversification = TRUE,
#'  specieslax = FALSE,
#'  plotslope = HarvestableAreaOutputsCable$PlotSlope,
#'  harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#'  scenario = scenario,
#'  advancedloggingparameters = loggingparameters())
#'
#' inventory <- harvestableOutputs$inventory
#' HVinit <- harvestableOutputs$HVinit
#'
#' inventory <- selected(inventory, scenario = "manual", fuel = "0",
#' diversification = TRUE, specieslax = FALSE, objectivelax = TRUE,
#' topography = DTMParacou,
#' advancedloggingparameters = loggingparameters(), VO = 125,
#'  HVinit = HVinit)$inventory
#'
#' futurereserve(inventory, SpeciesCriteria)
#'
futurereserve <- function(
  inventory,
  speciescriteria,
  advancedloggingparameters = loggingparameters()

){

  # Arguments check

  # if(!all(unlist(lapply(list(inventory, speciescriteria), inherits, "data.frame"))))
  #   stop("The 'inventory' and 'speciescriteria' arguments of the 'futurereserve' function must be data.frame")
  #
  # if(!inherits(advancedloggingparameters, "list"))
  #   stop("The 'advancedloggingparameters' argument of the 'futurereserve' function must be a list")

  # Global variables
  Accessible <- Circ <- CircCorr <- CodeAlive <- CommercialLevel <- NULL
  Condition <- DBH <- PU <- NULL
  DeathCause <- DistCriteria <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCriteria <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  #Future: select essence and diameters

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(CommercialLevel == "1" & Selected != "1" & PU == TRUE &
                                    ((Up == "0" &
                                        (DBH >= advancedloggingparameters$FutureTreesMinDiameter & DBH < MinFD))
                                     | (Up == "1" &
                                          (DBH >= advancedloggingparameters$FutureTreesMinDiameter & DBH < UpMinFD))
                                     ),
                                  "future", LoggingStatus))

  #Reserve
  # Randomly select the reserved trees (among the futures, as many as the number of trees exploited):

  StemNbr <- sum(as.numeric(inventory$Selected == "1"), na.rm = TRUE)
  FutureNbr <- sum(as.numeric(inventory$LoggingStatus == "future"), na.rm = TRUE)


  if(StemNbr < FutureNbr){
  ReserveRows <- sample(which(inventory$LoggingStatus == "future"), size = StemNbr, replace = F)

  inventory$LoggingStatus[ReserveRows] <-"reserve"
  }

  if(StemNbr >= FutureNbr){
    inventory <- inventory %>%
      mutate(LoggingStatus = ifelse(LoggingStatus == "future", "reserve", LoggingStatus))
  }


  return(inventory)

}

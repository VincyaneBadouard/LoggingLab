#' futurereserve
#'
#' @param inventory Your inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param speciescriteria Table of species exploitability criteria : species
#'   names, economic interest level, minimum and maximum felling diameter, in
#'   the same format of \code{\link{SpeciesCriteria}} (data.frame)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list) MainTrail (multiline)
#'
#' @return your inventory with selected future and reserve trees, for your
#'   logging criteria (\code{\link{SpeciesCriteria}})
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
#' data(PlotSlope)
#'
#' inventory <- addtreedim(inventorycheckformat(Paracou6_2016),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- ONFGuyafortaxojoin(inventory, SpeciesCriteria)
#'
#' harvestableOutputs <- harvestable(inventory, diversification = TRUE,
#'  specieslax = FALSE,
#' topography = DTMParacou, plotslope = PlotSlope,
#' advancedloggingparameters = loggingparameters())
#'
#' inventory <- harvestableOutputs$inventory
#' HVinit <- harvestableOutputs$HVinit
#'
#' inventory <- selected(inventory, scenario = "manual", fuel = "0",
#' diversification = TRUE, specieslax = FALSE, objectivelax = FALSE,
#' topography = DTMParacou,
#' advancedloggingparameters = loggingparameters(), VO = 30,
#'  HVinit = HVinit)$inventory
#'
#' futurereserve(inventory)
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
  Accessible <- Circ <- CircCorr <- CodeAlive <- Commercial <- NULL
  Commercial.genus <- Commercial.species <- Condition <- DBH <- NULL
  DeathCause <- DistCrit <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  PlotTopo <- ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCrit <- Species <- Species.genus <- NULL
  SpeciesCriteria <- Taxo <- Taxo.family <- Taxo.genus <- Taxo.species <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD <- UpMinFD.genus <- NULL
  UpMinFD.species <- VernName.genus <- VernName.genus.genus <- NULL
  VernName.species <- VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL
  alpha <- alpha.family <- alpha.genus <- alpha.species <- bCoef <- NULL
  beta.family <- beta.genus <- beta.species <- geometry <- idTree <- NULL


  #Future: select essence and diameters

  inventory <- inventory %>%
    mutate(LoggingStatus = ifelse(Commercial == "1" &
                                    ((Up == "0" &
                                        (DBH >= advancedloggingparameters$FutureTreesMinDiameter & DBH < MinFD))
                                     | (Up == "1" &
                                          (DBH >= advancedloggingparameters$FutureTreesMinDiameter & DBH < UpMinFD))),
                                  "future", LoggingStatus))

  #Reserve
  # Randomly select the reserved trees (among the futures, as many as the number of trees exploited):

  StemNbr <- sum(as.numeric(inventory$Selected == "1"), na.rm = TRUE)#29 selected ind
  ReserveRows <- sample(which(inventory$LoggingStatus == "future"), size = StemNbr, replace = F)

  inventory$LoggingStatus[ReserveRows] <-"reserve"

  return(inventory)

}

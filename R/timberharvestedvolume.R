#' Compute the timber harvested volume
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#' @param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'   "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'   vignette)
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with the logged volume (TimberLoggedVolume)
#' and when fuel = "2", the logged volume without the hollow trees
#'   (NoHollowTimberLoggedVolume).
#'   NoHollowTimberLoggedVolume is the logged volume (TimberLoggedVolume)
#'   when fuel = "0" or "1".
#'
#' @details When fuel is "2", by default, 2/3 of the log of a hollow tree will
#'   be usable as timber and 1/3 as fuel wood ('TreeHollowPartForFuel'). When
#'   fuel is not "2", hollow trees are not used for timber nor fuel wood. Dead
#'   trees from the operation (trails, secondary windfall) are never exploited
#'   as timber, but as fuel wood if chosen.
#'
#' @export
#'
#' @importFrom dplyr filter
#'
#' @examples
#' data(Paracou6_2016) # inventory
#' data(DTMParacou) # topography
#' data(MainTrails)  # MainTrails
#' data(HarvestableAreaOutputsCable)
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(inventory,
#' topography = DTMParacou,
#' speciescriteria = SpeciesCriteria,
#' scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
#' winching = "0", specieslax = FALSE, objectivelax = TRUE,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' advancedloggingparameters = loggingparameters())$inventory)
#'
#' timberharvestedvolume(inventory, scenario = "manual", fuel = "2",
#' advancedloggingparameters = loggingparameters())
#'
timberharvestedvolume <- function(
  inventory,
  scenario,
  fuel = NULL,
  advancedloggingparameters = loggingparameters()
){

  #### Global variables ####
  Accessible <- Circ <- CircCorr <- CodeAlive <- NULL
  Condition <- DBH <- NULL
  DeathCause <- DistCriteria <- Family <- NULL
  ForestZoneVolumeParametersTable <- Genus <- Logged <- NULL
  TimberLoggedVolume <- LoggingStatus <- MaxFD <- MaxFD.genus <- NULL
  MaxFD.species <- MinFD <- MinFD.genus <- MinFD.species <- NULL
  NoHollowTimberLoggedVolume <- ParamCrownDiameterAllometry <- PlotSlope <- NULL
  ProbedHollow <- ProbedHollowProba <- ScientificName <- NULL
  Selected <- Slope <- SlopeCriteria <- Species <- Species.genus <- NULL
  SpeciesCriteria  <- geometry <- idTree <- NULL
  TreeFellingOrientationSuccess <- TreeHarvestableVolume <- NULL
  TreeHeight <- TrunkHeight <- Up <- UpMinFD  <- NULL
  VolumeCumSum <- Xutm <- Yutm <- aCoef <- NULL


  #### Arguments check ####
  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'timberharvestedvolume' function must be a data.frame")

  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"||
           scenario == "RIL3"|| scenario == "RIL3fuel"||
           scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'timberharvestedvolume' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'timberharvestedvolume' function must be '0', '1', '2' or NULL")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'timberharvestedvolume' function must be a list")


  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel)

  fuel <- scenariosparameters$fuel


  #### Function ####
  LoggedTable <- inventory %>%
    filter(Selected == "1" & ProbedHollow == "0") # Logged trees

  Healthy <- sum(LoggedTable$TreeHarvestableVolume)

  if (fuel !="2") {                    # no hollow trees exploitation

    NoHollowTimberLoggedVolume <- TimberLoggedVolume <- Healthy

  }

  if (fuel =="2") {                   # with hollow trees exploitation

    HollowTable <- inventory %>%
      filter(Selected == "1" & ProbedHollow == "1")

    NoHollowTimberLoggedVolume <- Healthy

    if(nrow(HollowTable) > 0){
      # heathy + (1 -part for fuel) * hollow's volume)
      TimberLoggedVolume <- sum(NoHollowTimberLoggedVolume +
                                  (1-advancedloggingparameters$TreeHollowPartForFuel) *
                                  sum(HollowTable$TreeHarvestableVolume))
    }else if(nrow(HollowTable) == 0){

      TimberLoggedVolume <- NoHollowTimberLoggedVolume # no probed hollow trees
    }

  }

  outputs <- list(TimberLoggedVolume = TimberLoggedVolume,
                  NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume
  )


  return(outputs)
}

#' Compute the harvestable fuel wood biomass and the logging residual biomass
#'
#' @description Computes the harvestable fuel wood biomass in healthy trees
#'   exploited for timber (their unused part), in the hollow trees and in the
#'   damage trees (caused by trails, secondary windfall). Computes also the the
#'   unused degraded tree biomass.
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#' @param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'   "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'   vignette)
#'
#'@param fuel Fuel wood exploitation:
#'"0": no exploitation
#'"1": exploitation of damage and unused part of logged trees
#'"2": exploitation of hollow trees, damage and and unused part of the log
#'
#' @param TimberLoggedVolume All the logged volume (in m3) (numeric)
#'
#' @param NoHollowTimberLoggedVolume The healthy logged volume (in m3)
#'   (numeric)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with the logging residual biomass (ton), the fuel wood biomass
#'   (ton) when fuel wood exploitation is chosen and the inventory with the added
#'   columns:
#'   - TimberLoggedBiomass (ton): The timber biomass logged by tree
#'   - LogBiomass (ton): the biomass of the log (equivalent to the
#'                 'TreeHarvestableVolume' but in biomass)
#'   - PurgeVolume (m3): the volume of the purge of each harvestable tree
#'   - PurgeBiomass (ton): the biomass of the purge of each harvestable tree
#'   - CrownBiomass (ton): the biomass of the tree crown
#'   - FuelWoodBiomass (ton): the fuel wood biomass harvestable by tree
#'   - LoggingResidualBiomass (ton): the unused degraded tree biomass.
#'
#' @export
#'
#' @details The harvestable part in fuel wood in:
#'   - The healthy trees exploited for timber: 2/3 of their crown (default value
#'     of 'CrownPartForFuel') + the unused part of the trunk ('Purge') (Default =
#'     0.14 m3 of purge/m3 of volume of timber harvested).
#'
#'   - The hollow trees if logged: 1/3 (default value of 'TreeHollowPartForFuel')
#'     of the log + 2/3 of their crown (default value of 'CrownPartForFuel')
#'
#'  - The damage trees (trails, secondary windfall): their trunk
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotMask)
#' data(ForestZoneVolumeParametersTable)
#' data(MainTrails)
#' data(HarvestableAreaOutputsCable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#'                         volumeparameters = ForestZoneVolumeParametersTable)
#'
#' inventory <- suppressMessages(treeselection(
#'   inventory,
#'   topography = DTMParacou,
#'   speciescriteria = SpeciesCriteria,
#'   scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
#'   winching = "0", specieslax = FALSE, objectivelax = TRUE,
#'   harvestablearea = HarvestableAreaOutputsCable$HarvestableArea,
#'   plotslope = HarvestableAreaOutputsCable$PlotSlope,
#'   maintrails = MainTrails,
#'   harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#'   advancedloggingparameters = loggingparameters())$inventory)
#'
#' if (!("DeathCause" %in% names(inventory))){ inventory <- inventory %>%
#'   tibble::add_column(DeathCause = NA) # if "DeathCause" column doesnt exist create it
#' }
#'
#' inventory <- inventory %>%
#'   dplyr ::mutate(DeathCause = ifelse(is.na(DeathCause) &
#'                                        Selected == "1" & ProbedHollow == "0",
#'                                      "cutted", DeathCause)) %>%
#'   dplyr::mutate(DeathCause = ifelse(is.na(DeathCause) &
#'                                       Selected == "1" & ProbedHollow == "1",
#'                                     "hollowfuel", DeathCause))
#'
#' inventory[is.na(inventory$DeathCause), ]$DeathCause[1] <- "maintrail"
#' inventory[is.na(inventory$DeathCause), ]$DeathCause[2] <- "2ndtrail"
#' inventory[is.na(inventory$DeathCause), ]$DeathCause[3] <- "treefall2nd"
#' inventory[is.na(inventory$DeathCause), ]$DeathCause[4] <- "landing"
#'
#' TimberV <- timberharvestedvolume(inventory, scenario = "manual", fuel = "2",
#'                                  advancedloggingparameters = loggingparameters())
#'
#' inventory <- TimberV$inventory
#' TimberLoggedVolume <- TimberV$TimberLoggedVolume
#' NoHollowTimberLoggedVolume <- TimberV$NoHollowTimberLoggedVolume
#'
#' Rslt <- harvestablefuelwood(inventory,
#'                             scenario = "manual", fuel = "2",
#'                             TimberLoggedVolume = TimberLoggedVolume,
#'                             NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
#'                             advancedloggingparameters = loggingparameters())
#'
harvestablefuelwood <- function(
    inventory,
    scenario,
    fuel = NULL,
    TimberLoggedVolume,
    NoHollowTimberLoggedVolume,
    advancedloggingparameters = loggingparameters()
){
  #### Global variables ####
  DeathCause <- ProbedHollow <- Selected <- TreeHarvestableVolume <- AGB <- NULL
  WoodDensity <- TimberLoggedBiomass <- PurgeVolume <- NULL
  LogBiomass <- PurgeBiomass <- CrownBiomass <- NULL

  #### Arguments check ####
  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument of the 'harvestablefuelwood' function must be a data.frame")

  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"||
           scenario == "RIL3"|| scenario == "RIL3fuel"||
           scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'harvestablefuelwood' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'harvestablefuelwood' function must be '0', '1', '2' or NULL")

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument
         of the 'harvestablefuelwood' function must be a list")

  if(!all(unlist(lapply(list(TimberLoggedVolume, NoHollowTimberLoggedVolume), inherits, "numeric"))))
    stop("The 'TimberLoggedVolume' and 'NoHollowTimberLoggedVolume' arguments
         of the 'harvestablefuelwood' function must be numeric")

  # initial inventory
  inventory0 <- inventory

  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, fuel = fuel)

  fuel <- scenariosparameters$fuel


  #### Function ####

  # From volume to biomass: Biomass (t) = Volume (m3) * WoodDensity (g/cm^3)
  # 1 t/m3 = 1 g/cm3

  # TimberLoggedBiomass
  inventory <- inventory %>%
    mutate(TimberLoggedBiomass = TimberLoggedVolume * WoodDensity)

  # LogBiomass
  inventory <- inventory %>%
    mutate(LogBiomass = TreeHarvestableVolume * WoodDensity)

  # Purge only of healthy trees (0 for the others)
  inventory <- inventory %>%
    mutate(PurgeVolume =
             ifelse(Selected == "1" & ProbedHollow == "0",
                    advancedloggingparameters$Purge * TreeHarvestableVolume, 0))

  # PurgeBiomass
  inventory <- inventory %>%
    mutate(PurgeBiomass = PurgeVolume * WoodDensity)

  # CrownBiomass
  # Tree biomass - log biomass
  inventory <- inventory %>% mutate(CrownBiomass = AGB - LogBiomass)

  # Healthy trees
  # 2/3 of CrownBiomass + PurgeBiomass
  inventory <- inventory %>%
    mutate(FuelWoodBiomass = ifelse(Selected == "1" & ProbedHollow == "0",
                                    advancedloggingparameters$CrownPartForFuel * CrownBiomass + # 2/3 of CrownBiomass
                                      PurgeBiomass, NA)) # + purge

  # Hollow trees
  # 2/3 of CrownBiomass + 1/3 LogBiomass
  inventory <- inventory %>%
    mutate(FuelWoodBiomass =
             ifelse(Selected == "1" & ProbedHollow == "1",
                    advancedloggingparameters$CrownPartForFuel * CrownBiomass + # 2/3 of CrownBiomass
                      advancedloggingparameters$TreeHollowPartForFuel * LogBiomass, # 1/3 log
                    FuelWoodBiomass))

  # Damage trees
  # LogBiomass
  inventory <- inventory %>%
    mutate(FuelWoodBiomass = ifelse(DeathCause %in% "maintrail" |
                                      DeathCause %in% "2ndtrail" |
                                      DeathCause %in% "treefall2nd" |
                                      DeathCause %in% "landing",
                                    LogBiomass,
                                    FuelWoodBiomass))

  # LoggingResidualBiomass: biomass damage left in place
  if(fuel != "0"){
  inventory <- inventory %>%
    mutate(LoggingResidualBiomass = AGB - (TimberLoggedBiomass + FuelWoodBiomass))
  }

  if(fuel == "0"){ # No fuel wood exploitation
    inventory <- inventory %>%
      mutate(LoggingResidualBiomass = ifelse(!is.na(DeathCause),
                                             AGB - TimberLoggedBiomass, NA))
  }

  FuelWoodBiomass <- sum(inventory$FuelWoodBiomass, na.rm = TRUE)
  LoggingResidualBiomass <- sum(inventory$LoggingResidualBiomass, na.rm = TRUE)

  if(fuel == "0"){ # No fuel wood exploitation
    inventory <- inventory %>%
      mutate(FuelWoodBiomass = NA_real_)

    FuelWoodBiomass <- NULL
  }

  if(nrow(inventory) != nrow(inventory0))
    stop("The number of rows between the input inventory and the output inventory
         of the function harvestablefuelwood() is not the same.The function must be corrected.")

  outputs <- list(inventory = inventory,
                  FuelWoodBiomass = FuelWoodBiomass,
                  LoggingResidualBiomass = LoggingResidualBiomass) # biomass damage left in place


  return(outputs)

}

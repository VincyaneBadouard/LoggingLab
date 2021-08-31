#' Summary of the outputs of the loggingsimulation function
#'
#' @param x Outputs of the loggingsimulation function (list)
#'
#' @return a summary of the outputs of the loggingsimulation function:
#' - a reminder of the inputs
#' - the outgoing figures
#' - a print of the output inventory
#'
#' @export
#'
#' @examples
#' data(Paracou6_2016) # inventory
#' data(DTMParacou) # topography
#' # data() relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' Rslt <- suppressMessages(
#'   loggingsimulation(Paracou6_2016, topography = DTMParacou,
#'                     relativeelevation  = DTMParacou, speciescriteria = SpeciesCriteria,
#'                     volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'                     objective = 20, fuel = "2", diversification = TRUE, winching = "2",
#'                     directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'                     crowndiameterparameters = ParamCrownDiameterAllometry,
#'                     advancedloggingparameters = loggingparameters(), iter = 1, cores = 1)
#' )
#'
#' loggingsummary(Rslt)
#'
loggingsummary <- function(x
){

  # Arguments check
  if(!inherits(x, "list"))
    stop("The argument of the 'loggingsummary' function must be a list")

  # Reminder of INPUTS:
  cat('inventory :', x$INPUTinventory, '\n') # input inventory name
  cat('scenario :', x$scenario, '\n') # scenario,
  cat('objective :', x$objective, '\n') # objective volume
  cat('fuel :', x$fuel, '\n') # fuel
  cat('diversification :', x$diversification, '\n') # diversification
  cat('winching :', x$winching, '\n') # winching type
  cat('directionalfelling :', x$directionalfelling, '\n') # directionalfelling type
  cat('specieslax :', x$specieslax, '\n') # specieslax
  cat('objectivelax :', x$objectivelax, '\n') # objectivelax

  # Numeric values:
  # cat('Harvestable area :', x$HarvestableArea, '\n') # harvestable area (m^2)

  cat('Objective volume :', x$VO, '\n') # your objective volume (m^3) with or without a bonus (if hollow trees exploitation)

  cat('Initial harvestable volume :', x$HVinit, '\n') # the harvestable volume (m^3) with your initial criteria

  cat('Timber logged volume :', x$TimberLoggedVolume, '\n') # Logged volume (m^3) (only healthy trees if fuel != "2", healthy + hollow trees if fuel = "2")

  cat('No hollow timber logged volume :', x$NoHollowTimberLoggedVolume, '\n') # Logged volume (m^3) (only healthy trees)

  cat('Fuel wood volume :', x$FuelVolume, '\n') # Damages + purge (+ hollow trees if fuel = "2") (m^3)

  cat('Damages volume :', x$DamageVolume, '\n') # only damages (without purge and hollow trees) (m^3)

  # cat('Lost biomass :', x$LostBiomass, '\n') # Lost biomass (Mg)

  # cat('Trails density :', x$TrailsDensity, '\n') # Trails density (m/m^2)

  # The after simulation inventory (data.frame)
  print(x$inventory)

}

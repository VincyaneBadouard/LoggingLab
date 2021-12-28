#' Summary of the outputs of the loggingsimulation function
#'
#' @param x Outputs of the loggingsimulation function (list)
#'
#' @return A summary of the outputs of the loggingsimulation function:
#' - a reminder of the inputs
#' - the outgoing figures
#' - a print of the output inventory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(Paracou6_2016) # inventory
#' data(DTMParacou) # topography
#' data(VerticalCreekHeight) # relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' Rslt <- suppressMessages(
#'   loggingsimulation(
#'     Paracou6_2016, topography = DTMParacou,
#'     verticalcreekheight  = VerticalCreekHeight,
#'     speciescriteria = SpeciesCriteria,
#'     volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'     objective = 20, fuel = "2", diversification = TRUE, winching = "2",
#'     directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'     crowndiameterparameters = ParamCrownDiameterAllometry,
#'     advancedloggingparameters = loggingparameters(), iter = 1, cores = 1)
#' )
#'
#' loggingsummary(Rslt)
#' }
#'
loggingsummary <- function(x
){

  # Arguments check
  if(!inherits(x, "list"))
    stop("The argument of the 'loggingsummary' function must be a list")

  PlotArea <- unique(x$inventory$PlotArea)

  # Reminder of INPUTS:
  cat('inventory :', x$INPUTinventory, '\n') # input inventory name
  cat('Plot area :', PlotArea, 'ha\n') # Plot area,
  cat('scenario :', x$scenario, '\n') # scenario,
  cat('objective :', x$objective, 'm3/ha\n') # objective volume (m3/ha)
  cat('fuel :', x$fuel, '\n') # fuel
  cat('diversification :', x$diversification, '\n') # diversification
  cat('winching :', x$winching, '\n') # winching type
  cat('directionalfelling :', x$directionalfelling, '\n') # directionalfelling type
  cat('specieslax :', x$specieslax, '\n') # specieslax
  cat('objectivelax :', x$objectivelax, '\n') # objectivelax

  # Numeric values:
  # cat('Harvestable area :', x$HarvestableArea, 'm^2\n') # harvestable area (m^2)

  cat('Objective volume :', round(x$VO, digits = 1), 'm3\n') # your objective volume (m3)

  cat('Initial harvestable volume :', round(x$HVinit, digits = 1), 'm3, ',
      round(x$HVinit/PlotArea, digits = 1), 'm3/ha\n') # the harvestable volume (m3) with your initial criteria

  cat('Timber logged volume :', round(x$TimberLoggedVolume, digits = 1), 'm3, ',
      round(x$TimberLoggedVolume/PlotArea, digits = 1), 'm3/ha\n') # Logged volume (m3) (only healthy trees if fuel != "2", healthy + hollow trees if fuel = "2")

  cat('No hollow timber logged volume :', round(x$NoHollowTimberLoggedVolume, digits = 1), 'm3, ',
      round(x$NoHollowTimberLoggedVolume/PlotArea, digits = 1), 'm3/ha\n') # Logged volume (m3) (only healthy trees)

  cat('Fuel wood volume :', round(x$FuelVolume, digits = 1), 'm3, ',
      round(x$FuelVolume/PlotArea, digits = 1), 'm3/ha\n') # Damages + purge (+ hollow trees if fuel = "2") (m3)

  cat('Damages volume :', round(x$DamageVolume, digits = 1), 'm3, ',
      round(x$DamageVolume/PlotArea, digits = 1), 'm3/ha\n') # only damages (without purge and hollow trees) (m3)

  # cat('Lost biomass :', x$LostBiomass, 'Mg\n') # Lost biomass (Mg)

  # cat('Trails density :', x$TrailsDensity, 'm/m^2\n') # Trails density (m/m^2)

  # The after simulation inventory (data.frame)
  print(x$inventory)

}

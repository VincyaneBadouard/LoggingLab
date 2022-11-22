#' Summary of the outputs of the loggingsimulation1 function
#'
#' @param x Outputs of the loggingsimulation function (list)
#'
#' @return A summary of the outputs of the loggingsimulation1 function
#' (console display):
#' - a reminder of the inputs
#' - the outgoing figures
#' - a print of the output inventory
#' Please note that all volumes in m3/ha and biomass in ton/ha are per
#' exploitable hectare, not per plot hectare.
#'
#' @export
#'
#' @examples
#' data(LoggingSimulationOutputs) # Outputs of one logging simulation
#'
#' loggingsummary1(LoggingSimulationOutputs)
#'
loggingsummary1 <- function(x
){

  # Arguments check
  if(!inherits(x, "list"))
    stop("The argument of the 'loggingsummary1' function must be a list")

  # Reminder of INPUTS:
  cat('inventory :', x$INPUTinventory, '\n') # input inventory name
  cat('scenario :', x$scenario, '\n') # scenario,
  cat('objective :', x$objective, 'm3/harvestable ha\n') # objective volume (m3/ha)
  cat('fuel :', x$fuel, '\n') # fuel
  cat('diversification :', x$diversification, '\n') # diversification
  cat('winching :', x$winching, '\n') # winching type
  cat('directionalfelling :', x$directionalfelling, '\n') # directionalfelling type
  cat('specieslax :', x$specieslax, '\n') # specieslax
  cat('objectivelax :', x$objectivelax, '\n') # objectivelax

  # Numeric values:
  HarvestableArea <- x$HarvestableArea
  cat('Harvestable area :', round(HarvestableArea, digits = 2), 'ha\n') # harvestable area (ha)

  cat('Objective volume :', round(x$VO, digits = 1), 'm3\n') # your objective volume (m3)

  cat('Initial harvestable volume :', round(x$HVinit, digits = 1), 'm3, ',
      round(x$HVinit/HarvestableArea, digits = 1), 'm3/harvestable ha\n') # the harvestable volume (m3) with your initial criteria

  cat('Timber logged volume :', round(x$TimberLoggedVolume, digits = 1), 'm3, ',
      round(x$TimberLoggedVolume/HarvestableArea, digits = 1), 'm3/harvestable ha\n') # Logged volume (m3) (only healthy trees if fuel != "2", healthy + hollow trees if fuel = "2")

  cat('No hollow timber logged volume :', round(x$NoHollowTimberLoggedVolume, digits = 1), 'm3, ',
      round(x$NoHollowTimberLoggedVolume/HarvestableArea, digits = 1), 'm3/harvestable ha\n') # Logged volume (m3) (only healthy trees)

  cat('Fuel wood biomass :', round(x$FuelWoodBiomass, digits = 1), 'ton, ',
      round(x$FuelWoodBiomass/HarvestableArea, digits = 1), 'ton/harvestable ha\n') # Damages + purge (+ hollow trees if fuel = "2") (m3)

  cat('Damages biomass :', round(x$DamageBiomass, digits = 1), 'ton, ',
      round(x$DamageBiomass/HarvestableArea, digits = 1), 'ton/harvestable ha\n') # only damages (without purge and hollow trees) (m3)

  cat('Lost biomass :', round(x$LostBiomass, digits = 1), 'ton\n') # Total lost biomass (ton)

  cat('Trails density :', round(x$TrailsDensity, digits = 1), 'm/ha\n') # Trails density (m/ha) (Preliminary if fuel)

  cat('Adjusted trails density :', round(x$AdjustTrailsDensity, digits = 1), 'm/ha\n') # Adjusted rails density (m/m^2) (for fuel)

  cat('\n') # skip a line

  cat('Please note that all volumes in m3/ha are per exploitable hectare, not per plot hectare.')


  # The after simulation inventory (data.frame)
  # print(x$inventory)

}

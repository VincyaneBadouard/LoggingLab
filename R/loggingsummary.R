#' Summary of the outputs of the loggingsimulation function
#'
#' @param x Outputs of the loggingsimulation function (list)
#'
#' @return A summary of the outputs of the loggingsimulation function
#' (console display):
#' - a reminder of the inputs
#' - the iterations statistics (mean, standard deviation, quantiles) of the
#'    outgoing figures
#' Please note that all volumes in m3/ha and biomass in ton/ha are per
#' exploitable hectare, not per plot hectare.
#'
#' @export
#'
#' @importFrom units set_units
#' @importFrom stats median quantile sd
#'
#' @examples
#' data(LoggingSimulationOutputs_iter) # Outputs of one logging simulation (2 iterations and 2 cores)
#'
#' loggingsummary(x = LoggingSimulationOutputs_iter)
#'
loggingsummary <- function(x
){

  # Global variables
  m3 <- ha <- ton <- NULL

  # Arguments check
  if(!inherits(x, "list"))
    stop("The argument of the 'loggingsummary' function must be a list")

  # Reminder of INPUTS:
  cat('inventory :', x[[1]]$INPUTinventory, '\n') # input inventory name
  cat('scenario :', x[[1]]$scenario, '\n') # scenario,
  cat('objective :', x[[1]]$objective, 'm3/harvestable ha\n') # objective volume (m3/ha)
  cat('fuel :', x[[1]]$fuel, '\n') # fuel
  cat('diversification :', x[[1]]$diversification, '\n') # diversification
  cat('winching :', x[[1]]$winching, '\n') # winching type
  cat('directionalfelling :', x[[1]]$directionalfelling, '\n') # directionalfelling type
  cat('specieslax :', x[[1]]$specieslax, '\n') # specieslax
  cat('objectivelax :', x[[1]]$objectivelax, '\n') # objectivelax

  # Numeric values:

  var <- c("HarvestableArea", "VO", "HVinit", "TimberLoggedVolume", "NoHollowTimberLoggedVolume", "FuelWoodBiomass",
           "LoggingResidualBiomass", "LostBiomass", "TrailsDensity", "AdjustTrailsDensity")

  statsvars <- function(v, x){ # x the list, v the var

    itervalues <- vector("numeric") # empty vector
    for(i in 1:length(x)){
      itervalues <- c(itervalues, as.numeric(x[[i]][v]))
    }

    Mean <- round(mean(itervalues), digits = 1)
    Sd <- round(sd(itervalues), digits = 1)
    Q1 <- round(quantile(itervalues, probs = 0.25, digits = 1))
    Median <- round(median(itervalues), digits = 1)
    Q3 <- round(quantile(itervalues, probs = 0.75, digits = 1))
    Min <- round(min(itervalues), digits = 1)
    Max <- round(max(itervalues), digits = 1)

    output <- list(Mean = Mean, Sd = Sd, Min = Min, Q1 = Q1, Median = Median, Q3 = Q3, Max = Max)

    return(output)
  }

  RsltStats <- lapply(var, statsvars, x)

  cat('Harvestable area (ha):\n')
  HarvestableArea <- unlist(RsltStats[1])
  print(HarvestableArea) # harvestable area (ha)


  cat('\n') # skip a line

  cat('Objective volume (m3):\n')
  print(unlist(RsltStats[2])) # your objective volume (m3)

  cat('\n') # skip a line

  cat('Initial harvestable volume (m3):\n')
  print(unlist(RsltStats[3]))
  print(set_units(unlist(RsltStats[3])/HarvestableArea, m3/ha)) # the harvestable volume (m3) with your initial criteria

  cat('\n') # skip a line

  cat('Timber logged volume (m3):\n')
  print(unlist(RsltStats[4]))
  print(set_units(unlist(RsltStats[4])/HarvestableArea, m3/ha)) # Logged volume (m3) (only healthy trees if fuel != "2", healthy + hollow trees if fuel = "2")

  cat('\n') # skip a line

  cat('No hollow timber logged volume (m3):\n')
  print(unlist(RsltStats[5]))
  print(set_units(unlist(RsltStats[5])/HarvestableArea, m3/ha)) # Logged volume (m3) (only healthy trees)

  cat('\n') # skip a line

  cat('Fuel wood biomass (ton):\n')
  print(unlist(RsltStats[6]))
  print(set_units(unlist(RsltStats[6])/HarvestableArea, ton/ha))

  cat('\n') # skip a line

  cat('Logging residual biomass (ton):\n')
  print(unlist(RsltStats[7]))
  print(set_units(unlist(RsltStats[7])/HarvestableArea, ton/ha))

  cat('\n') # skip a line

  cat('Lost biomass (ton):\n')
  print(unlist(RsltStats[8])) # Lost biomass (ton)

  cat('\n') # skip a line

  cat('Trails density (m/ha):\n')
  print(unlist(RsltStats[9])) # Trails density (m/ha)

  cat('\n') # skip a line

  cat('Adjusted trails density (m/ha):\n')
  print(unlist(RsltStats[10])) # Adjusted trails density (m/ha)

  cat('\n') # skip a line

  cat('Please note that all volumes in m3/ha and biomass in ton/ha are per exploitable hectare, not per plot hectare.')

  # The after simulation inventory (data.frame)
  # print(x$inventory)

}

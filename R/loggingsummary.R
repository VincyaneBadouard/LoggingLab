#' Summary of the outputs of the loggingsimulation function
#'
#' @param x Outputs of the loggingsimulation function (list)
#'
#' @return A summary of the outputs of the loggingsimulation function
#' (console display):
#' - a reminder of the inputs
#' - the iterations statistics (mean, standard deviation, quantiles) of the
#'    outgoing figures
#'
#' @export
#'
#' @importFrom units set_units
#' @importFrom stats median quantile sd
#'
#' @examples
#' data(LoggingSimulationOutputs_iter) # Outputs of one logging simulation (2 iterations and 2 cores)
#'
#' loggingsummary(LoggingSimulationOutputs_iter)
#'
loggingsummary <- function(x
){

  # Global variables
  m3 <- ha <- NULL

  # Arguments check
  if(!inherits(x, "list"))
    stop("The argument of the 'loggingsummary' function must be a list")

  PlotArea <- unique(x[[1]]$inventory$PlotArea)

  # Reminder of INPUTS:
  cat('inventory :', x[[1]]$INPUTinventory, '\n') # input inventory name
  cat('Plot area :', PlotArea, 'ha\n') # Plot area,
  cat('scenario :', x[[1]]$scenario, '\n') # scenario,
  cat('objective :', x[[1]]$objective, 'm3/ha\n') # objective volume (m3/ha)
  cat('fuel :', x[[1]]$fuel, '\n') # fuel
  cat('diversification :', x[[1]]$diversification, '\n') # diversification
  cat('winching :', x[[1]]$winching, '\n') # winching type
  cat('directionalfelling :', x[[1]]$directionalfelling, '\n') # directionalfelling type
  cat('specieslax :', x[[1]]$specieslax, '\n') # specieslax
  cat('objectivelax :', x[[1]]$objectivelax, '\n') # objectivelax

  # Numeric values:

  var <- c("HarvestableArea", "VO", "HVinit", "TimberLoggedVolume", "NoHollowTimberLoggedVolume", "FuelVolume",
           "DamageVolume", "LostBiomass", "TrailsDensity", "AdjustTrailsDensity")

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
  print(unlist(RsltStats[1])) # harvestable area (ha)

  cat('\n') # skip a line

  cat('Objective volume (m3):\n')
  print(unlist(RsltStats[2])) # your objective volume (m3)

  cat('\n') # skip a line

  cat('Initial harvestable volume (m3):\n')
  print(unlist(RsltStats[3]))
  print(set_units(unlist(RsltStats[3])/PlotArea, m3/ha)) # the harvestable volume (m3) with your initial criteria

  cat('\n') # skip a line

  cat('Timber logged volume (m3):\n')
  print(unlist(RsltStats[4]))
  print(set_units(unlist(RsltStats[4])/PlotArea, m3/ha)) # Logged volume (m3) (only healthy trees if fuel != "2", healthy + hollow trees if fuel = "2")

  cat('\n') # skip a line

  cat('No hollow timber logged volume (m3):\n')
  print(unlist(RsltStats[5]))
  print(set_units(unlist(RsltStats[5])/PlotArea, m3/ha)) # Logged volume (m3) (only healthy trees)

  cat('\n') # skip a line

  cat('Fuel wood volume (m3):\n')
  print(unlist(RsltStats[6]))
  print(set_units(unlist(RsltStats[6])/PlotArea, m3/ha)) # Damages + purge (+ hollow trees if fuel = "2") (m3)

  cat('\n') # skip a line

  cat('Damages volume (m3):\n')
  print(unlist(RsltStats[7]))
  print(set_units(unlist(RsltStats[7])/PlotArea, m3/ha)) # only damages (without purge and hollow trees) (m3)

  cat('\n') # skip a line

  cat('Lost biomass (ton):\n')
  print(unlist(RsltStats[8])) # Lost biomass (ton)

  cat('\n') # skip a line

  cat('Trails density (m/m2):\n')
  print(unlist(RsltStats[9])) # Trails density (m/m2)

  cat('\n') # skip a line

  cat('Adjusted trails density (m/m2):\n')
  print(unlist(RsltStats[10])) # Adjusted trails density (m/m2)


  # The after simulation inventory (data.frame)
  # print(x$inventory)

}

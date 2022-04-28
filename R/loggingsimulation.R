#' Logging simulation
#'
#'@description This function allows to simulate a timber and wood energy logging
#'  on a forest plot. It covers: harvestable area definition, tree selection,
#'  main (only for ONF plots) and secondary trails layout, tree felling,
#'  landings implementation (only for ONF plots), timber harvested, fuel wood
#'  volume and damages quantification.
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  vignette) (data.frame)
#'
#'@param plotmask Inventoried plot mask
#'(SpatialPolygonsDataFrame **with a crs in UTM**)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (Default: \code{\link{DTMParacou}})
#'  (RasterLayer **with a crs in UTM**)
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#'@param creekdistances Relative distances (vertical and horizontal) (1 m
#'  resolution) from nearest channel network (list of 2 large RasterLayers
#'  **with a crs in UTM**) (Default: \code{\link{CreekDistances}})
#'  To generate vertical creek height: \code{\link{CreekDistances}} in
#'  'Articles'.
#'
#'@param speciescriteria Table of species exploitability criteria : species
#'  names, economic interest level, minimum and maximum felling diameter, in the
#'  same format of \code{\link{SpeciesCriteria}} (2 levels of commercial
#'  species) (data.frame)
#'
#'@param volumeparameters Volume parameters table (in the same format of
#'  \code{\link{ForestZoneVolumeParametersTable}}) to compute the harvestable
#'  volume of each tree, depend to its geographic zone if several locations
#'  (data.frame)
#'
#'@param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
#'
#'@param objective Objective volume (m3/ha) (numeric)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'   If fuel wood exploitation (fuel = "1" or "2") the tree will be recovered
#'   from the crown with a grapple if possible (respected grapple conditions).
#'   If not, recovery at the foot with a cable at an angle to the trail.
#'   Avoid future/reserve trees if chosen.
#'
#'@param diversification Possibility to log other species in addition to the
#' main commercial species (species with a value of 2 for commercial in the
#' \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40m)
#' "2": grapple (default = 6m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees if possible
#' "2": to avoid damage to future and reserve trees if possible
#'       + orientation angle to the trail. Among the 2 possible angle positions,
#'       the position that favours the return to the main trail should be chosen.
#'       The angle to the trail is favoured to avoid future/reserve trees.
#' If the avoidance of future/reserve trees could not be performed,
#' a message is returned.
#'
#'@param specieslax Allow diversification if stand is too poor to reach the
#' objective volume without diversification, = FALSE by
#'  default (logical)
#'
#'@param objectivelax Allow exploitation in case of non-achievement of the
#'  objective volume (if stand too poor), = FALSE by default (logical)
#'
#'@param crowndiameterparameters Crown diameter allometry parameters table (in
#'  the same format of \code{\link{ParamCrownDiameterAllometry}}) to compute the
#'  crown diameter of each tree, depend to its DBH (Diameter at Breast Height)
#'  and its species, genus or family. (data.frame)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@param iter Number of iterations (numeric). Default = 1.
#'@param cores Number of cores for parallelization (numeric). Default = 1.
#'
#'@return Input inventory (data.frame) with logging informations (list)
#'(see the outputs metadata in the vignette).
#'
#'@seealso \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{ForestZoneVolumeParametersTable}},
#'  \code{\link{ParamCrownDiameterAllometry}}, \code{\link{loggingparameters}},
#'
#'  \code{\link{addtreedim}}, \code{\link{treeselection}},
#'  \code{\link{harvestable}}, \code{\link{selected}},
#'  \code{\link{futurereserve}}, \code{\link{treefelling}},
#'  \code{\link{createcanopy}}, \code{\link{treefromthesky}},
#'  \code{\link{felling1tree}}, \code{\link{rotatepolygon}},
#'  \code{\link{getgeometry}}, \code{\link{timberharvestedvolume}},
#'  \code{\link{exploitablefuelwoodvolume}}
#'
#'@export
#'
#'@importFrom parallel makePSOCKcluster clusterExport parLapply stopCluster
#'@importFrom doSNOW registerDoSNOW
#'@importFrom foreach foreach %dopar%
#'@importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples
#' \dontrun{
#' data(Paracou6_2016) # inventory
#' data(DTMParacou) # topography
#' data(CreekDistances) # relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' Rslt <- loggingsimulation(
#'  Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
#'  creekdistances  = CreekDistances, speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 10, fuel = "2", diversification = TRUE, winching = "2",
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'  crowndiameterparameters = ParamCrownDiameterAllometry,
#'  advancedloggingparameters = loggingparameters(), iter = 2, cores = 2)
#'  }
#'
loggingsimulation <- function(
  inventory,
  plotmask,
  topography,
  creekdistances,
  speciescriteria,
  volumeparameters,

  scenario,
  objective = NULL,
  fuel = NULL,
  diversification = NULL,

  winching = NULL,
  directionalfelling = NULL,

  specieslax = FALSE,
  objectivelax = FALSE,

  crowndiameterparameters = ParamCrownDiameterAllometry,
  advancedloggingparameters = loggingparameters(),

  iter = 1,
  cores = 1

){


  # Check args
  if(!all(unlist(lapply(list(iter, cores), inherits, "numeric"))))
    stop("The 'iter' and 'cores' arguments of the 'loggingsimulation' function must be numeric")

  # Global variables
  ParamCrownDiameterAllometry <- NULL


  # apply versions
  # replicate(iter, loggingsimulation1(inventory = inventory,
  #                                    plotmask = plotmask,
  #                                    topography = topography,
  #                                    creekdistances = creekdistances,
  #                                    speciescriteria = speciescriteria,
  #                                    volumeparameters = volumeparameters,
  #                                    scenario = scenario,
  #                                    objective = objective,
  #                                    fuel = fuel,
  #                                    diversification = diversification,
  #                                    winching = winching,
  #                                    directionalfelling = directionalfelling,
  #                                    specieslax = specieslax,
  #                                    objectivelax = objectivelax,
  #                                    crowndiameterparameters = crowndiameterparameters,
  #                                    advancedloggingparameters = advancedloggingparameters),
  #           simplify = F)
  #
  # lapply(seq_len(iter), function(x) loggingsimulation1(inventory = inventory,
  #                                            plotmask = plotmask,
  #                                            topography = topography,
  #                                            creekdistances = creekdistances,
  #                                            speciescriteria = speciescriteria,
  #                                            volumeparameters = volumeparameters,
  #                                            scenario = scenario,
  #                                            objective = objective,
  #                                            fuel = fuel,
  #                                            diversification = diversification,
  #                                            winching = winching,
  #                                            directionalfelling = directionalfelling,
  #                                            specieslax = specieslax,
  #                                            objectivelax = objectivelax,
  #                                            crowndiameterparameters = crowndiameterparameters,
  #                                            advancedloggingparameters = advancedloggingparameters)
  # )
  #
  # # purr versions
  # purr::rerun(iter, loggingsimulation1(inventory = inventory,
  #                                      plotmask = plotmask,
  #                                      topography = topography,
  #                                      creekdistances = creekdistances,
  #                                      speciescriteria = speciescriteria,
  #                                      volumeparameters = volumeparameters,
  #                                      scenario = scenario,
  #                                      objective = objective,
  #                                      fuel = fuel,
  #                                      diversification = diversification,
  #                                      winching = winching,
  #                                      directionalfelling = directionalfelling,
  #                                      specieslax = specieslax,
  #                                      objectivelax = objectivelax,
  #                                      crowndiameterparameters = crowndiameterparameters,
  #                                      advancedloggingparameters = advancedloggingparameters)
  # )
  #
  # purr::map(seq_len(iter), ~ loggingsimulation1(inventory = inventory,
  #                                                 plotmask = plotmask,
  #                                                 topography = topography,
  #                                                 creekdistances = creekdistances,
  #                                                 speciescriteria = speciescriteria,
  #                                                 volumeparameters = volumeparameters,
  #                                                 scenario = scenario,
  #                                                 objective = objective,
  #                                                 fuel = fuel,
  #                                                 diversification = diversification,
  #                                                 winching = winching,
  #                                                 directionalfelling = directionalfelling,
  #                                                 specieslax = specieslax,
  #                                                 objectivelax = objectivelax,
  #                                                 crowndiameterparameters = crowndiameterparameters,
  #                                                 advancedloggingparameters = advancedloggingparameters)
  # )
  #
  #
  # Parallelization version
  # For Windows
  # cl <- makePSOCKcluster(getOption("cl.cores", cores)) # create a cluster
  #
  # # clusterEvalQ(cl, set.seed(41)) # https://stackoverflow.com/questions/58631433/how-to-set-seeds-when-using-parallel-package-in-r
  #
  # clusterExport(cl, varlist = c("inventory", "plotmask","topography","creekdistances",
  #                               "speciescriteria","volumeparameters","scenario","objective",
  #                               "fuel","diversification","winching","directionalfelling",
  #                               "specieslax","objectivelax","crowndiameterparameters","advancedloggingparameters"),
  #               envir = environment()) # cluster environment
  #
  # # try() to catch the error and continue
  # output <- parLapply(cl, seq_len(iter), function(x) try(loggingsimulation1(inventory = inventory,
  #                                                                           plotmask = plotmask,
  #                                                                           topography = topography,
  #                                                                           creekdistances = creekdistances,
  #                                                                           speciescriteria = speciescriteria,
  #                                                                           volumeparameters = volumeparameters,
  #                                                                           scenario = scenario,
  #                                                                           objective = objective,
  #                                                                           fuel = fuel,
  #                                                                           diversification = diversification,
  #                                                                           winching = winching,
  #                                                                           directionalfelling = directionalfelling,
  #                                                                           specieslax = specieslax,
  #                                                                           objectivelax = objectivelax,
  #                                                                           crowndiameterparameters = crowndiameterparameters,
  #                                                                           advancedloggingparameters = advancedloggingparameters)))
  #
  # stopCluster(cl) # stop the cluster

  # For other OS
  # parallel::mclapply(seq_len(iter), function(x) loggingsimulation1(inventory = inventory,
  #                                                        plotmask = plotmask,
  #                                                        topography = topography,
  #                                                        creekdistances = creekdistances,
  #                                                        speciescriteria = speciescriteria,
  #                                                        volumeparameters = volumeparameters,
  #                                                        scenario = scenario,
  #                                                        objective = objective,
  #                                                        fuel = fuel,
  #                                                        diversification = diversification,
  #                                                        winching = winching,
  #                                                        directionalfelling = directionalfelling,
  #                                                        specieslax = specieslax,
  #                                                        objectivelax = objectivelax,
  #                                                        crowndiameterparameters = crowndiameterparameters,
  #                                                        advancedloggingparameters = advancedloggingparameters),
  #                    mc.cores = cores)

  # foreach version
  i <- NULL
  cl <- parallel::makeCluster(cores)
  doSNOW::registerDoSNOW(cl)
  pb <- txtProgressBar(max = iter, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  output <- foreach::foreach(i=1:iter,
                                .options.snow = opts) %dopar% {
                                  sim <- iter[i]
                                  try(loggingsimulation1(inventory = inventory,
                                                         plotmask = plotmask,
                                                         topography = topography,
                                                         creekdistances = creekdistances,
                                                         speciescriteria = speciescriteria,
                                                         volumeparameters = volumeparameters,
                                                         scenario = scenario,
                                                         objective = objective,
                                                         fuel = fuel,
                                                         diversification = diversification,
                                                         winching = winching,
                                                         directionalfelling = directionalfelling,
                                                         specieslax = specieslax,
                                                         objectivelax = objectivelax,
                                                         crowndiameterparameters = crowndiameterparameters,
                                                         advancedloggingparameters = advancedloggingparameters))
                                }
  close(pb)
  stopCluster(cl)
  names(output) <- iter

  return(output)

}


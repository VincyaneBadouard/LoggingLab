#' Logging simulation
#'
#'@description This function allows to simulate a timber and wood energy logging
#'  on a forest plot. It covers: harvestable area definition, tree selection,
#'  main (only for ONF plots) and secondary trails layout, tree felling,
#'  landings implementation (only for ONF plots), timber harvested, fuel wood
#'  volume and damages quantification.
#'
#'@param inventory Input inventory (see the inputs formats and metadata in the
#'  \code{\link{vignette}}) (data.frame)
#'
#'@param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (Default: \code{\link{DTMParacou}}) (RasterLayer)
#'
#'@param verticalcreekheight Vertical creek height (in m) of the inventoried
#' plot (1 m resolution) (Default: \code{\link{VerticalCreekHeight}})
#' (Large RasterLayer). To generate vertical creek height:
#'  \code{\link{VerticalCreekHeight}} in 'docs' folder of the package
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
#'  \code{\link{vignette}})
#'
#'@param objective Objective volume (m3/ha) (numeric)
#'
#'@param fuel Fuel wood exploitation: no exploitation = "0", exploitation of
#'   damage and unused part of logged trees for fuel = "1", exploitation of
#'   hollow trees, damage and and unused part of the log for fuel = "2"
#'
#'@param diversification Possibility to log other species in addition to the
#' main commercial species (species with a value of 2 for commercial in the
#' \code{\link{SpeciesCriteria}} table) (logical)
#'
#'@param winching No cable or grapple = "0", only cable = "1", grapple + cable =
#'  "2"
#'
#'@param directionalfelling Directional felling =
#' "0": only to direct the foot of the tree towards the trail
#' "1": to direct the foot of the tree towards the trail + to avoid damage to
#'         future and reserve trees
#' "2": to avoid damage to future and reserve trees + orientation angle
#'       to the trail
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
#'@return Input inventory (data.frame) with logging informations
#'(see the outputs metadata in the \code{\link{vignette}}).
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
#'@importFrom dplyr filter mutate select left_join bind_rows
#'@importFrom sf st_as_sf st_point
#'@importFrom raster crs extract
#'@importFrom methods as
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
#' Rslt <- loggingsimulation(
#'  Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
#'  verticalcreekheight  = VerticalCreekHeight, speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 20, fuel = "2", diversification = TRUE, winching = "2",
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'  crowndiameterparameters = ParamCrownDiameterAllometry,
#'  advancedloggingparameters = loggingparameters(), iter = 2, cores = 1)
#'  }
#'
loggingsimulation <- function(
  inventory,
  plotmask,
  topography,
  verticalcreekheight,
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


  # apply versions
  # replicate(iter, loggingsimulation1(inventory = inventory,
  #                                    plotmask = plotmask,
  #                                    topography = topography,
  #                                    verticalcreekheight = verticalcreekheight,
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
  # lapply(seq_along(iter), loggingsimulation1(inventory = inventory,
  #                                            plotmask = plotmask,
  #                                            topography = topography,
  #                                            verticalcreekheight = verticalcreekheight,
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
  #                                      verticalcreekheight = verticalcreekheight,
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
  # purr::map(seq_along(iter), ~ loggingsimulation1(inventory = inventory,
  #                                                 plotmask = plotmask,
  #                                                 topography = topography,
  #                                                 verticalcreekheight = verticalcreekheight,
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
  # # Parallelization version
  # parallel::mclapply(seq_along(iter), loggingsimulation1(inventory = inventory,
  #                                                        plotmask = plotmask,
  #                                                        topography = topography,
  #                                                        verticalcreekheight = verticalcreekheight,
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


}


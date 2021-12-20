#'loggingsimulation
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
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#'@param relativeelevation (RasterLayer)
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
#'         future and reserve trees if possible
#' "2": to avoid damage to future and reserve trees if possible
#'       + orientation angle to the trail
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
#' # data() relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' Rslt <- loggingsimulation(Paracou6_2016, topography = DTMParacou,
#'  relativeelevation  = DTMParacou, speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 20, fuel = "2", diversification = TRUE, winching = "2",
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'  crowndiameterparameters = ParamCrownDiameterAllometry,
#'  advancedloggingparameters = loggingparameters(), iter = 1, cores = 1)
#'  }
#'
loggingsimulation <- function(
  inventory,

  topography, # = NULL perspective
  relativeelevation, # = NULL perspective

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

  # Arguments check

  # inventory, speciescriteria, volumeparameters, crowndiameterparameters
  if(!all(unlist(lapply(list(inventory, speciescriteria, volumeparameters, crowndiameterparameters), inherits, "data.frame"))))
    stop("The 'inventory', 'speciescriteria', 'volumeparameters' and 'crowndiameterparameters' arguments
         of the 'loggingsimulation' function must be data.frames")

  # topography, relativeelevation
  if(!all(unlist(lapply(list(topography, relativeelevation), inherits, "RasterLayer"))))
    stop("The 'topography' and 'relativeelevation' arguments of the 'loggingsimulation' function must be RasterLayers")

  # scenario
  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"|| scenario == "RIL3"||
           scenario == "RIL3fuel"|| scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'loggingsimulation' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # objective
  if(!any(inherits(objective, "numeric") || is.null(objective)))
    stop("The 'objective' argument of the 'loggingsimulation' function must be numeric or NULL")

  # fuel
  if (!any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # diversification
  if(!any(inherits(diversification, "logical") || is.null(diversification)))
    stop("The 'diversification' argument of the 'loggingsimulation' function must be logical or NULL")

  # winching
  if (!any(winching == "0" || winching == "1"|| winching == "2"|| is.null(winching)))
    stop("The 'winching' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # directionalfelling
  if (!any(directionalfelling == "0" || directionalfelling == "1" || directionalfelling == "2" || is.null(directionalfelling)))
    stop("The 'directionalfelling' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # manual mode
  if(scenario == "manual" &&
     (is.null(objective) || is.null(fuel) || is.null(diversification) || is.null(winching) || is.null(directionalfelling)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'objective', 'fuel', 'diversification', 'winching' and 'directionalfelling'")

  # specieslax, objectivelax
  if(!all(unlist(lapply(list(specieslax, objectivelax), inherits, "logical"))))
    stop("The 'specieslax' and 'objectivelax' arguments of the 'loggingsimulation' function must be logicals") # any() don't take a list


  # advancedloggingparameters
  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'loggingsimulation' function must be a list")

  # iter,cores
  if(!all(unlist(lapply(list(iter, cores), inherits, "numeric"))))
    stop("The 'iter' and 'cores' arguments of the 'loggingsimulation' function must be numeric")



  # Global variables
  ParamCrownDiameterAllometry <- NULL

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(scenario = scenario, objective = objective, fuel = fuel,
                                             diversification = diversification, winching = winching,
                                             directionalfelling = directionalfelling)

  objective <- scenariosparameters$objective
  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification
  winching <- scenariosparameters$winching
  directionalfelling <- scenariosparameters$directionalfelling

  INPUTinventory <- deparse(substitute(inventory)) # object name to this name in character

  # Function


  # Check & format the inventory + add the tree dimensions:
  inventory <- addtreedim(inventorycheckformat(inventory),
                          volumeparameters = volumeparameters,
                          crowndiameterparameters = crowndiameterparameters,
                          advancedloggingparameters = advancedloggingparameters)

  # # Harvestable area definition: A FAIRE
  #
  #
  # # Main trails layout: (only for ONF plots)
  #
  #
  # Tree selection (harvestable, future and reserve trees + defects trees):
  plotslope <- PlotSlope # A SUPPRIMER
  treeselectionoutputs <- treeselection(inventory, topography = topography, plotslope = plotslope,
                                        scenario = scenario, objective = objective, fuel = fuel,
                                        diversification = diversification, specieslax = specieslax,
                                        objectivelax = objectivelax, speciescriteria = speciescriteria,
                                        advancedloggingparameters = advancedloggingparameters)

  inventory <- treeselectionoutputs$inventory
  VO <- treeselectionoutputs$VO
  HVinit <- treeselectionoutputs$HVinit
  HarvestableTreesPoints <- treeselectionoutputs$HarvestableTreesPoints
  SelectedTreesPoints <- treeselectionoutputs$SelectedTreesPoints
  FutureTreesPoints <- treeselectionoutputs$FutureTreesPoints
  ReserveTreesPoints <- treeselectionoutputs$ReserveTreesPoints
  HollowTreesPoints <- treeselectionoutputs$HollowTreesPoints
  EnergywoodTreesPoints <- treeselectionoutputs$EnergywoodTreesPoints

  # # Secondary trails layout (preliminaries for fuel wood harvesting): A FAIRE
  #
#
  MainTrail <- sf::st_linestring(matrix(c(286400, 583130, # A SUPPRIMER
                                          286400, 583250,
                                          286655, 583250,
                                          286655, 583130,
                                          286400, 583130) # the return
                                        ,ncol=2, byrow=TRUE))

  pol1 <- list(matrix(c(286503, 583134,
                        286503, 583240,
                        286507, 583240,
                        286507, 583134,
                        286503, 583134) # the return
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 583134,
                        286650, 583240,
                        286654, 583240,
                        286654, 583134,
                        286650, 583134) # the return
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2) #list of lists of numeric matrices
  ScndTrail <- sf::st_multipolygon(PolList) # A SUPPRIMER

  # Tree felling:
  inventory <- treefelling(inventory, scenario = scenario, fuel = fuel,
                           directionalfelling = directionalfelling,
                           MainTrail = MainTrail, ScndTrail = ScndTrail,
                           advancedloggingparameters = advancedloggingparameters)


  # Adjusted secondary trails layout (for fuel wood harvesting only) A FAIRE

  # Landings implementation: (only for ONF plots)

  # Timber harvested volume quantification
  Timberoutputs <- timberharvestedvolume(inventory,
                                         scenario = scenario, fuel = fuel,
                                         advancedloggingparameters = advancedloggingparameters)

  TimberLoggedVolume <- Timberoutputs$TimberLoggedVolume
  NoHollowTimberLoggedVolume <- Timberoutputs$NoHollowTimberLoggedVolume


  # Exploitable fuel wood volume quantification
  Fueloutputs <- exploitablefuelwoodvolume(inventory,
                                           scenario = scenario, fuel = fuel,
                                           TimberLoggedVolume = TimberLoggedVolume,
                                           NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
                                           advancedloggingparameters = advancedloggingparameters)

  DamageVolume <- Fueloutputs$DamageVolume # only damage (without purge and hollow trees)
  FuelVolume <- Fueloutputs$FuelVolume




  Outputs <- list(inventory = inventory,

                  # Numeric values
                  # HarvestableArea = HarvestableArea,
                  VO = VO,
                  HVinit = HVinit,
                  TimberLoggedVolume = TimberLoggedVolume,
                  NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
                  FuelVolume = FuelVolume,
                  DamageVolume = DamageVolume, # only damage (without purge and hollow trees)
                  # LostBiomass = LostBiomass,
                  # TrailsDensity = TrailsDensity,

                  # POINTS
                  HarvestableTreesPoints = HarvestableTreesPoints,
                  SelectedTreesPoints = SelectedTreesPoints,
                  FutureTreesPoints = FutureTreesPoints,
                  ReserveTreesPoints = ReserveTreesPoints,
                  HollowTreesPoints = HollowTreesPoints,
                  EnergywoodTreesPoints = EnergywoodTreesPoints,

                  # INPUTS reminder
                  INPUTinventory = INPUTinventory,
                  scenario = scenario,
                  objective = objective,
                  fuel = fuel,
                  diversification = diversification,
                  winching = winching,
                  directionalfelling = directionalfelling,
                  specieslax = specieslax,
                  objectivelax = objectivelax
  )

  return(Outputs)


}

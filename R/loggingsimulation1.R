#' Logging simulation without iteration or parallelization
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
#'@param creekdistances Vertical creek height (in m) of the inventoried
#' plot (1 m resolution) (Default: \code{\link{CreekDistances}})
#' (Large RasterLayer). To generate vertical creek height:
#'  \code{\link{CreekDistances}} in 'docs' folder of the package
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
#'@return Input inventory (data.frame) with logging informations (list)
#'(see the outputs metadata in the \code{\link{vignette}}).
#'
#'@seealso \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'  \code{\link{ForestZoneVolumeParametersTable}},
#'  \code{\link{ParamCrownDiameterAllometry}}, \code{\link{loggingparameters}},
#'
#'  \code{\link{inventorycheckformat}}, \code{\link{cleaninventory}}
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
#' data(PlotMask) # inventoried plot mask
#' data(DTMParacou) # topography
#' data(CreekDistances) # relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' Rslt <- loggingsimulation1(
#'  Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
#'  creekdistances  = CreekDistances, speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 20, fuel = "2", diversification = TRUE, winching = "2",
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'  crowndiameterparameters = ParamCrownDiameterAllometry,
#'  advancedloggingparameters = loggingparameters())
#'  }
#'
loggingsimulation1 <- function(
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
  advancedloggingparameters = loggingparameters()
){

  #### Arguments check ####

  # inventory, speciescriteria, volumeparameters, crowndiameterparameters
  if(!all(unlist(lapply(list(inventory, speciescriteria, volumeparameters, crowndiameterparameters),
                        inherits, "data.frame"))))
    stop("The 'inventory', 'speciescriteria', 'volumeparameters' and 'crowndiameterparameters' arguments
         of the 'loggingsimulation' function must be data.frames")

  # plotmask
  if(!inherits(plotmask, "SpatialPolygonsDataFrame"))
    stop("The 'plotmask' argument of the 'loggingsimulation' function must be a SpatialPolygonsDataFrame")

  # topography, creekdistances
  if(!all(unlist(lapply(list(topography, creekdistances), inherits, "RasterLayer"))))
    stop("The 'topography' and 'creekdistances' arguments of the 'loggingsimulation' function must be RasterLayers")

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


  #### Global variables ####
  DeathCause <- AGB <- ParamCrownDiameterAllometry <- NULL

  #### Redefinition of the parameters according to the chosen scenario ####
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


  #### Check and clean the inventory + add the tree dimensions ####
  inventory <- inventorycheckformat(inventory)

  inventory <- cleaninventory(inventory, plotmask = plotmask, advancedloggingparameters = advancedloggingparameters)

  inventory <- addtreedim(inventory,
                          volumeparameters = volumeparameters,
                          crowndiameterparameters = crowndiameterparameters,
                          advancedloggingparameters = advancedloggingparameters)


  #### Main trails layout ####

  data(MainTrails) # A SUPPRIMER

  ##### Harvestable area definition: ####
  HarvestableAreaOutputs <- harvestableareadefinition(topography = topography,
                                                      creekdistances = creekdistances,
                                                      advancedloggingparameters = advancedloggingparameters)

  HarvestablePolygons <- HarvestableAreaOutputs$HarvestablePolygons
  PlotSlope <- HarvestableAreaOutputs$PlotSlope
  HarvestableArea <- HarvestableAreaOutputs$HarvestableArea

  #### Tree selection (harvestable, future and reserve trees + defects trees): ####
  treeselectionoutputs <- treeselection(inventory, topography = topography, plotslope = PlotSlope,
                                        scenario = scenario, objective = objective, fuel = fuel,
                                        diversification = diversification, specieslax = specieslax,
                                        harvestablepolygons = HarvestablePolygons, MainTrails = MainTrails,
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

  #### Secondary trails layout (preliminaries for fuel wood harvesting) ####
  pol1 <- list(matrix(c(286503, 582925,
                        286503, 583240,
                        286507, 583240,
                        286507, 582925,
                        286503, 582925) # the return
                      ,ncol=2, byrow=TRUE))
  pol2 <- list(matrix(c(286650, 582925,
                        286650, 583240,
                        286654, 583240,
                        286654, 582925,
                        286650, 582925) # the return
                      ,ncol=2, byrow=TRUE))

  PolList = list(pol1,pol2) #list of lists of numeric matrices
  ScndTrail <- sf::st_as_sf(sf::st_sfc(sf::st_multipolygon(PolList)))
  ScndTrail <- sf::st_set_crs(ScndTrail, sf::st_crs(MainTrails)) # A SUPPRIMER

  # ScndTrailOutputs <- secondtrailsopening(
  #   topography = topography,
  #   plotmask = PlotMask,
  #   treeselectionoutputs = treeselectionoutputs,
  #   creekdistances = creekdistances,
  #   CostMatrix = CostMatrix,
  #   scenario = scenario, winching = winching,
  #   fact = 3,
  #   advancedloggingparameters = advancedloggingparameters)

  # ScndTrail <- ScndTrailOutputs$ScndTrail
  # TrailsDensity <- ScndTrailOutputs$TrailsDensity # A AJOUTER A LA FCT


  #### Tree felling ####
  inventory <- treefelling(inventory, scenario = scenario, fuel = fuel,
                           winching = winching, directionalfelling = directionalfelling,
                           MainTrails = MainTrails, ScndTrail = ScndTrail,
                           advancedloggingparameters = advancedloggingparameters)


  #### Adjusted secondary trails layout (for fuel wood harvesting only) A FAIRE ####

  #### Landings implementation (only for ONF plots) ####

  #### Timber harvested volume quantification ####
  Timberoutputs <- timberharvestedvolume(inventory,
                                         scenario = scenario, fuel = fuel,
                                         advancedloggingparameters = advancedloggingparameters)

  TimberLoggedVolume <- Timberoutputs$TimberLoggedVolume
  NoHollowTimberLoggedVolume <- Timberoutputs$NoHollowTimberLoggedVolume


  #### Exploitable fuel wood volume quantification ####
  Fueloutputs <- exploitablefuelwoodvolume(inventory,
                                           scenario = scenario, fuel = fuel,
                                           TimberLoggedVolume = TimberLoggedVolume,
                                           NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
                                           advancedloggingparameters = advancedloggingparameters)

  DamageVolume <- Fueloutputs$DamageVolume # only damage (without purge and hollow trees)
  FuelVolume <- Fueloutputs$FuelVolume

  DeadTrees <- inventory %>%
    filter(!is.na(DeathCause))

  LostBiomass <- sum(DeadTrees$AGB) # in ton


  #### Outputs ####
  Outputs <- list(inventory = inventory,

                  # Numeric values
                  HarvestableArea = HarvestableArea,
                  VO = VO,
                  HVinit = HVinit,
                  TimberLoggedVolume = TimberLoggedVolume,
                  NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
                  FuelVolume = FuelVolume,
                  DamageVolume = DamageVolume, # only damage (without purge and hollow trees)
                  LostBiomass = LostBiomass,
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

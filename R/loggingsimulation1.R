#' Logging simulation without iteration or parallelization
#'
#'@description This function allows to simulate a timber and fuel wood logging
#'  on a forest plot. It covers: harvestable zones definition, tree selection,
#'  secondary skidding trails layout, tree felling, timber harvested, fuel wood
#'  volume and short-term damages quantification. This simulator is
#'  individual-centred, spatialised, and takes into account the topography and
#'  the hydrographic network.
#'
#'@param inventory Input forest inventory for 1 plot and 1 census year (see the
#'  inputs formats and metadata in the vignette or in
#'  \code{\link{Paracou6_2016}}) (data.frame)
#' The columns required are:
#' - *Forest* (to apply the corresponding volume formula)
#' - *idTree*
#' - *Xutm* and *Yutm*
#' - *CodeAlive*
#' - *Family*, *Genus*, and *Species*
#' - *CircCorr*
#' The optional columns are
#' - *Plot* (1 value)
#' - *CensusYear* (1 value)
#'
#'@param plotmask Inventoried plot mask (SpatialPolygonsDataFrame
#'  **with a crs in UTM**)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR,
#'  1m resolution)
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{DTMParacou}})
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#'@param creekverticaldistance Relative vertical distance
#'  (1 m resolution) from nearest channel network
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{CreekDistances}})
#'  To generate creek distances: \code{\link{CreekDistances}} in 'Articles'.
#'
#'@param creekhorizontaldistance Relative horizontal distance
#'  (1 m resolution) from nearest channel network
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{CreekDistances}})
#'  To generate creek distances: \code{\link{CreekDistances}} in 'Articles'.
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
#'@param scenario Logging scenario among: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette for details)
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
#'@param winching Tree recovery =
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
#'  and its Species, Genus or Family names (Aubry-Kientz et al.2019).
#'  (data.frame)
#'
#'@param seed The seed set for the uniform random-number generator (numeric).
#'  Default = NULL
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'@return  A large list of 40  elements: input forest inventory (data.frame) with
#'  logging informations (list) (see the outputs metadata in the vignette or
#'  \code{\link{LoggingSimulationOutputs}}).
#'
#'@section Paying attention to inputs - important source of error:
#'Common error sources:
#' - no crs
#' - crs with accent
#' - *topography* and *plotmask* do not match (plot them to check)
#' - *topography* import as R Worspace (you must import it as a .tif file)
#' - *Forest* name of the *inventory* doesn't match with the *Forest* name in
#'    *volumeparameters* table
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
#'  \code{\link{harvestablefuelwood}}
#'
#'@export
#'
#'@importFrom dplyr filter mutate select left_join bind_rows
#'@importFrom sf st_as_sf st_point as_Spatial
#'@importFrom raster crs extract raster
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
#' LoggingSimulationOutputs <- loggingsimulation1(
#'  Paracou6_2016, plotmask = PlotMask, topography = DTMParacou,
#'  creekverticaldistance = CreekDistances$distvert,
#'  creekhorizontaldistance = CreekDistances$disthorz,
#'  speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 10, fuel = "2", diversification = TRUE, winching = "2",
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = TRUE,
#'  crowndiameterparameters = ParamCrownDiameterAllometry,
#'  advancedloggingparameters = loggingparameters())
#'  }
#'
loggingsimulation1 <- function(
    inventory,
    plotmask,
    topography,
    creekverticaldistance,
    creekhorizontaldistance,
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
    seed = round(runif(n = 1, min = 1, max = 2^23)),
    advancedloggingparameters = loggingparameters()
){

  creekdistances <- list("distvert" = creekverticaldistance,
                         "disthorz" = creekhorizontaldistance)

  #### Arguments check ####

  # inventory, speciescriteria, volumeparameters, crowndiameterparameters
  if(!all(unlist(lapply(list(inventory, speciescriteria, volumeparameters, crowndiameterparameters),
                        inherits, "data.frame"))))
    stop("The 'inventory', 'speciescriteria', 'volumeparameters' and 'crowndiameterparameters' arguments
         of the 'loggingsimulation' function must be data.frames")
  # inventory as.data.frame to remove data.table or dplyr formats

  INPUTinventory <- deparse(substitute(inventory)) # object name to this name in character

  inventory <- as.data.frame(inventory)

  # plotmask
  if(length(plotmask) != 1 | !inherits(plotmask, "SpatialPolygons")){
    if((inherits(plotmask, "sf") & inherits(plotmask$geometry, "sfc_POLYGON")))
      plotmask <- as_Spatial(plotmask)
    if(!(inherits(plotmask, "sf") & inherits(plotmask$geometry, "sfc_POLYGON")))
      stop("The 'plotmask' argument of the 'loggingsimulation' function must be a SpatialPolygons")
  }

  # topography
  if(!inherits(topography, "RasterLayer")){
    if(inherits(topography, "SpatRaster"))
      topography <- raster(topography)
    if(!inherits(topography, "SpatRaster"))
      stop("The 'topography' argument of the 'loggingsimulation' function must be a RasterLayer")
  }

  # creekdistances
  if(!inherits(creekverticaldistance, "RasterLayer")){
    if(inherits(creekverticaldistance, "SpatRaster"))
      creekverticaldistance <- raster(creekverticaldistance)
    if(!inherits(creekverticaldistance, "SpatRaster"))
      stop("The 'creekverticaldistance' argument of the 'loggingsimulation' function must be a RasterLayer")
  }
  if(!inherits(creekhorizontaldistance, "RasterLayer")){
    if(inherits(creekhorizontaldistance, "SpatRaster"))
      creekhorizontaldistance <- raster(creekhorizontaldistance)
    if(!inherits(creekhorizontaldistance, "SpatRaster"))
      stop("The 'creekhorizontaldistance' argument of the 'loggingsimulation' function must be a RasterLayer")
  }

  # scenario
  if (length(scenario) != 1 |
      !any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"|| scenario == "RIL3"||
           scenario == "RIL3fuel"|| scenario == "RIL3fuelhollow"|| scenario =="manual"))
    stop("The 'scenario' argument of the 'loggingsimulation' function must be
         'RIL1', 'RIL2broken', 'RIL2', 'RIL3', 'RIL3fuel', 'RIL3fuelhollow' or 'manual'")

  # objective
  if(length(objective) > 1 |
     !any(inherits(objective, "numeric") || is.null(objective)))
    stop("The 'objective' argument of the 'loggingsimulation' function must be numeric or NULL")

  # fuel
  if (length(fuel) > 1 |
      !any(fuel == "0" || fuel == "1"|| fuel == "2"|| is.null(fuel)))
    stop("The 'fuel' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # diversification
  if(length(diversification) > 1 |
     !any(inherits(diversification, "logical") || is.null(diversification)))
    stop("The 'diversification' argument of the 'loggingsimulation' function must be logical or NULL")

  # winching
  if (length(winching) > 1 |
      !any(winching == "0" || winching == "1"|| winching == "2"|| is.null(winching)))
    stop("The 'winching' argument of the 'loggingsimulation' function must be '0', '1', '2' or NULL")

  # directionalfelling
  if (length(directionalfelling) > 1 |
      !any(directionalfelling == "0" || directionalfelling == "1" || directionalfelling == "2" || is.null(directionalfelling)))
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

  # Options
  options("rgdal_show_exportToProj4_warnings"="none") # to avoid gdal warnings

  #### Global variables ####
  DeathCause <- AGB <- ParamCrownDiameterAllometry <- NULL

  #### Set seed #####
  seedsim <- seed
  set.seed(seed)

  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, objective = objective, fuel = fuel,
                                             diversification = diversification, winching = winching,
                                             directionalfelling = directionalfelling)

  objective <- scenariosparameters$objective
  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification
  winching <- scenariosparameters$winching
  directionalfelling <- scenariosparameters$directionalfelling

  # Function


  #### Check and clean the inventory + add the tree dimensions ####
  inventory <- inventorycheckformat(inventory)

  inventory <- cleaninventory(inventory, plotmask = plotmask, advancedloggingparameters = advancedloggingparameters)

  inventory <- addtreedim(inventory,
                          volumeparameters = volumeparameters,
                          crowndiameterparameters = crowndiameterparameters,
                          advancedloggingparameters = advancedloggingparameters)


  #### Main trails layout ####

  MainTrails <- maintrailextract(topography = topography, advancedloggingparameters = advancedloggingparameters)

  if(is.null(MainTrails) | MainTrails$layer == 0 | is.null(MainTrails$geometry)){
    stop("The main trails could not be calculated. There is probably a problem with the inputs: 'topography'")}

  ##### Harvestable area definition ####
  HarvestableAreaOutputs <- harvestableareadefinition(
    topography = topography,
    creekverticaldistance = creekdistances$distvert,
    creekhorizontaldistance = creekdistances$disthorz,
    maintrails = MainTrails,
    plotmask = plotmask,
    scenario = scenario, winching = winching,
    advancedloggingparameters = advancedloggingparameters
  )

  HarvestablePolygons <- HarvestableAreaOutputs$HarvestablePolygons
  PlotSlope <- HarvestableAreaOutputs$PlotSlope
  HarvestableArea <- HarvestableAreaOutputs$HarvestableArea
  MachinePolygons <- HarvestableAreaOutputs$MachinePolygons

  if(is.null(HarvestableArea) | HarvestableArea == 0){
    stop("The havestable area is equal to 0 or NULL.
    Either your plot is not exploitable at all according to your criteria, or there is probably a problem with the inputs:
    'topography', 'creekverticaldistance', 'creekhorizontaldistance', and/or 'plotmask'")}

  #### Tree selection (harvestable, future and reserve trees + defects trees): ####
  treeselectionoutputs <- treeselection(inventory,
                                        topography = topography,
                                        speciescriteria = speciescriteria,
                                        scenario = scenario, objective = objective,
                                        fuel = fuel, winching = winching,
                                        diversification = diversification,
                                        specieslax = specieslax, objectivelax = objectivelax,
                                        harvestablearea = HarvestableArea,
                                        plotslope = PlotSlope,maintrails = MainTrails,
                                        harvestablepolygons = HarvestablePolygons,
                                        advancedloggingparameters = advancedloggingparameters)

  VO <- treeselectionoutputs$VO
  HVinit <- treeselectionoutputs$HVinit
  HarvestableTreesPoints <- treeselectionoutputs$HarvestableTreesPoints
  SelectedTreesPoints <- treeselectionoutputs$SelectedTreesPoints
  FutureTreesPoints <- treeselectionoutputs$FutureTreesPoints
  ReserveTreesPoints <- treeselectionoutputs$ReserveTreesPoints
  HollowTreesPoints <- treeselectionoutputs$HollowTreesPoints
  EnergywoodTreesPoints <- treeselectionoutputs$EnergywoodTreesPoints

  if(!any(treeselectionoutputs$inventory$Selected == 1)){
    print(inventory)
    stop("No trees have been selected. Check the printed inventory")
  }

  #### Secondary trails layout (preliminaries for fuel wood harvesting) ####

  accesspts <- genaccesspts(topography = topography,
                            machinepolygons = MachinePolygons,
                            maintrails = MainTrails,
                            advancedloggingparameters = advancedloggingparameters)

  ScndTrailOutputs <- secondtrailsopening(topography = topography,
                                          plotmask = plotmask,
                                          maintrails = MainTrails, plotslope = PlotSlope,
                                          harvestablepolygons = HarvestablePolygons,
                                          machinepolygons = MachinePolygons,
                                          maintrailsaccess = accesspts$AccessPointAll,
                                          treeselectionoutputs = treeselectionoutputs,
                                          scenario = scenario, winching = winching, fuel = fuel,
                                          advancedloggingparameters = advancedloggingparameters
  )


  inventory <- ScndTrailOutputs$inventory
  SmoothedTrails <- ScndTrailOutputs$SmoothedTrails
  MainTrailsAccess <- ScndTrailOutputs$MainTrailsAccess
  TrailsDensity <- ScndTrailOutputs$TrailsDensity
  TrailsIdentity <- ScndTrailOutputs$TrailsIdentity
  RawSecondTrails <- ScndTrailOutputs$RawSecondTrails
  CostRasterAgg <- ScndTrailOutputs$CostRasterAgg

  if(is.null(TrailsDensity) | as.numeric(TrailsDensity) == 0){
    print(inventory)
    stop("No trails have been traced. There is probably a problem. Check your inputs:
         'topography' and 'plotmask' and the printed inventory")
  }

  #### Tree felling ####
  inventory <- treefelling(inventory = inventory, scenario = scenario, fuel = fuel,
                           winching = winching, directionalfelling = directionalfelling,
                           maintrailsaccess = MainTrailsAccess, scndtrail = SmoothedTrails,
                           advancedloggingparameters = advancedloggingparameters)

  if(all(is.na(inventory$TreePolygon))){
    print(inventory)
    stop("The 'treefelling' function did not work: no polygon of tree on the ground was created. Check the printed inventory")
  }
  #### Adjusted secondary trails layout (for fuel wood harvesting only) ####
  if(fuel != "0"){

    ScndTrailAdjustOutputs <- secondtrailsadjusted(inventory = inventory,
                                                   topography = topography, plotmask = plotmask, maintrails = MainTrails,
                                                   plotslope = PlotSlope,
                                                   harvestablepolygons = HarvestablePolygons,
                                                   machinepolygons = MachinePolygons, maintrailsaccess = MainTrailsAccess,
                                                   scenario = scenario, winching = winching,
                                                   advancedloggingparameters = advancedloggingparameters)


    if(is.null(ScndTrailAdjustOutputs$TrailsDensity) | as.numeric(ScndTrailAdjustOutputs$TrailsDensity) == 0){
      print(inventory)
      stop("No adjusted trails have been traced. Check the printed inventory")
    }

    inventory <- ScndTrailAdjustOutputs$inventory
    AdjustSmoothedTrails <- ScndTrailAdjustOutputs$SmoothedTrails
    AdjustTrailsDensity <- ScndTrailAdjustOutputs$TrailsDensity
    AdjustTrailsIdentity <- ScndTrailAdjustOutputs$TrailsIdentity
    AdjustRawSecondTrails <- ScndTrailAdjustOutputs$RawSecondTrails


  }else{
    AdjustSmoothedTrails <- NULL
    AdjustTrailsDensity <- NULL
    AdjustTrailsIdentity <- NULL
    AdjustRawSecondTrails <- NULL
  }

  #### Landings implementation (only for ONF plots) ####

  #### Timber harvested volume quantification ####
  Timberoutputs <- timberharvestedvolume(inventory,
                                         scenario = scenario, fuel = fuel,
                                         advancedloggingparameters = advancedloggingparameters)

  inventory <- Timberoutputs$inventory
  TimberLoggedVolume <- Timberoutputs$TimberLoggedVolume # purge included
  NoHollowTimberLoggedVolume <- Timberoutputs$NoHollowTimberLoggedVolume

  if(is.null(TimberLoggedVolume) |TimberLoggedVolume == 0){
    print(inventory)
    stop("No TimberLoggedVolume. Check the printed inventory")
  }



  #### Exploitable fuel wood volume quantification ####
  Fueloutputs <- harvestablefuelwood(inventory,
                                           scenario = scenario, fuel = fuel,
                                           TimberLoggedVolume = TimberLoggedVolume,
                                           NoHollowTimberLoggedVolume = NoHollowTimberLoggedVolume,
                                           advancedloggingparameters = advancedloggingparameters)

  inventory <- Fueloutputs$inventory
  LoggingResidualBiomass <- Fueloutputs$LoggingResidualBiomass
  FuelWoodBiomass <- Fueloutputs$FuelWoodBiomass

  # BO = TimberLoggedVolume - Purge
  TotalPurgeVolume <- sum(inventory$PurgeVolume, na.rm = TRUE)
  TimberExtractedVolume <- TimberLoggedVolume - TotalPurgeVolume


  DeadTrees <- inventory %>%
    dplyr::filter(!is.na(DeathCause))

  if(nrow(DeadTrees)==0){
    print(inventory)
    stop("No DeadTrees. Check the printed inventory")
  }

  LostBiomass <- sum(DeadTrees$AGB) # in ton

  if(is.null(LostBiomass) | LostBiomass == 0){
    print(inventory)
    message("No LostBiomass. Check if the printed inventory")
  }


  #### Outputs ####
  Outputs <- list("seed" = seedsim,
                  "inventory" = inventory,

                  # Numeric values
                  "HarvestableArea" = HarvestableArea,
                  "VO" = VO,
                  "HVinit" = HVinit,
                  "TimberLoggedVolume" = TimberLoggedVolume,
                  "NoHollowTimberLoggedVolume" = NoHollowTimberLoggedVolume,
                  "TimberExtractedVolume" = TimberExtractedVolume, # Timber volume without purge
                  "FuelWoodBiomass" = FuelWoodBiomass,
                  "LoggingResidualBiomass" = LoggingResidualBiomass,
                  "LostBiomass" = LostBiomass,
                  "TrailsDensity" = TrailsDensity,
                  "AdjustTrailsDensity" = AdjustTrailsDensity,


                  # Spatial objects
                  "MainTrails" = MainTrails,
                  "HarvestablePolygons" = HarvestablePolygons,
                  "MachinePolygons" = MachinePolygons,
                  "PlotSlope" = PlotSlope,
                  "SmoothedTrails" = SmoothedTrails,
                  "MainTrailsAccess" = MainTrailsAccess,
                  "TrailsIdentity" = TrailsIdentity,
                  "RawSecondTrails" = RawSecondTrails,
                  "CostRasterAgg" = CostRasterAgg,

                  "AdjustSmoothedTrails" = AdjustSmoothedTrails,
                  "AdjustTrailsIdentity" = AdjustTrailsIdentity,
                  "AdjustRawSecondTrails" = AdjustRawSecondTrails,

                  ## POINTS
                  "HarvestableTreesPoints" = HarvestableTreesPoints,
                  "SelectedTreesPoints" = SelectedTreesPoints,
                  "FutureTreesPoints" = FutureTreesPoints,
                  "ReserveTreesPoints" = ReserveTreesPoints,
                  "HollowTreesPoints" = HollowTreesPoints,
                  "EnergywoodTreesPoints" = EnergywoodTreesPoints,

                  # INPUTS reminder
                  "INPUTinventory" = INPUTinventory,
                  "scenario" = scenario,
                  "objective" = objective,
                  "fuel" = fuel,
                  "diversification" = diversification,
                  "winching" = winching,
                  "directionalfelling" = directionalfelling,
                  "specieslax" = specieslax,
                  "objectivelax" = objectivelax
  )

  return(Outputs)

}

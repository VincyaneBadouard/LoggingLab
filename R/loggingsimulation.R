#' loggingsimulation
#'
#' @description This function allows to simulate a timber and wood energy
#'   logging on a forest plot. It covers: harvestable area definition, tree
#'   selection, main (only for ONF plots) and secondary trails layout, tree
#'   felling, landings implementation (only for ONF plots), timber harvested,
#'   fuel wood volume and damages quantification.
#'
#' @param inventory Your inventory (see the inputs formats and metadata in the
#'   \code{\link{vignette}}) (data.frame)
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'   or SRTM) (default: \code{\link{DTMParacou}}) (RasterLayer)
#'
#' @param relativeelevation (RasterLayer)
#'
#' @param speciescriteria Table of species exploitability criteria : species
#'   names, economic interest level, minimum and maximum felling diameter, in
#'   the same format of \code{\link{SpeciesCriteria}} (2 levels of commercial
#'   species) (data.frame)
#'
#' @param volumeparameters Volume parameters table (in the same format of
#'   \code{\link{ForestZoneVolumeParametersTable}}) to compute the harvestable
#'   volume of each tree, depend to its geographic zone if several locations
#'   (data.frame)
#'
#' @param scenario Logging scenario: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'   "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'   \code{\link{vignette}})
#'
#' @param objective Objective volume per hectare (numeric)
#'
#' @param fuel Fuel wood exploitation: no exploitation = "0", damage
#'   exploitation in fuelwood = "1", exploitation of hollow trees and damage in
#'   fuelwood = "2"
#'
#' @param diversification Taking of other species in addition to the main
#'   commercial species (2 levels of commercial species in the
#'   \code{\link{SpeciesCriteria}} table) (logical)
#'
#' @param winching No cable or grapple = "0", only cable = "1", grapple + cable
#'   = "2"
#'
#' @param directionalfelling Directional felling = "0" (absent), "1" (only to
#'   avoid damage to future and reserve trees), "2" (avoid damage to future and
#'   reserve trees + track orientation)
#'
#' @param specieslax Allow diversification if stand is too poor, = FALSE by
#'   default (logical)
#'
#' @param objectivelax Allow exploitation in case of non-achievement of the
#'   objective volume (if stand too poor), = FALSE by default (logical)
#'
#' @param crowndiameterparameters Crown diameter allometry parameters table (in
#'   the same format of \code{\link{ParamCrownDiameterAllometry}}) to compute
#'   the crown diameter of each tree, depend to its DBH (Diameter at Breast
#'   Height) and its species, genus or family. (data.frame)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @param iter Number of iterations (numeric). Default = 1.
#' @param cores Number of cores for parallelization (numeric). Default = 1.
#'
#' @return
#'
#' @seealso \code{\link{Paracou6_2016}}, \code{\link{SpeciesCriteria}},
#'   \code{\link{ForestZoneVolumeParametersTable}},
#'   \code{\link{ParamCrownDiameterAllometry}}, \code{\link{loggingparameters}},
#'
#'   \code{\link{addtreedim}}, \code{\link{treeselection}},
#'   \code{\link{harvestable}}, \code{\link{selected}},
#'   \code{\link{futurereserve}}, \code{\link{treefelling}},
#'   \code{\link{createcanopy}}, \code{\link{treefromthesky}},
#'   \code{\link{felling1tree}}, \code{\link{rotatepolygon}},
#'   \code{\link{getgeometry}}, \code{\link{timberharvestedvolume}},
#'   \code{\link{exploitablefuelwoodvolume}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(Paracou6_2016) # inventory
#' data(DTMParacou) # topography
#' # relative elevation
#' data(SpeciesCriteria) # species exploitability criteria
#' data(ForestZoneVolumeParametersTable) # volume parameters
#' data(ParamCrownDiameterAllometry) # parameters values of the crown diameter allometry
#'
#' loggingsimulation(inventory = Paracou6_2016, topography = DTMParacou,
#'  relativeelevation, speciescriteria = SpeciesCriteria,
#'  volumeparameters = ForestZoneVolumeParametersTable, scenario = "manual",
#'  objective = 20, fuel = "2", diversification = TRUE, winching,
#'  directionalfelling = "2", specieslax = FALSE, objectivelax = FALSE,
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
  if (!any(scenario == "RIL1" || scenario == "RIL2broken"|| scenario == "RIL2"|| scenario == "RIL3"|| scenario == "RIL3fuel"||
           scenario == "RIL3fuelhollow"|| scenario =="manual"))
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



  # # Function
  #
  # # inventorycheckformat, addtreedim, treeselection, treefelling, timberharvestedvolume, exploitablefuelwoodvolume
  #
  # # Check & format the inventory + add the tree dimensions:
  # inventory <- addtreedim(inventorycheckformat(inventory),
  #                         volumeparameters = volumeparameters,
  #                         crowndiameterparameters = crowndiameterparameters,
  #                         advancedloggingparameters = advancedloggingparameters)
  #
  # # Harvestable area definition: A FAIRE
  #
  #
  # # Main trails layout: (only for ONF plots)
  #
  #
  # # Tree selection (harvestable, future and reserve trees + defects trees):
  # treeselectionoutputs <- treeselection(inventory, topography = topography,
  #                                       scenario = scenario, objective = objective,
  #                                       fuel = fuel, diversification = diversification, specieslax = specieslax,
  #                                       objectivelax = objectivelax, plotslope = plotslope,
  #                                       speciescriteria = speciescriteria,
  #                                       advancedloggingparameters = advancedloggingparameters)
  #
  # VO <- treeselectionoutputs$VO
  # HarvestableTreesPoints <- treeselectionoutputs$HarvestableTreesPoints
  # SelectedTreesPoints <- treeselectionoutputs$SelectedTreesPoints
  # FutureTreesPoints <- treeselectionoutputs$FutureTreesPoints
  # ReserveTreesPoints <- treeselectionoutputs$ReserveTreesPoints
  # HollowTreesPoints <- treeselectionoutputs$HollowTreesPoints
  # EnergywoodTreesPoints <- treeselectionoutputs$EnergywoodTreesPoints
  #
  # # Secondary trails layout (preliminaries for fuel wood harvesting): A FAIRE
  #
  #
  # # Tree felling:
  # inventory <- treefelling(inventory, scenario = scenario, fuel = fuel,
  #                          directionalfelling = directionalfelling, MainTrail = MainTrail, ScndTrail = ScndTrail,
  #                          advancedloggingparameters = advancedloggingparameters)
  #
  #
  # # Adjusted secondary trails layout (for fuel wood harvesting only) A FAIRE
  #
  # # Landings implementation: (only for ONF plots)
  #
  # # Timber harvested volume quantification
  # Timberoutputs <- timberharvestedvolume(inventory,
  # scenario = scenario, fuel = fuel, advancedloggingparameters = advancedloggingparameters)
  #
  # LoggedVolume <- Timberoutputs$LoggedVolume
  # NoHollowLoggedVolume <- Timberoutputs$NoHollowLoggedVolume
  #
  #
  # # Exploitable fuel wood volume quantification
  # Fueloutputs <- exploitablefuelwoodvolume(inventory,
  # scenario = scenario, fuel = fuel, advancedloggingparameters = advancedloggingparameters,
  #'LoggedVolume = LoggedVolume, NoHollowLoggedVolume = NoHollowLoggedVolume)
  #
  # DamageVolume <- Fueloutputs$DamageVolume # only damage (without purge and hollow trees)
  # FuelVolume <- Fueloutputs$FuelVolume
  #
  #
  #
  #
  # Outputs <- list(inventory = inventory,
  #
  #                 VO = VO,
  #                 LoggedVolume,
  #                 NoHollowLoggedVolume,
  #                 DamageVolume, # only damage (without purge and hollow trees)
  #                 FuelVolume,
  #
  #                 HarvestableTreesPoints = HarvestableTreesPoints,
  #                 SelectedTreesPoints = SelectedTreesPoints,
  #                 FutureTreesPoints = FutureTreesPoints,
  #                 ReserveTreesPoints = ReserveTreesPoints,
  #                 HollowTreesPoints = HollowTreesPoints,
  #                 EnergywoodTreesPoints
  # )
  # #
  # return(Outputs)


}

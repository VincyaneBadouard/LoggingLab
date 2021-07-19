#' secondtrailsopening
#'
#' @param inventory (data.frame)
#' @param DEM (raster)
#' @param plotslope (raster)
#' @param plots
#' @param prospectionunit
#' @param maintrails
#' @param selectedtreespoints
#' @param hollowtreespoints
#' @param avenirtreespoints
#' @param reservetreespoints
#' @param deadtreespoints
#' @param otherlogginparameters (list)
#' @param objective Objective volume per hectare (numeric)
#' @param type "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow" or "manual"(character)
#' @param fuel no  exploitation = "0", damage exploitation in fuelwood = "1",
#' exploitation of hollow trees and damage in fuelwood = "2"
#' @param diversification  taking of other species in addition to the main commercial species (logical)
#' @param winching no cable or grapple = "0", only cable = "1", grapple + cable = "2"
#'
#' @return
#' @export
#'
#' @examples
#'
secondtrailsopening <- function(inventory,
                                DEM = DEMParacou,
                                plotslope =  PlotSlope,
                                plots = Plots,
                                prospectionunit = ProspectionUnit,
                                maintrails = MainTrails,
                                selectedtreespoints = SelectedTreesPoints,
                                hollowtreespoints = HollowTreesPoints,
                                avenirtreespoints = AvenirTreesPoint,
                                reservetreespoints = ReserveTreesPoints,
                                deadtreespoints = DeadTreesPoints,
                                otherlogginparameters = logginparameters(),
                                objective = NULL,
                                type,
                                fuel = NULL,
                                diversification = NULL,
                                winching = NULL) {

  # Arguments check
  if(!inherits(inventory, "data.frame"))
    stop("The 'inventory' arguments of the 'secondtrailsopening' function must be data.frame")

  if(!any(unlist(lapply(list(DEM, plotslope), inherits, "RasterLayer"))))
    stop("The 'DEM' and 'plotslope' arguments of the 'secondtrailsopening' function must be raster")

  # Redefinition of the parameters according to the chosen scenario
  scenariosparameters <- scenariosparameters(objective = objective, type = type, fuel = fuel,
                                             diversification = diversification)

  objective <- scenariosparameters$objective
  fuel <- scenariosparameters$fuel
  diversification <- scenariosparameters$diversification



}

#' Trails smoothing
#'
#'@param paths Raw secondary trails polygons (sp polylines)
#'
#'@param pts Harvested tree locations (sf points)
#'
#'@param plots Studied plots (sf)
#'
#'@param partMainTrails Intersections between accessible area and MainTrails (sf
#'  polygon)
#'
#'@param smthfact A positive number controlling the smoothness and level of
#'  generalization (numeric)
#'
#'@param advancedloggingparameters Other parameters of the logging simulator
#'  \code{\link{loggingparameters}} (list)
#'
#'
#'@importFrom  sf st_as_sf st_buffer st_union st_geometry st_area st_difference
#'  st_combine
#'@importFrom smoothr smooth
#'
#'@return A list with : 1: Smoothed secondary trails polygons.; 2: Second trails density
#'
#'
#'
#' @examples
#' \dontrun{
#' data(DTMParacou)
#' data(Paracou6_2016)
#' data(HarvestablePolygons)
#' data(MainTrails)
#' data(Plots)
#' data(PlotSlope)
#' data("SpeciesCriteria")
#'
#' scenarios <- scenariosparameters(scenario = "RIL3", objective = 15)
#'
#' inventory <- ONFGuyafortaxojoin(addtreedim(inventorycheckformat(Paracou6_2016),
#'  volumeparameters = ForestZoneVolumeParametersTable),SpeciesCriteria)
#'
#' AccessPolygons <- FilterAccesExplArea(harvestablepolygons = HarvestablePolygons,
#' MainTrails = MainTrails,
#' winching = scenarios$winching,
#' advancedloggingparameters = loggingparameters())
#'
#' treeselectionoutputs <- treeselection(inventory,
#' topography = DTMParacou,
#' speciescriteria = SpeciesCriteria, objective = scenarios$objective,
#' scenario ="manual", fuel = "2", diversification = TRUE, specieslax = FALSE,
#' objectivelax = TRUE, MainTrails = MainTrails, plotslope = PlotSlope,
#' harvestablepolygons = AccessPolygons,
#' advancedloggingparameters = loggingparameters())
#'
#' SecondTrailsRaw <- secondtrailsopening(DTM = DTMParacou, plotslope =  PlotSlope,plots = Plots,
#'   harvestablepolygons = HarvestablePolygons, MainTrails = MainTrails, treeselectionoutputs,
#'   CostMatrix = list(list(list(Slope = 3, Cost = 3),
#'                            list(Slope = 5, Cost = 5),
#'                            list(Slope = 12, Cost = 20),
#'                            list(Slope = 22, Cost = 60),
#'                            list(Slope = 27, Cost = 600),
#'                            list(Slope = Inf, Cost = 1000)),
#'                       list(list(CostType = "Initial", CostValue = 1000),
#'                            list(CostType = "Access", CostValue = 1000),
#'                            list(CostType = "BigTrees", CostValue = 500),
#'                            list(CostType = "Reserves", CostValue = 500),
#'                            list(CostType = "Futures", CostValue = 50),
#'                            list(CostType = "MainTrails", CostValue = 1E-4),
#'                            list(CostType = "SecondTrails", CostValue = 0.1))),
#'  scenarios = scenarios,
#'  fact = 3,
#'  advancedloggingparameters = loggingparameters())
#'
#'  SecondTrailsSmth <- smoothtrails(paths = SecondTrailsRaw[[1]],
#'                                  plots = Plots,
#'                                    )
#'}
#'
smoothtrails <- function(
  paths,
  pts,
  plots,
  partMainTrails,
  smthfact = 5,
  advancedloggingparameters = loggingparameters()
){

  ptsBuffered <- pts  %>%
    st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2) %>%
    st_union()

  secondtrails <- paths %>%
    st_as_sf()  %>%
    st_difference(partMainTrails %>% st_buffer(dist = 2) %>% st_union()) %>%
    smoothr::smooth(method = "ksmooth",smoothness = smthfact) %>%
    st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2) %>%
    st_union(ptsBuffered) %>%
    st_union() %>%
    st_intersection(st_as_sf(plots) %>% st_union())

  ScndTrailDens <- (secondtrails %>% st_area / advancedloggingparameters$ScndTrailWidth)/(plots %>% st_as_sf() %>% st_area() /10000)

  if (as.numeric(ScndTrailDens) <= 200) {
    message(paste0("The second trails density criteria is validated (", ScndTrailDens," m.ha^-1 <= 200m.ha^-1 )"))
  }else{
    message(paste0("The second trails density criteria is NOT validated (", ScndTrailDens," m.ha^-1 >= 200m.ha^-1)"))
  }


secondtrails <- list(secondtrails = secondtrails,
                     ScndTrailDens = ScndTrailDens)

  return(secondtrails)
}

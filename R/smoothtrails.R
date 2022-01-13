#' Trails smoothing
#'
#'@param paths Raw secondary trails polygons (sp polylines)
#'
#'@param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
#'
#'@param smthfact A positive number controlling the smoothness and level of
#'  generalization (numeric)
#'
#'@param verbose return message on second trails density criteria (boolean)
#'
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
#' data(PlotMask)
#' data(PlotSlope)
#' data("SpeciesCriteria")
#'
#' scenarios <- scenariosparameters(scenario = "RIL3", objective = 15)
#'
#' inventory <- commercialcriteriajoin(addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#'  volumeparameters = ForestZoneVolumeParametersTable),SpeciesCriteria)
#'
#' AccessPolygons <- filteraccesexplarea(harvestablepolygons = HarvestablePolygons,
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
#' SecondTrailsRaw <- secondtrailsopening(DTM = DTMParacou, plotslope =  PlotSlope,plotmask = PlotMask,
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
#'                                  plotmask = PlotMask,
#'                                    )
#'}
#'
smoothtrails <- function(
  paths,
  plotmask,
  smthfact = 5,
  verbose = FALSE,
  advancedloggingparameters = loggingparameters()
){

  SmoothedSecondTrails <- paths %>%
    st_as_sf() %>% filter(st_is_empty(paths %>%
                                        st_as_sf()) == FALSE)  %>%
    smoothr::smooth(method = "ksmooth", smoothness = smthfact) %>%
    st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2) %>%
    st_union() %>%
    st_intersection(st_as_sf(plotmask) %>% st_union())

  TrailsDensity <- (SmoothedSecondTrails %>% st_area / advancedloggingparameters$ScndTrailWidth)/(plotmask %>% st_as_sf() %>% st_area() /10000)
  if (verbose) {
    if (as.numeric(TrailsDensity) <= 200) {
      message(paste0("The second trails density criteria is validated (", round(TrailsDensity, digits = 1)," m/ha <= 200 m/ha)"))
    }else{
      message(paste0("The second trails density criteria is NOT validated (", round(TrailsDensity, digits = 1)," m/ha > 200 m/ha)"))
    }
  }



secondtrails <- list(SmoothedSecondTrails = SmoothedSecondTrails,
                     TrailsDensity = TrailsDensity)

  return(secondtrails)
}

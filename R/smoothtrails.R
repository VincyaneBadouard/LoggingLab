#' Trails smoothing
#'
#'@param paths Raw secondary trails polygons (sp polylines)
#'
#'@param plotmask Inventoried plot mask (SpatialPolygonsDataFrame)
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
#'@return A list with :
#'   1) SmoothedTrails: Smoothed trails polygons;
#'   2) Smoothed second trails polygons;
#'   3) Smoothed main trails polygons;
#'   4) smoothed trail density.
#'
#'
#'
#' @examples
#' data(DTMParacou)
#' data(Paracou6_2016)
#' data(HarvestableAreaOutputsCable)
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
#' treeselectionoutputs <- treeselection(inventory,
#' topography = DTMParacou,
#' speciescriteria = SpeciesCriteria,
#' scenario = "manual", objective = 10, fuel = "2", diversification = TRUE,
#' winching = "0", specieslax = FALSE, objectivelax = TRUE,
#' plotslope = HarvestableAreaOutputsCable$PlotSlope,
#' harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#' advancedloggingparameters = loggingparameters())
#'
#' SecondTrailsRaw <- secondtrailsopening(DTM = DTMParacou, plotslope =  PlotSlope,plotmask = PlotMask,
#'   harvestablepolygons = HarvestablePolygons, maintrails = MainTrails, treeselectionoutputs,
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
#'
smoothtrails <- function(
  paths,
  plotmask,
  verbose = FALSE,
  advancedloggingparameters = loggingparameters()
){

  # Global Variables
  n.overlaps <- NULL

  RawSecondTrails <- paths %>%
    st_as_sf() %>% filter(!st_is_empty(paths %>%
                                        st_as_sf()))

  SmoothedSecondTrails <- RawSecondTrails %>% st_intersection() %>% filter(n.overlaps < 20) %>%
    st_union() %>% st_cast("LINESTRING") %>%
    smoothr::smooth(method = "ksmooth", smoothness = advancedloggingparameters$SmoothingFact) %>%
    st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2) %>%
    st_union() %>%
    st_intersection(st_as_sf(plotmask) %>% st_union())

  RawMainTrails <- RawSecondTrails %>% st_intersection() %>% filter(n.overlaps >= 20)

  if (dim(RawMainTrails)[1] > 0) {
    SmoothedMainTrails <- RawMainTrails %>%
      st_union() %>% st_cast("LINESTRING") %>%
      smoothr::smooth(method = "ksmooth", smoothness = advancedloggingparameters$SmoothingFact) %>%
      st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth/2) %>%
      st_union() %>%
      st_intersection(st_as_sf(plotmask) %>% st_union())

    SmoothedTrails <- st_union(SmoothedSecondTrails,SmoothedMainTrails)
  }else{
    SmoothedMainTrails <- NULL

    SmoothedTrails <- SmoothedSecondTrails
  }




  TrailsDensity <- (SmoothedTrails %>% st_area / advancedloggingparameters$ScndTrailWidth)/(plotmask %>% st_as_sf() %>% st_area() /10000)
  if (verbose) {
    if (as.numeric(TrailsDensity) <= 200) {
      message(paste0("The second trails density criteria is validated (", round(TrailsDensity, digits = 1)," m/ha <= 200 m/ha)"))
    }else{
      message(paste0("The second trails density criteria is NOT validated (", round(TrailsDensity, digits = 1)," m/ha > 200 m/ha)"))
    }
  }



  smoothedtrails <- list("SmoothedTrails" = SmoothedTrails,
                     "SmoothedSecondTrails" = SmoothedSecondTrails,
                     "SmoothedMainTrails" = SmoothedMainTrails,
                     "TrailsDensity" = TrailsDensity)

  return(smoothedtrails)
}

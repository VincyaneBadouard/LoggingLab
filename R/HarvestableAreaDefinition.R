#' Harvestable area definition
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer)
#'
#' @param creekdistances Relative distances (vertical and horizontal) from nearest channel network
#'   (list of 2 large RasterLayers)
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with:
#' - 'HarvestablePolygons': a collection of polygons (sf (sfc_POLYGON)) defined as:
#'    1 : harvestable area,
#'    0 : non-harvestable area
#' - 'PlotSlope': a raster with slope (in radians) characteristic of the studied
#'    plot (Large RasterLayer)
#' - HarvestableArea : the harvestable area in hectares (double).
#'
#'
#' @export
#'
#' @importFrom sf st_as_sf st_cast st_area
#' @importFrom raster mask terrain rasterFromXYZ rasterToPolygons
#'   rasterToPoints
#' @importFrom dplyr as_tibble left_join rename mutate if_else
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data(PlotMask)
#' data(DTMParacou)
#' data(creekdistances)
#'
#' HarvestableAreaOutputs <- harvestableareadefinition(
#'   topography = DTMParacou,
#'   creekdistances = creekdistances,
#'   advancedloggingparameters = loggingparameters()
#'   )
#'
#' library(ggplot2)
#' library(sf)
#' ggplot() +
#' geom_sf(data = HarvestableAreaOutputs[[1]],
#'         aes(alpha = Harvestable),
#'         fill = "olivedrab") +
#'   labs(alpha = "Harvestable") +
#'   labs(title = "P6  Harvestable zones")
#'
#' HarvestableAreaOutputs[[2]]
#' }
#'
harvestableareadefinition <- function(
  topography,
  creekdistances,
  advancedloggingparameters = loggingparameters()
){



  # Variables
  PlotSlope <- PlotSlopePoint <- CreekVHeightPlotPoint <- PlotTib <- NULL
  SlpCrit <- PlotSlopeCreekVHeight <- RasterHarvestable <- PolygonHarvestable <- NULL
  sf_PolygonHarvestable <- HarvestablePolygons <- CreekVHeight<- slope <-  NULL


  # Slope Calculation
  PlotSlope <- terrain(topography,
                       opt = "slope",
                       units = 'radians',
                       neighbors = 8)
  # RastersToPoints

  PlotSlopePoint <-
    as_tibble(rasterToPoints(PlotSlope))

  CreekVHeightPlotPoint <-
    as_tibble(rasterToPoints(creekdistances$distvert))

  CreekHDistPlotPoint <-
    as_tibble(rasterToPoints(creekdistances$distHorz))

  # Join tibbles by x and y
  PlotTib <-
    left_join(PlotSlopePoint, CreekVHeightPlotPoint, by = c('x', 'y')) %>%
  left_join(CreekHDistPlotPoint, by = c('x', 'y'))

  SlpCrit <- atan(advancedloggingparameters$MaxAreaSlope/100)
  CreekHDistCrit <- advancedloggingparameters$WaterSourcesBufferZone
  CreekVDistCrit <- advancedloggingparameters$WaterSourcesRelativeHeight

  PlotTib %>% rename("CreekVHeight" = names(PlotTib[4]),"CreekHDist" = names(PlotTib[5]))  %>%
    mutate(Harvestable = if_else(
      condition = CreekVHeight > CreekVDistCrit &
        CreekHDist > CreekHDistCrit &
        slope <= SlpCrit,
      true = 1,
      false = 0
    )) -> PlotSlopeCreekDist # Identify harvestable area (1) /  non-harvestable area (0) by slope and Creek Vertical Height




  # transform tibble to raster
  RasterHarvestable <-
    rasterFromXYZ(PlotSlopeCreekDist, crs = raster::crs(topography)) # set crs to WGS84 UTM 22N

  # raster to polygon
  PolygonHarvestable <-
    rasterToPolygons(x = RasterHarvestable$Harvestable,
                     n = 16,
                     dissolve = TRUE)



  sf_PolygonHarvestable <- st_as_sf(PolygonHarvestable) # transform PolygonExploit to an sf object

  # Disaggregate PolygonExploit


  HarvestablePolygons <-
    st_cast(x = sf_PolygonHarvestable, to = "POLYGON", warn = FALSE)

  HarvestableArea <- as.numeric(sum(st_area(HarvestablePolygons))/10000) # 1 ha = 10 000 m2 (as numeric to remove unit)


  return(list(
    HarvestablePolygons = HarvestablePolygons,
    PlotSlope = PlotSlope,
    HarvestableArea = HarvestableArea
  ))

}


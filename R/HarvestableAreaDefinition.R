#' HarvestableAreaDefinition
#'
#' @param plot Studied plot (SpatialPolygonsDataFrame)
#' @param dtm Digital terrain model (Large RasterLayer)
#' @param verticalcreekheight Relative elevation from nearest channel network
#'   (Large RasterLayer)
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A collection of polygons defined as 1 : harvestable area, 0 :
#'   non-harvestable area
#'
#' @export
#'
#' @importFrom sf st_as_sf st_cast
#' @importFrom raster mask terrain rasterFromXYZ rasterToPolygons
#'   rasterToPoints
#' @importFrom  dplyr as_tibble left_join rename mutate if_else
#'
#' @examples
#' data(Plots)
#' data(DTMParacou)
#' data(VerticalCreekHeight)
#'
#'
#' HarvestableArea <- HarvestableAreaDefinition(plot = Plots,
#'                                              dtm = DTMParacou,
#'                                              verticalcreekheight = VerticalCreekHeight,
#'                                              advancedloggingparameters = loggingparameters())
#' plot(HarvestableArea)
#'
HarvestableAreaDefinition <- function(
  plot,
  dtm,
  verticalcreekheight,
  advancedloggingparameters
){

  # Variables
  PlotSlope <- PlotSlopePoint <- CreekVHeightPlotPoint <- PlotTib <- NULL
  SlpCrit <- PlotSlopeCreekVHeight <- RasterExploit <- PolygoneExploit <- NULL
  sf_PolygoneExploit <- ExploitPolygones <- CreekVHeight<- slope <-  NULL


  # Mask rasters by plot
  #PlotTopo <- raster::mask(x = dtm,
  #   mask = plot) # Mask topography raster by plot
  #CreekVHeightPlot <- raster::mask(x = verticalcreekheight,
  #   mask = plot) # Mask verticalcreekheight raster by plot

  # Slope Calculation
  PlotSlope <- terrain(dtm,
                       opt = "slope",
                       units = 'radians',
                       neighbors = 8)
  # RastersToPoints

  PlotSlopePoint <-
    as_tibble(rasterToPoints(PlotSlope))

  CreekVHeightPlotPoint <-
    as_tibble(rasterToPoints(verticalcreekheight))

  # Join tibbles by x and y
  PlotTib <-
    left_join(PlotSlopePoint, CreekVHeightPlotPoint, by = c('x', 'y'))

  SlpCrit <- atan(advancedloggingparameters$MaxAreaSlope/100)

  PlotTib %>% rename("CreekVHeight" = names(PlotTib[4]))  %>%
    mutate(Exploit = if_else(
      condition = CreekVHeight > 2 &
        slope <= SlpCrit,
      true = 1,
      false = 0
    )) -> PlotSlopeCreekVHeight # Identify harvestable area (1) /  non-harvestable area (0) by slope and Creek Vertical Height




  # transform tibble to raster
  RasterExploit <-
    rasterFromXYZ(PlotSlopeCreekVHeight, crs = 32622) # set crs to WGS84 UTM 22N

  # raster to polygon
  PolygoneExploit <-
    rasterToPolygons(x = RasterExploit$Exploit,
                     n = 16,
                     dissolve = TRUE)



  sf_PolygoneExploit <- st_as_sf(PolygoneExploit) # transform PolygonExploit to an sf object

  # Disaggregate PolygonExploit

  ExploitPolygones <-
    st_cast(x = sf_PolygoneExploit, to = "POLYGON", warn = FALSE)

  return(ExploitPolygones)

}


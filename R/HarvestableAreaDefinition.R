#' HarvestableAreaDefinition
#'
#' @param plot Studied plots (sf data.frame)
#' @param DEM Digital elevation model (raster)
#' @param HCrique Relative elevation from nearest channel network (raster)
#' @param advancedloggingparameters Other parameters of the logging simulator
#'
#' @return A collection of polygones defined as 1 : harvestable area / 0 : non-harvestable area
#'
#' @export
#'
#' @import sf
#' @importFrom  raster mask terrain rasterFromXYZ rasterToPolygons
#' @import dplyr
#'
#' @examples
#'
#' ExploitPolygones <- HarvestableAreaDefinition(plot = Plots,DEM = DEM,verticalcreekheight = VerticalCreekHeight)
#'
HarvestableAreaDefinition <- function(plot = Plots,
                                      dtm = DTMParacou,
                                      verticalcreekheight = VerticalCreekHeight,
                                      advancedloggingparameters = loggingparameters()) {


  # Mask rasters by plot
  #PlotTopo <- raster::mask(x = dtm,
                        #   mask = plot) # Mask topography raster by plot
  #CreekVHeightPlot <- raster::mask(x = verticalcreekheight,
                           #   mask = plot) # Mask verticalcreekheight raster by plot

  # Slope Calculation
  PlotSlope <- raster::terrain(dtm,
                               opt = "slope",
                               units = 'radians',
                               neigbors = 8)
  # RastersToPoints

  PlotSlopePoint <-
    dplyr::as_tibble(raster::rasterToPoints(PlotSlope))

  CreekVHeightPlotPoint <-
    dplyr::as_tibble(raster::rasterToPoints(verticalcreekheight))

  # Join tibbles by x and y
  PlotTib <-
    dplyr::left_join(PlotSlopePoint, CreekVHeightPlotPoint, by = c('x', 'y'))

    SlpCrit <- atan(advancedloggingparameters$MaxAreaSlope/100)

  PlotTib %>% dplyr::rename("CreekVHeight" = names(PlotTib[4]))  %>%
    mutate(Exploit = if_else(
      condition = CreekVHeight > 2 &
        slope <= SlpCrit ,
      true = 1,
      false = 0
    )) -> PlotSlopeCreekVHeight # Identify harvestable area (1) /  non-harvestable area (0) by slope and Creek Vertical Height




  # transform tibble to raster
  RasterExploit <-
    raster::rasterFromXYZ(PlotSlopeCreekVHeight, crs = 32622) # set crs to WGS84 UTM 22N

  # raster to polygon
  PolygoneExploit <-
    raster::rasterToPolygons(x = RasterExploit$Exploit,
                             n = 16,
                             dissolve = TRUE)



  sf_PolygoneExploit <- sf::st_as_sf(PolygoneExploit) # transform PolygonExploit to an sf object

  # Disaggregate PolygonExploit

  ExploitPolygones <-
    sf::st_cast(x = sf_PolygoneExploit, to = "POLYGON")

  return(ExploitPolygones)

}


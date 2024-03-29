#' Harvestable zones definition within the plot
#'
#' @description The simulator avoids the water sources buffer zones and slopes
#'   inaccessible to machinery by taking the highest maximum slope percentage
#'   tolerated by the machines.
#'
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'
#' @param creekverticaldistance Relative vertical distance
#'  (1 m resolution) from nearest channel network
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{CreekDistances}})
#'  To generate creek distances: \code{\link{CreekDistances}} in 'Articles'.
#'
#' @param creekhorizontaldistance Relative horizontal distance
#'  (1 m resolution) from nearest channel network
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{CreekDistances}})
#'  To generate creek distances: \code{\link{CreekDistances}} in 'Articles'.
#'
#' @param maintrails Main trails defined at the entire harvestable area (sf
#'   linestring **with a crs in UTM**)
#'
#' @param plotmask Inventoried plot mask
#' (SpatialPolygonsDataFrame **with a crs in UTM**)
#'
#' @param scenario Logging scenario among: "RIL1", "RIL2broken", "RIL2", "RIL3",
#'  "RIL3fuel", "RIL3fuelhollow" or "manual"(character) (see the
#'  vignette)
#'
#' @param winching
#' "0": no cable or grapple (trail to tree foot)
#' "1": only cable (default = 40m)
#' "2": grapple (default = 6m) + cable (grapple priority)
#' If grapple + cable (winching = "2") without fuel wood (fuel = "0")
#'  recovery of the tree foot with grapple if possible (respected grapple
#'  conditions) otherwise with cable with angle to the trail.
#'  Avoidance of future/reserves if chosen.
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return A list with:
#' - 'HarvestablePolygons': a collection of polygons (sfc_MULTIPOLYGON, with
#'   crs) defined as:
#'    1 : harvestable zone,
#'    0 : non-harvestable zone
#' - 'PlotSlope': Slopes of the plot (in radians) characteristic of the studied
#'    plot (Large RasterLayer with crs)
#' - HarvestableArea : the harvestable area in hectares (double)
#' - MachinePolygons : a collection of polygons (sf (sfc_POLYGON with crs))
#'   defined as:
#'    1 : accessible machine zone,
#'    0 : non-accessible machine zone
#'
#'
#' @export
#'
#' @importFrom sf st_as_sf st_union st_cast st_area st_buffer st_drop_geometry
#' @importFrom raster mask terrain rasterFromXYZ rasterToPolygons
#'   rasterToPoints extend aggregate focal disaggregate res
#' @importFrom dplyr as_tibble left_join rename mutate if_else
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom sp coordinates proj4string
#'
#' @examples
#' \dontrun{
#' data(PlotMask)
#' data(DTMParacou)
#' data(CreekDistances)
#' data(MainTrails)
#'
#' HarvestableAreaOutputsCable <- harvestableareadefinition(
#'   topography = DTMParacou,
#'   creekverticaldistance = CreekDistances$distvert,
#'   creekhorizontaldistance = CreekDistances$disthorz,
#'   maintrails = MainTrails,
#'   plotmask = PlotMask,
#'   scenario = "manual", winching = "1",
#'   advancedloggingparameters = loggingparameters()
#'   )
#'}
#' data(HarvestableAreaOutputsCable)
#' library(ggplot2)
#' library(sf)
#' ggplot() +
#'   # Harvestable zones
#'   geom_sf(data = HarvestableAreaOutputsCable$HarvestablePolygons,
#'         fill = "olivedrab", alpha = 0.1) +
#'    geom_sf(data = HarvestableAreaOutputsCable$MachinePolygons,
#'         fill = "olivedrab", alpha = 0.5) +
#'   labs(alpha = "Harvestable") +
#'   labs(title = "Paracou P6 - Harvestable zones") +
#'   scale_colour_manual(values = c("Harvestable area" = "olivedrab"))
#'
#' HarvestableAreaOutputsCable$PlotSlope
#'
harvestableareadefinition <- function(
    topography,
    creekverticaldistance,
    creekhorizontaldistance,
    maintrails,
    plotmask,
    scenario,
    winching = NULL,
    advancedloggingparameters = loggingparameters()
){

  creekdistances <- list("distvert" = creekverticaldistance,
                         "disthorz" = creekhorizontaldistance)
  # Arguments check

  if(!all(unlist(lapply(list(topography, creekdistances$distvert, creekdistances$disthorz), inherits, "RasterLayer"))))
    stop("The 'topography', 'plotslope', 'creekverticaldistance', and 'creekhorizontaldistance' arguments
    of the 'harvestableareadefinition' function must be RasterLayer")


  if(!(scenario %in% c("RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel", "RIL3fuelhollow", "manual")))
    stop(paste('"scenario" argument should be in "RIL1", "RIL2broken", "RIL2", "RIL3", "RIL3fuel",
               "RIL3fuelhollow" or "manual"; not',
               scenario))

  if(!class(winching) %in% c("character", "NULL"))
    stop("The 'winching' argument should be character or null.")

  if(scenario == "manual" &&
     (is.null(winching)))
    stop("If you choose the 'manual' mode,
         you must fill in the arguments 'winching' ")

  if(!inherits(maintrails$geometry,"sfc_LINESTRING"))
    stop("The 'maintrails' arguments of the 'harvestableareadefinition' function must be a sfc_LINESTRING object")

  # Options
  options("rgdal_show_exportToProj4_warnings"="none") # to avoid gdal warnings

  # Variables
  PlotSlope <- PlotSlopePoint <- CreekVHeightPlotPoint <- PlotTib <- CreekHDist <- NULL
  SlpCrit <- PlotSlopeCreekVHeight <- RasterHarvestable <- PolygonHarvestable <- Access <- NULL
  sf_PolygonHarvestable <- HarvestablePolygons <- CreekVHeight<- slope <- fact <-  NULL
  accesspolygones <- accesspolygonesMain <- accesspolygonesCable <- Harvestable <- NULL


  #### Redefinition of the parameters according to the chosen scenario ####
  scenariosparameters <- scenariosparameters(scenario = scenario, winching = winching)
  winching <- scenariosparameters$winching
  ##################################

  factAgg <- floor(advancedloggingparameters$SlopeDistance/res(topography)[1])

  # Slope Calculation

  fact <- floor(advancedloggingparameters$ResamplDistDTM/res(topography)[1])

  if (res(topography)[1] < 5) {
    DTMExtExtended <- extend(aggregate(topography,fact = fact, fun=mean), c(1,1)) # extend the extent

    fill.boundaries <- function(x) {
      center = 0.5 + (fact*fact/2)
      if( is.na(x)[center] ) {
        return( round(mean(x, na.rm=T),5) )
      } else {
        return( x[center] )
      }
    }

    DTMExtended <- focal(DTMExtExtended,
                         matrix(1,fact,fact),
                         fun=fill.boundaries,
                         na.rm=F, pad=T)

    PlotSlope <- disaggregate(terrain(DTMExtended,
                                      opt = "slope",
                                      units = 'radians',
                                      neighbors = 8),fact = fact, method = 'bilinear')
    PlotSlope <- crop(PlotSlope,plotmask)
    PlotSlope <- mask(PlotSlope,plotmask)
  }else{
    topography <- disaggregate(topography,fact = fact,method = 'bilinear')
    PlotSlope <- terrain(topography,
                         opt = "slope",
                         units = 'radians',
                         neighbors = 8)

  }


  # RastersToPoints

  PlotSlopePoint <-
    as_tibble(rasterToPoints(PlotSlope))

  PlotTib <- tibble(x = PlotSlopePoint$x)
  PlotTib$y <- PlotSlopePoint$y

  coordinates(PlotSlopePoint) <- ~ x + y
  proj4string(PlotSlopePoint) <- crs(topography)


  PlotSlopePoint$CreekVHeight <- raster::extract(creekdistances$distvert,PlotSlopePoint)

  PlotSlopePoint$CreekHDist <- raster::extract(creekdistances$disthorz,PlotSlopePoint)

  PlotTib <- cbind(PlotTib,PlotSlopePoint %>% st_as_sf() %>% st_drop_geometry())

  # Take the maximum slope threshold:
  maxslope <- max(advancedloggingparameters$MaxTrailCenterlineSlope,
                  advancedloggingparameters$GrappleMaxslope)

  SlpCrit <- atan(maxslope/100)

  CreekHDistCrit <- advancedloggingparameters$WaterSourcesBufferZone
  CreekVDistCrit <- advancedloggingparameters$WaterSourcesRelativeHeight

  PlotTib  %>%
    mutate(Harvestable = if_else(
      condition = CreekVHeight > CreekVDistCrit &
        CreekHDist > CreekHDistCrit &
        slope <= SlpCrit,
      true = 1,
      false = 0
    )) -> PlotSlopeCreekDist # Identify harvestable area (1) / non-harvestable area (0) by slope and Creek Vertical Height
  RasterHarvestableFoT <-
    try(rasterFromXYZ(PlotSlopeCreekDist, crs = raster::crs(topography)), silent=TRUE)

  PolygonHarvestableFoT <- raster::rasterToPolygons(x = RasterHarvestableFoT$Harvestable,
                                                    n = 16,
                                                    dissolve = TRUE)



  HarvestablePolygonsFoT <- PolygonHarvestableFoT %>%
    st_as_sf() %>%
    st_cast(to = "POLYGON", warn = FALSE)


  maintrailsWIP <- maintrails %>%
    st_buffer(dist = advancedloggingparameters$MaxMainTrailWidth + factAgg)

  accesspolygonesFoT <- HarvestablePolygonsFoT %>%
    filter(Harvestable == 1) %>%
    st_union() %>% st_cast("POLYGON") %>% st_as_sf()

  accesspolygonesFoT <- accesspolygonesFoT %>%
    filter(as.numeric(st_area(accesspolygonesFoT)) > factAgg^2 * res(topography)[1] )

  accesspolygonesMainFoT <- accesspolygonesFoT %>%
    st_buffer(dist =  -advancedloggingparameters$ScndTrailWidth/2) %>%
    mutate("Access" = as.vector(st_intersects(accesspolygonesFoT, maintrailsWIP, sparse = FALSE))) %>%
    dplyr::filter(Access == TRUE) %>%
    st_union() %>% st_buffer(dist = advancedloggingparameters$ScndTrailWidth/2)

  if (winching != "0") {

    SlpCrit <- atan(advancedloggingparameters$CableTreesMaxSlope/100)
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
    RasterHarvestableCbl <-
      rasterFromXYZ(PlotSlopeCreekDist, crs = raster::crs(topography))

    PolygonHarvestableCbl <- raster::rasterToPolygons(x = RasterHarvestableCbl$Harvestable,
                                                      n = 16,
                                                      dissolve = TRUE)

    HarvestablePolygonsCbl <- PolygonHarvestableCbl %>% st_as_sf() %>% st_cast(to = "POLYGON", warn = FALSE)
    accesspolygonesCbl <- HarvestablePolygonsCbl %>% filter(Harvestable == 1) %>%
      st_union() %>% st_cast("POLYGON") %>% st_as_sf()

    accesspolygonesCbl <- accesspolygonesCbl %>%
      filter(as.numeric(st_area(accesspolygonesCbl)) > factAgg^2 * res(topography)[1] )

    accesspolygonesCable <- accesspolygonesMainFoT  %>%
      st_union()  %>%
      st_buffer(dist = advancedloggingparameters$CableLength-0.1) %>%
      st_intersection(accesspolygonesCbl %>%
                        st_union()) %>%
      st_cast("POLYGON") %>% st_as_sf()

    accesspolygones <- accesspolygonesCable %>%
      st_buffer(dist =  -advancedloggingparameters$ScndTrailWidth/2) %>%
      st_buffer(dist =  advancedloggingparameters$ScndTrailWidth/2)%>%
      filter(as.numeric(st_area(accesspolygonesCable)) > factAgg^2 * res(topography)[1] ) %>%
      st_union()


  }else{

    accesspolygones <- accesspolygonesMainFoT
  }

  # transform tibble to raster
  # set crs to WGS84 UTM 22N

  # raster to polygon


  HarvestableArea <- as.numeric(sum(st_area(accesspolygones))/10000) # 1 ha = 10 000 m2 (as numeric to remove unit)


  return(list(
    HarvestablePolygons = accesspolygones,
    PlotSlope = PlotSlope,
    HarvestableArea = HarvestableArea,
    MachinePolygons = accesspolygonesMainFoT
  ))

}


#' st_temporaryintersection
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'
#' @param plotmask Inventoried plot mask
#' (SpatialPolygonsDataFrame **with a crs in UTM**)
#'
#' @param treebuffers Polygons vector
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @return sf_Polygons
#'
#' @importFrom raster raster extent res crs mask crop rasterToPolygons values
#' @importFrom terra rast as.polygons
#' @importFrom fasterize fasterize
#' @importFrom dplyr rename mutate
#' @importFrom sf as_Spatial st_as_sf st_centroid st_intersects st_set_crs
#' st_crs st_cast st_join st_geometry
#'
#'
#' @export
#' @examples
#' \dontrun{
#' data(Paracou6_2016)
#' data(DTMParacou)
#' data(PlotMask)
#' data(SpeciesCriteria)
#' data(ForestZoneVolumeParametersTable)
#' data(HarvestableAreaOutputsCable)
#'
#' inventory <- addtreedim(cleaninventory(Paracou6_2016, PlotMask),
#' volumeparameters = ForestZoneVolumeParametersTable)
#'
#' treeselectionoutputs <- suppressWarnings(treeselection(
#'   inventory,
#'   topography = DTMParacou,
#'   speciescriteria = SpeciesCriteria,
#'   scenario = "manual", objective = 20,
#'   fuel = "0",
#'   diversification = FALSE,
#'   winching = "0",
#'   specieslax = FALSE, objectivelax = TRUE,
#'   plotslope = HarvestableAreaOutputsCable$PlotSlope,
#'   harvestablepolygons = HarvestableAreaOutputsCable$HarvestablePolygons,
#'   advancedloggingparameters = loggingparameters()))
#'
#' treebuffers <- treeselectionoutputs$SelectedTreesPoints %>% sf::st_buffer(40)
#'
#' st_temporaryintersection(DTMParacou, PlotMask, treebuffers)
#' }
#'
st_temporaryintersection <- function(
  topography,
  plotmask,
  treebuffers,
  advancedloggingparameters = loggingparameters()
){

  # Arguments check

  if(!inherits(treebuffers$geometry, "sfc_POLYGON"))
    stop("The 'treebuffers' arguments of the 'st_temporaryintersection'
         function must be a sf POLYGON object")

  if(!inherits(plotmask, "SpatialPolygonsDataFrame"))
    stop("The 'plotmask' argument of the 'st_temporaryintersection' function must be
         SpatialPolygonsDataFrame")

  if(!inherits(topography, "RasterLayer"))
    stop("The 'topography' argument of the 'st_temporaryintersection' function must
         be RasterLayer")

  if(st_is_empty(treebuffers$geometry)[1]) {
    stop("The 'treebuffers' argument does not contain any object.")
  }

  if(!inherits(advancedloggingparameters, "list"))
    stop("The 'advancedloggingparameters' argument of the 'st_temporaryintersection' function must be a list")

  # Global Variables
  layer <- NULL

  # Generate a raster
  RasterAccum <- raster(extent(topography), resolution = res(topography)/5, crs = crs(topography))
  values(RasterAccum) <- 0
  RasterAccum <- mask(RasterAccum, plotmask)
  RasterAccum <- try(crop(RasterAccum, plotmask), silent=TRUE) # "Error in x$.self$finalize() : attempt to apply non-function"

  RasterOrigins <- RasterAccum
  values(RasterOrigins) <- NA


  # Raster update
  for(p in 1:dim(treebuffers)[1] ){
    RasterOrigins <- fasterize(sf = treebuffers[p,],raster = RasterAccum,field = "idTree",background = 0)
    RasterOrigins@data@values[RasterOrigins@data@values == 0] <- NA
    if (p==1) {
      sf_OriginsPolygons<- terra::rast(RasterOrigins) %>%
        terra::as.polygons(dissolve = TRUE) %>%
        st_as_sf() %>% st_set_crs(st_crs(treebuffers)) %>%
        rename(idTree = layer) %>%
        st_cast("POLYGON",warn = FALSE)
    }else{
      sf_OriginsPolygons <- rbind(sf_OriginsPolygons, terra::rast(RasterOrigins) %>%
                                    terra::as.polygons(dissolve = TRUE) %>%
                                    st_as_sf() %>% st_set_crs(st_crs(treebuffers))%>%
                                    rename(idTree = layer) %>%
                                    st_cast("POLYGON",warn = FALSE))
    }

    RasterAccum <- RasterAccum + fasterize(sf = treebuffers[p,],raster = RasterAccum,background = 0)
  }


  RasterAccum <- mask(RasterAccum, plotmask)
  RasterAccum <- crop(RasterAccum, plotmask)

  RasterAccum@data@values[RasterAccum@data@values == 0] <- NA

  # Raster to polygons
  sf_AccumPolygons <- terra::rast(RasterAccum) %>%
    terra::as.polygons(dissolve = TRUE) %>%
    st_as_sf() %>% st_set_crs(st_crs(treebuffers)) %>%
    rename(n.overlaps = layer) %>% st_cast("POLYGON",warn = FALSE)
  sf_interPolygons <- suppressWarnings(sf_AccumPolygons%>%
                                         mutate(origins = c(sf_AccumPolygons %>%
                                                              st_centroid() %>%
                                                              st_intersects(sf_OriginsPolygons)))%>%
                                         st_join(treebuffers))



  return(sf_interPolygons)

}

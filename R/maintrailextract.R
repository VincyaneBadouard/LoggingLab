#' Generate main skidding trail of the inventoried plot
#'
#' @param topography Digital terrain model (DTM) of the inventoried plot (LiDAR
#'  or SRTM) (\code{\link{DTMParacou}}) (RasterLayer **with a crs in UTM**)
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#' @param advancedloggingparameters Other parameters of the logging simulator
#'   \code{\link{loggingparameters}} (list)
#'
#' @details The main trails are defined as lines at the edge and outside of the
#'   plot.
#'
#' @return The main trail at the edge and outside of the topography raster (sf
#'   LINESTRING object **with a crs in UTM**)
#'
#' @export
#'
#' @importFrom raster res extend focal rasterToPolygons
#' @importFrom sf st_as_sf st_cast
#'
#' @examples
#' data(DTMParacou)
#' \dontrun{
#' MainTrails <- maintrailextract(DTMParacou)
#' }
#' data(MainTrails)
#' library(raster)
#' plot(DTMParacou)
#' plot(MainTrails, add = TRUE)
#'
maintrailextract <- function(topography,
                             advancedloggingparameters = loggingparameters()){

  # Arguments check

  if(!inherits(topography,"RasterLayer"))
    stop("The 'topography' arguments of the 'maintrailextract' function must be RasterLayer")

  # Variables
  fact <- DTMExtExtended <- DTMExtended <- preMainTrails <- NULL
  # MainTrails <- NULL

  #function

  fact <- floor(advancedloggingparameters$SlopeDistance/res(topography)[1])

    DTMExtExtended <- raster::extend(x = topography,y =  fact) # extend the extent

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

    preMainTrails <- DTMExtended > -Inf
    preMainTrails<- rasterToPolygons(preMainTrails, dissolve=T)
    MainTrails <- preMainTrails %>% st_as_sf() %>% st_cast(to = 'LINESTRING', warn= FALSE)

  return(MainTrails)

}

#' Get mask
#'
#' @param inventory Input inventory (see the inputs formats and metadata in the
#'   vignette) (data.frame)
#'
#'@param topography Digital terrain model (DTM) of the inventoried plot (LiDAR,
#'  1m resolution)
#'  (RasterLayer **with a crs in UTM**) (See \code{\link{DTMParacou}})
#'  We advise you to generate your raster with Qgis rather than with the
#'  'raster' package on R.
#'
#' @return The inventoried plot mask (SpatialPolygonsDataFrame
#'  **with a crs in UTM**)
#'
#' @export
#'
#' @importFrom dplyr select filter summarise
#' @importFrom sf st_as_sf st_set_crs st_crs st_cast st_convex_hull
#'   as_Spatial
#' @importFrom raster extract raster
#'
#' @examples
#' data(Paracou6_2016)
#' data(DTMParacou)
#' mask <- getmask(Paracou6_2016, DTMParacou)
#'
getmask <- function(
  inventory,
  topography
){

  idTree <- Xutm <- Yutm <- coords <- NULL

  # Argument check

  ## inventory
  if (!inherits(inventory, "data.frame"))
    stop("The 'inventory' argument must be a data.frame")
  inventory <- inventorycheckformat(inventory)

  ## topography as Sp or sf
  if(!inherits(topography, "RasterLayer")){
    if(inherits(topography, "SpatRaster"))
      topography <- raster(topography)
    if(!inherits(topography, "SpatRaster"))
      stop("The 'topography' argument of the 'loggingsimulation' function must be a RasterLayer")
  }

  # Create mask

  ## prepare inventory
  inventory <- inventory %>%
    select(idTree, Xutm, Yutm) %>%
    st_as_sf(coords = c("Xutm", "Yutm")) %>% # as sf
    st_set_crs(st_crs(topography)) # set crs
  inventory$topography <- extract(topography, inventory)
  inventory <- inventory %>%
    filter(!is.na(topography))
  if(nrow(inventory) < 1)
    stop("The inventory does not match your topography. Please check your inputs.")

  ## make mask
  mask <- inventory %>%
    summarise() %>%
    st_cast("POLYGON") %>%
    st_convex_hull() %>%
    as_Spatial()

  return(mask)
}

#' Secondary trails of Paracou plot 6
#'
#' A list with :
#'
#' - inventory: Updated inventory
#'
#' - SmoothedTrails: Smoothed secondary trails polygons
#' (sfc_MULTIPOLYGON with crs)
#'
#' - TrailsDensity: Second trails density (in m/ha)
#'
#' - TrailsIdentity: information on sections of the trails (matrix) with:
#'     - LineID:
#'     - LoggedTrees: idTree of trees reached by the trails
#'     - TypeExpl: type of winching
#'
#' - MainTrailsAccess : Random access point of main trail for each harvestable
#'   zone (sfc_POINT with crs)
#'
#' - RawSecondTrails : non-smoothed secondary trails (SpatialLines with crs)
#'
#' - CostRasterAgg: The cost raster (RasterLayer  with crs)
#'
#' @format list
#'
"SecondaryTrails"

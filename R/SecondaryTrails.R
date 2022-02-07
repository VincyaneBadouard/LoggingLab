#' Secondary trails of Paracou plot 6
#'
#' A list with :
#'
#' - inventory: Updated inventory
#'
#' - SmoothedTrails: Smoothed main and secondary trails polygons
#' (sfc_MULTIPOLYGON)
#'
#' - TrailsDensity: Second trails density (in m/ha)
#'
#' - TrailsIdentity: information on sections of the trails (matrix) with:
#'     - LineID:
#'     - LoggedTrees: idTree of trees reached by the trails
#'     - TypeExpl: type of winching
#'
#' - MainTrailsAccess : Random access point of maintrail for each PU
#' (sfc_MULTIPOLYGON)
#'
#' - RawSecondTrails : non-smoothed secondary trails (SpatialLines)
#'
#' - CostRasterAgg: A cost Raster (RasterLayer)
#'
#' @format list
#'
"SecondaryTrails"

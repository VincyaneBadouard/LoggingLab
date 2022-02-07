#' Secondary trails of Paracou plot 6
#'
#' A list with :
#'
#' - RawSecondTrails : non-smoothed secondary trails
#'
#' - TrailsIdentity: information on sections of the trails with:
#'     - LineID:
#'     - LoggedTrees: idTree of trees reached by the trails
#'     - TypeExpl: type of winching
#'
#' - SmoothedTrails: Smoothed main and secondary trails polygons
#' (sfc_MULTIPOLYGON)
#'
#' - MainTrailsAccess : Random access point of maintrail for each PU
#' (sfc_MULTIPOLYGON)
#'
#' - TrailsDensity: Second trails density (in m/ha)
#'
#' - inventory: Updated inventory
#'
#' - CostRasterAgg: A cost Raster (RasterLayer)
#'
#' @format list
#'
"SecondaryTrails"
